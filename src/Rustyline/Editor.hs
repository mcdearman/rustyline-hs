{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

-- | Port of @rustyline::Editor@ — the stateful line editor that ties together
-- the 'Config', the 'Helper' (completer\/hinter\/highlighter\/validator) and
-- the 'History', and drives the terminal.
--
-- rustyline uses methods on a @&mut Editor@; the Haskell analogue is a value
-- holding 'IORef's, with module-level functions standing in for the methods.
module Rustyline.Editor
  ( Editor,
    DefaultEditor,

    -- * Construction
    new,
    withConfig,
    newDefaultEditor,

    -- * Reading input
    readline,
    readlineWithInitial,

    -- * Helper
    setHelper,
    helper,

    -- * History
    addHistoryEntry,
    history,
    clearHistory,
    saveHistory,
    loadHistory,
    appendHistory,

    -- * Key bindings
    bindSequence,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (when)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Rustyline.Command
import Rustyline.Completion (Candidate (..), Completer (..))
import Rustyline.Config
import Rustyline.Context (Context (..))
import Rustyline.Error
import Rustyline.Helper (Helper)
import Rustyline.Highlight (Highlighter (..))
import Rustyline.Hint (Hint (..), Hinter (..))
import Rustyline.History (History)
import qualified Rustyline.History as Hist
import Rustyline.KeyEvent
import Rustyline.LineBuffer
import Rustyline.Tty
import Rustyline.Validate
import System.IO (hFlush, hSetEncoding, putStr, stdin, stdout, utf8)
import System.IO.Error (isEOFError)

-- | The editor. Parameterised by the 'Helper' type @h@ (use @()@ for none).
data Editor h = Editor
  { edConfig :: Config,
    edHelperRef :: IORef (Maybe h),
    edHistoryRef :: IORef History,
    edBindingsRef :: IORef (Map KeyEvent Cmd)
  }

-- | @Editor ()@ — no helper. Mirrors rustyline's @DefaultEditor@.
type DefaultEditor = Editor ()

-- | Create an editor with 'defaultConfig' (rustyline @Editor::new@).
new :: IO (Editor h)
new = withConfig defaultConfig

-- | Create an editor with a given 'Config' (rustyline @Editor::with_config@).
withConfig :: Config -> IO (Editor h)
withConfig cfg =
  Editor cfg
    <$> newIORef Nothing
    <*> newIORef (Hist.emptyHistory cfg)
    <*> newIORef Map.empty

-- | Convenience: a 'DefaultEditor' with default config.
newDefaultEditor :: IO DefaultEditor
newDefaultEditor = new

-- | Attach (or remove) the helper (rustyline @set_helper@).
setHelper :: Editor h -> Maybe h -> IO ()
setHelper ed = writeIORef (edHelperRef ed)

-- | The current helper, if any (rustyline @helper@).
helper :: Editor h -> IO (Maybe h)
helper = readIORef . edHelperRef

-- | Add a line to history; returns whether it was actually added
-- (rustyline @add_history_entry@).
addHistoryEntry :: Editor h -> String -> IO Bool
addHistoryEntry ed line = do
  h <- readIORef (edHistoryRef ed)
  let (h', added) = Hist.addEntry (edConfig ed) line h
  writeIORef (edHistoryRef ed) h'
  pure added

-- | All history entries, oldest first (rustyline @history@).
history :: Editor h -> IO [String]
history ed = Hist.toList <$> readIORef (edHistoryRef ed)

-- | Clear history (rustyline @clear_history@).
clearHistory :: Editor h -> IO ()
clearHistory ed = modifyIORef' (edHistoryRef ed) Hist.clearHistory

-- | Save history to a file (rustyline @save_history@).
saveHistory :: Editor h -> FilePath -> IO ()
saveHistory ed path = readIORef (edHistoryRef ed) >>= Hist.saveTo path

-- | Load history from a file, replacing the current entries
-- (rustyline @load_history@).
loadHistory :: Editor h -> FilePath -> IO ()
loadHistory ed path = Hist.loadFrom (edConfig ed) path >>= writeIORef (edHistoryRef ed)

-- | Append current history to a file (rustyline @append_history@).
appendHistory :: Editor h -> FilePath -> IO ()
appendHistory ed path = readIORef (edHistoryRef ed) >>= Hist.appendTo path

-- | Bind a key to a command, overriding the default map
-- (rustyline @bind_sequence@).
bindSequence :: Editor h -> KeyEvent -> Cmd -> IO ()
bindSequence ed k cmd = modifyIORef' (edBindingsRef ed) (Map.insert k cmd)

--------------------------------------------------------------------------------
-- Reading
--------------------------------------------------------------------------------

-- | Read a line, showing @prompt@ (rustyline @readline@).
readline :: (Helper h) => Editor h -> String -> IO (Either ReadlineError String)
readline ed prompt = readlineWithInitial ed prompt ("", "")

-- | Read a line pre-filled with @(left, right)@ around the cursor
-- (rustyline @readline_with_initial@).
readlineWithInitial ::
  (Helper h) =>
  Editor h ->
  String ->
  (String, String) ->
  IO (Either ReadlineError String)
readlineWithInitial ed prompt (lhs, rhs) = do
  -- rustyline operates on UTF-8 throughout; make sure our handles agree even
  -- under a non-UTF-8 locale (e.g. a pipe in a C-locale shell).
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  tty <- isInputTty
  if not tty
    then fallback ed prompt
    else withRawMode $ do
      let buf0 = setLine (lhs ++ rhs) (length lhs)
          st0 = EState buf0 Nothing "" Nothing 0
      r <- loop ed prompt st0
      finish ed r

-- accept-side bookkeeping (auto add to history)
finish :: Editor h -> Either ReadlineError String -> IO (Either ReadlineError String)
finish ed r@(Right line) = do
  when (autoAddHistory (edConfig ed)) (() <$ addHistoryEntry ed line)
  pure r
finish _ r = pure r

-- non-interactive fallback so the API still works under a pipe/redirect
fallback :: Editor h -> String -> IO (Either ReadlineError String)
fallback ed prompt = do
  putStr prompt
  hFlush stdout
  e <- try getLine :: IO (Either IOException String)
  case e of
    Left _ -> pure (Left Eof)
    Right l -> finish ed (Right l)

--------------------------------------------------------------------------------
-- Edit loop
--------------------------------------------------------------------------------

-- transient state while editing one line
data EState = EState
  { sBuf :: !LineBuffer,
    -- | Nothing = fresh line; Just i = viewing entry i.
    sHistIdx :: !(Maybe Int),
    -- | Saved fresh line while browsing history.
    sStash :: !String,
    sCompl :: !(Maybe Compl),
    -- | Cursor row offset from block top, last frame.
    sRow :: !Int
  }

-- circular-completion state
data Compl = Compl
  { cStart :: !Int,
    cCands :: ![String],
    cIdx :: !Int,
    cBaseLine :: !String,
    cBasePos :: !Int
  }

clearCompl :: EState -> EState
clearCompl st = st {sCompl = Nothing}

loop :: (Helper h) => Editor h -> String -> EState -> IO (Either ReadlineError String)
loop ed prompt st0 = do
  row <- render ed prompt st0
  let st = st0 {sRow = row} -- remember where the cursor ended up
  ek <- try readKey :: IO (Either IOException KeyEvent)
  case ek of
    Left e
      | isEOFError e ->
          if bufLen (sBuf st) == 0
            then finishBelow st >> pure (Left Eof)
            else loop ed prompt st
      | otherwise -> finishBelow st >> pure (Left (Io e))
    Right k -> do
      userBinds <- readIORef (edBindingsRef ed)
      let cmd = resolve (Map.union userBinds emacsKeymap) k
      dispatch ed prompt st cmd

-- | Move the cursor below the current block and onto a fresh line.
finishBelow :: EState -> IO ()
finishBelow st =
  moveBelowBlock (sRow st) (length (filter (== '\n') (asString (sBuf st))))

resolve :: Map KeyEvent Cmd -> KeyEvent -> Cmd
resolve binds k =
  case Map.lookup k binds of
    Just cmd -> cmd
    Nothing -> case k of
      KeyEvent (Char c) m | not (modCtrl m) && not (modAlt m) -> SelfInsert c
      _ -> Noop

dispatch ::
  (Helper h) =>
  Editor h ->
  String ->
  EState ->
  Cmd ->
  IO (Either ReadlineError String)
dispatch ed prompt st cmd =
  let cont s = loop ed prompt s
      withBuf f = cont (clearCompl st) {sBuf = f (sBuf st)}
   in case cmd of
        SelfInsert c -> cont (clearCompl st) {sBuf = insertChar c (sBuf st)}
        Noop -> cont (clearCompl st)
        Move m -> withBuf (applyMove m)
        Kill m -> withBuf (applyKill m)
        BackwardDeleteChar -> withBuf backspace
        DeleteChar -> withBuf deleteChar
        TransposeChars -> withBuf transpose
        ClearScreen -> clearScreen >> cont st {sRow = 0}
        PreviousHistory -> histPrev ed prompt st
        NextHistory -> histNext ed prompt st
        Complete -> doComplete ed prompt st
        Interrupt -> finishBelow st >> pure (Left Interrupted)
        EndOfFile
          | bufLen (sBuf st) == 0 -> finishBelow st >> pure (Left Eof)
          | otherwise -> withBuf deleteChar
        AcceptLine -> doAccept ed prompt st

applyMove :: Movement -> LineBuffer -> LineBuffer
applyMove m = case m of
  MoveChar Backward -> moveLeft
  MoveChar Forward -> moveRight
  MoveWord Backward -> moveBackwardWord
  MoveWord Forward -> moveForwardWord
  MoveLineStart -> moveHome
  MoveLineEnd -> moveEnd

applyKill :: Movement -> LineBuffer -> LineBuffer
applyKill m = case m of
  MoveLineEnd -> killToEnd
  MoveLineStart -> killToHome
  MoveWord Backward -> killWordBackward
  MoveWord Forward -> killWordForward
  MoveChar Backward -> backspace
  MoveChar Forward -> deleteChar

--------------------------------------------------------------------------------
-- History navigation
--------------------------------------------------------------------------------

histPrev :: (Helper h) => Editor h -> String -> EState -> IO (Either ReadlineError String)
histPrev ed prompt st = do
  entries <- history ed
  let n = length entries
  if n == 0
    then beep >> loop ed prompt (clearCompl st)
    else do
      let cur = maybe n id (sHistIdx st)
          newIdx = max 0 (cur - 1)
          stash = case sHistIdx st of Nothing -> asString (sBuf st); Just _ -> sStash st
          entry = entries !! newIdx
      loop
        ed
        prompt
        st
          { sBuf = fromString entry,
            sHistIdx = Just newIdx,
            sStash = stash,
            sCompl = Nothing
          }

histNext :: (Helper h) => Editor h -> String -> EState -> IO (Either ReadlineError String)
histNext ed prompt st = do
  entries <- history ed
  let n = length entries
  case sHistIdx st of
    Nothing -> beep >> loop ed prompt (clearCompl st)
    Just i
      | i + 1 >= n ->
          loop
            ed
            prompt
            st
              { sBuf = fromString (sStash st),
                sHistIdx = Nothing,
                sCompl = Nothing
              }
      | otherwise ->
          loop
            ed
            prompt
            st
              { sBuf = fromString (entries !! (i + 1)),
                sHistIdx = Just (i + 1),
                sCompl = Nothing
              }

--------------------------------------------------------------------------------
-- Accept / validate
--------------------------------------------------------------------------------

doAccept :: (Helper h) => Editor h -> String -> EState -> IO (Either ReadlineError String)
doAccept ed prompt st = do
  mh <- helper ed
  let line = asString (sBuf st)
      pos = lbPos (sBuf st)
  res <- case mh of
    Nothing -> pure (Valid Nothing)
    Just h -> validate h (ValidationContext line pos)
  case res of
    Incomplete -> loop ed prompt st {sBuf = insertChar '\n' (sBuf st), sCompl = Nothing}
    Invalid m -> do
      finishBelow st
      putFlush (maybe "" id m ++ "\r\n")
      loop ed prompt (clearCompl st) {sRow = 0}
    Valid _ -> finishBelow st >> pure (Right line)

--------------------------------------------------------------------------------
-- Completion
--------------------------------------------------------------------------------

doComplete :: (Helper h) => Editor h -> String -> EState -> IO (Either ReadlineError String)
doComplete ed prompt st = do
  mh <- helper ed
  case mh of
    Nothing -> beep >> loop ed prompt (clearCompl st)
    Just h ->
      case sCompl st of
        Just c | not (null (cCands c)) -> do
          let i' = (cIdx c + 1) `mod` length (cCands c)
              cand = cCands c !! i'
              line = take (cStart c) (cBaseLine c) ++ cand ++ drop (cBasePos c) (cBaseLine c)
              pos = cStart c + length cand
          loop ed prompt st {sBuf = setLine line pos, sCompl = Just c {cIdx = i'}}
        _ -> do
          hist <- history ed
          let line = asString (sBuf st)
              pos = lbPos (sBuf st)
          (start, cands) <- complete h line pos (Context hist pos)
          let reps = map replacement cands
          case reps of
            [] -> beep >> loop ed prompt (clearCompl st)
            [r] ->
              let line' = take start line ++ r ++ drop pos line
               in loop
                    ed
                    prompt
                    st
                      { sBuf = setLine line' (start + length r),
                        sCompl = Nothing
                      }
            _ -> case completionType (edConfig ed) of
              Circular ->
                let cand = head reps
                    line' = take start line ++ cand ++ drop pos line
                 in loop
                      ed
                      prompt
                      st
                        { sBuf = setLine line' (start + length cand),
                          sCompl = Just (Compl start reps 0 line pos)
                        }
              List -> do
                finishBelow st
                putFlush (unwords (map display cands) ++ "\r\n")
                loop ed prompt (clearCompl st) {sRow = 0}

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

render :: (Helper h) => Editor h -> String -> EState -> IO Int
render ed prompt st = do
  mh <- helper ed
  hist <- history ed
  let buf = sBuf st
      line = asString buf
      pos = lbPos buf
      styledPrompt = maybe prompt (\h -> highlightPrompt h prompt True) mh
      plen = visibleLength styledPrompt
      styledLine = case mh of
        Just h | highlightChar h line pos -> highlight h line pos
        _ -> line
  hintStr <- case mh of
    Nothing -> pure ""
    Just h -> do
      mhint <- hint h line pos (Context hist pos)
      pure $ maybe "" (highlightHint h . hintDisplay) mhint
  renderBlock
    Frame
      { fPrompt = styledPrompt,
        fPromptLen = plen,
        fStyled = styledLine,
        fRaw = line,
        fHint = hintStr,
        fCursor = pos,
        fOldRow = sRow st
      }

--------------------------------------------------------------------------------
-- Default (Emacs) key map
--------------------------------------------------------------------------------

emacsKeymap :: Map KeyEvent Cmd
emacsKeymap =
  Map.fromList
    [ (key Enter, AcceptLine),
      (key Backspace, BackwardDeleteChar),
      (key Delete, DeleteChar),
      (key LeftArrow, Move (MoveChar Backward)),
      (key RightArrow, Move (MoveChar Forward)),
      (key UpArrow, PreviousHistory),
      (key DownArrow, NextHistory),
      (key Home, Move MoveLineStart),
      (key End, Move MoveLineEnd),
      (key Tab, Complete),
      (ctrlKey 'a', Move MoveLineStart),
      (ctrlKey 'e', Move MoveLineEnd),
      (ctrlKey 'b', Move (MoveChar Backward)),
      (ctrlKey 'f', Move (MoveChar Forward)),
      (ctrlKey 'p', PreviousHistory),
      (ctrlKey 'n', NextHistory),
      (ctrlKey 'k', Kill MoveLineEnd),
      (ctrlKey 'u', Kill MoveLineStart),
      (ctrlKey 'w', Kill (MoveWord Backward)),
      (ctrlKey 'd', EndOfFile),
      (ctrlKey 'c', Interrupt),
      (ctrlKey 'l', ClearScreen),
      (ctrlKey 't', TransposeChars),
      (altKey 'b', Move (MoveWord Backward)),
      (altKey 'f', Move (MoveWord Forward)),
      (altKey 'd', Kill (MoveWord Forward))
    ]