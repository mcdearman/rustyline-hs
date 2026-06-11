{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

-- | Port of rustyline's terminal layer (@rustyline::tty@) for Unix.
--
-- This handles raw-mode setup, decoding bytes\/escape sequences into
-- 'KeyEvent's, and a multi-line refresh (the buffer may contain @\\n@). It is
-- intentionally a focused subset: Unix only, Emacs-style rendering, and it
-- assumes no automatic terminal-width line wrapping (one physical row per
-- logical row).
module Rustyline.Tty
  ( withRawMode
  , isInputTty
  , readKey
  , Frame (..)
  , renderBlock
  , moveBelowBlock
  , putFlush
  , beep
  , clearScreen
  , visibleLength
  ) where

import Rustyline.KeyEvent

import System.IO
import System.Posix.Terminal
import System.Posix.IO (stdInput)
import Control.Exception (bracket_, catch, SomeException)
import Data.Char (ord, chr)

-- | Is standard input an interactive terminal?
isInputTty :: IO Bool
isInputTty = queryTerminal stdInput

-- | Run an action with the terminal in raw mode, restoring the previous
-- attributes afterwards. On a non-tty stdin this is a no-op wrapper.
withRawMode :: IO a -> IO a
withRawMode action = do
  tty <- isInputTty
  if not tty
    then action
    else do
      old <- getTerminalAttributes stdInput
      let new = makeRaw old
      bracket_
        (setTerminalAttributes stdInput new WhenFlushed)
        (setTerminalAttributes stdInput old WhenFlushed)
        (do hSetBuffering stdin NoBuffering
            hSetBuffering stdout NoBuffering
            hSetEncoding  stdin utf8
            hSetEncoding  stdout utf8
            action)

makeRaw :: TerminalAttributes -> TerminalAttributes
makeRaw attrs =
  flip withTime 0 $
  flip withMinInput 1 $
  foldl withoutMode attrs
    [ EnableEcho        -- ECHO
    , ProcessInput      -- ICANON
    , KeyboardInterrupts -- ISIG  (so Ctrl-C/Ctrl-D arrive as bytes)
    , StartStopOutput   -- IXON
    , ExtendedFunctions -- IEXTEN
    ]

-- | Read and decode a single key press from standard input. May throw at
-- end of input (callers treat that as EOF).
readKey :: IO KeyEvent
readKey = do
  c <- hGetChar stdin
  let n = ord c
  case c of
    '\ESC' -> readEsc
    '\r'   -> pure (key Enter)
    '\n'   -> pure (key Enter)
    '\t'   -> pure (key Tab)
    '\DEL' -> pure (key Backspace)   -- 127
    '\b'   -> pure (key Backspace)   -- 8
    _ | n >= 1 && n <= 26 -> pure (ctrlKey (chr (n + 96)))
      | otherwise         -> pure (key (Char c))

-- after ESC: distinguish bare Esc, CSI/SS3 sequences, and Alt+key
readEsc :: IO KeyEvent
readEsc = do
  more <- hReady stdin `catch` \(_ :: SomeException) -> pure False
  if not more
    then pure (key Esc)
    else do
      c <- hGetChar stdin
      case c of
        '[' -> readCSI
        'O' -> readSS3
        _   -> pure (KeyEvent (Char c) alt)   -- Alt + key

readCSI :: IO KeyEvent
readCSI = do
  c <- hGetChar stdin
  case c of
    'A' -> pure (key UpArrow)
    'B' -> pure (key DownArrow)
    'C' -> pure (key RightArrow)
    'D' -> pure (key LeftArrow)
    'H' -> pure (key Home)
    'F' -> pure (key End)
    d | d `elem` ['0'..'9'] -> readTilde [d]
      | otherwise           -> pure (key Null)

readTilde :: String -> IO KeyEvent
readTilde ds = do
  c <- hGetChar stdin
  case c of
    '~' -> pure (tildeKey ds)
    d | d `elem` ['0'..'9'] -> readTilde (ds ++ [d])
      | otherwise           -> pure (key Null)

tildeKey :: String -> KeyEvent
tildeKey ds = case ds of
  "1" -> key Home
  "7" -> key Home
  "4" -> key End
  "8" -> key End
  "3" -> key Delete
  "5" -> key PageUp
  "6" -> key PageDown
  _   -> key Null

readSS3 :: IO KeyEvent
readSS3 = do
  c <- hGetChar stdin
  pure $ case c of
    'A' -> key UpArrow
    'B' -> key DownArrow
    'C' -> key RightArrow
    'D' -> key LeftArrow
    'H' -> key Home
    'F' -> key End
    _   -> key Null

-- | Everything needed to paint one line.
-- | Everything needed to repaint one editing frame. The line text may span
-- several physical rows via embedded @\\n@ characters.
data Frame = Frame
  { fPrompt    :: String  -- ^ Prompt text (already styled).
  , fPromptLen :: Int     -- ^ Visible width of the prompt.
  , fStyled    :: String  -- ^ Line text to print (already styled; keeps @\\n@).
  , fRaw       :: String  -- ^ Unstyled line text, used for cursor layout math.
  , fHint      :: String  -- ^ Trailing hint (already styled, no @\\n@), or "".
  , fCursor    :: Int     -- ^ Cursor char index into 'fRaw'.
  , fOldRow    :: Int     -- ^ Cursor's row offset from the block top on the
                          --   previous frame (0 on the first frame).
  }

-- | Repaint the whole multi-line block in place and position the cursor by row
-- and column. Returns the cursor's new row offset from the top of the block,
-- which must be threaded back in as 'fOldRow' on the next call.
--
-- Strategy (linenoise multi-line refresh, minus width-wrap handling): move up
-- to the top of the previous block, clear to the end of the screen, redraw
-- @prompt + line + hint@ (turning each @\\n@ into @\\r\\n@ so every row starts
-- at column 0), then move the cursor up\/left to its logical position.
renderBlock :: Frame -> IO Int
renderBlock f = do
  let raw    = fRaw f
      pos    = max 0 (min (length raw) (fCursor f))
      before = take pos raw
      crow   = countNL before
      ccol   = (if crow == 0 then fPromptLen f else 0)
                 + length (afterLastNL before)
      endRow = countNL raw
      up0      = if fOldRow f > 0 then csi (fOldRow f) 'A' else ""
      clearTop = "\r\ESC[J"                       -- col 0, clear to end of screen
      body     = fPrompt f ++ nlToCRLF (fStyled f) ++ fHint f
      backUp   = if endRow > crow then csi (endRow - crow) 'A' else ""
      toCol    = "\r" ++ (if ccol > 0 then csi ccol 'C' else "")
  putFlush (up0 ++ clearTop ++ body ++ backUp ++ toCol)
  pure crow

-- | Move the cursor below the whole block (from row @cur@ of a block whose last
-- row is @endRow@) and start a fresh line. Used when finishing a line so the
-- program's subsequent output appears after the input.
moveBelowBlock :: Int -> Int -> IO ()
moveBelowBlock cur endRow =
  putFlush ((if endRow > cur then csi (endRow - cur) 'B' else "") ++ "\r\n")

csi :: Int -> Char -> String
csi n c = "\ESC[" ++ show n ++ [c]

countNL :: String -> Int
countNL = length . filter (== '\n')

afterLastNL :: String -> String
afterLastNL = reverse . takeWhile (/= '\n') . reverse

nlToCRLF :: String -> String
nlToCRLF = concatMap (\ch -> if ch == '\n' then "\r\n" else [ch])

-- | Write a string to stdout and flush immediately.
putFlush :: String -> IO ()
putFlush s = putStr s >> hFlush stdout

-- | Ring the bell.
beep :: IO ()
beep = putFlush "\a"

-- | Clear the whole screen and home the cursor.
clearScreen :: IO ()
clearScreen = putFlush "\ESC[H\ESC[2J"

-- | Visible width of a string, ignoring ANSI SGR escape sequences.
visibleLength :: String -> Int
visibleLength = go 0
  where
    go !acc [] = acc
    go !acc ('\ESC' : '[' : rest) = go acc (dropSGR rest)
    go !acc (_ : rest) = go (acc + 1) rest
    dropSGR ('m' : rest) = rest
    dropSGR (_   : rest) = dropSGR rest
    dropSGR []           = []