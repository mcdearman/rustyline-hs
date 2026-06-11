{-# LANGUAGE TypeFamilies #-}

-- | Example program for @rustyline-hs@, mirroring the spirit of rustyline's
-- @examples/example.rs@: a custom helper that combines a completer, a
-- history-based hinter, a bracket-matching highlighter and a bracket
-- validator, driven by a 'readline' loop that records history.
module Main (main) where

import Control.Monad (unless)
import Data.List (isPrefixOf, nub)
import Rustyline

-- | Our helper bundles the four behaviours. Because of the blanket
-- @instance (Completer h, Hinter h, Highlighter h, Validator h) => Helper h@,
-- giving the four instances below is enough to make @MyHelper@ a 'Helper'.
data MyHelper = MyHelper
  { keywords :: [String],
    -- | reused from the library
    hinter :: HistoryHinter
  }

-- | Complete the word under the cursor against a fixed keyword list,
-- returning display\/replacement 'Pair's (the default candidate type).
instance Completer MyHelper where
  complete h line pos _ctx = do
    let before = take pos line
        wordStart = length before - length (reverse (takeWhile (/= ' ') (reverse before)))
        word = drop wordStart before
        matches = filter (word `isPrefixOf`) (keywords h)
        candidates = [Pair w (w ++ " ") | w <- matches]
    pure (wordStart, candidates)

-- | Delegate hinting to the library's 'HistoryHinter'.
instance Hinter MyHelper where
  type HintOf MyHelper = String
  hint h line pos ctx = hint (hinter h) line pos ctx

-- | Bold matching brackets near the cursor (library highlighter behaviour).
instance Highlighter MyHelper where
  highlight _ line pos = highlight MatchingBracketHighlighter line pos
  highlightChar _ line pos = highlightChar MatchingBracketHighlighter line pos

-- | Refuse to accept a line with unbalanced brackets.
instance Validator MyHelper where
  validate _ = validate MatchingBracketValidator

main :: IO ()
main = do
  ed <- new :: IO (Editor MyHelper)
  setHelper ed (Just (MyHelper kws HistoryHinter))
  putStrLn "rustyline-hs example. Type 'help', use Tab to complete, Ctrl-C / Ctrl-D to quit."
  loop ed
  where
    kws = nub ["help", "hello", "history", "highlight", "exit", "editor", "complete", "completion"]

    loop ed = do
      r <- readline ed "\x1b[32m\xBB\x1b[0m "
      case r of
        Left Eof -> putStrLn "<EOF>"
        Left Interrupted -> putStrLn "<Ctrl-C>"
        Left err -> putStrLn ("error: " ++ show err)
        Right line -> do
          unless (null line) $ do
            _ <- addHistoryEntry ed line
            pure ()
          case line of
            "exit" -> putStrLn "bye"
            _ -> do
              putStrLn ("Line: " ++ line)
              loop ed
