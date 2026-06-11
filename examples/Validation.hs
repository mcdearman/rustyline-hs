-- | Multi-line input gated by bracket balance, using the library's
-- 'MatchingBracketValidator'. Pressing Enter with unbalanced @()[]{}@ keeps
-- editing (a newline is inserted) until the brackets match. Mirrors
-- rustyline's @examples/input_validation.rs@.
module Main (main) where

import Rustyline

data ValHelper = ValHelper

instance Validator ValHelper where
  validate _ = validate MatchingBracketValidator

instance Completer   ValHelper  -- default: no completion
instance Hinter      ValHelper  -- default: no hints
instance Highlighter ValHelper  -- default: no highlighting

main :: IO ()
main = do
  ed <- new :: IO (Editor ValHelper)
  setHelper ed (Just ValHelper)
  putStrLn "Enter an expression; unbalanced brackets keep editing. Ctrl-D to quit."
  loop ed
  where
    loop ed = do
      r <- readline ed "expr> "
      case r of
        Right line       -> putStrLn ("Got:\n" ++ line) >> loop ed
        Left Interrupted -> putStrLn "CTRL-C"
        Left Eof         -> putStrLn "CTRL-D"
        Left err         -> putStrLn ("Error: " ++ show err)
