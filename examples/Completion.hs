{-# LANGUAGE TypeFamilies #-}

-- | Tab-completion of filenames, using the library's 'FilenameCompleter'.
--
-- A 'Helper' must satisfy all four classes, so we wrap the built-in completer
-- in our own type and take the defaults for hinting, highlighting and
-- validation. Mirrors rustyline's @examples/completion.rs@.
module Main (main) where

import Rustyline

-- | Our helper: filename completion, nothing else.
data FileHelper = FileHelper

instance Completer FileHelper where
  -- delegate to the library's FilenameCompleter (its candidate type is Pair,
  -- which is also our default CandidateOf, so the types line up)
  complete _ = complete FilenameCompleter

instance Hinter FileHelper -- default: no hints

instance Highlighter FileHelper -- default: no highlighting

instance Validator FileHelper -- default: always Valid

main :: IO ()
main = do
  ed <- new :: IO (Editor FileHelper)
  setHelper ed (Just FileHelper)
  putStrLn "Type part of a filename and press Tab. Ctrl-D to quit."
  loop ed
  where
    loop ed = do
      r <- readline ed "path> "
      case r of
        Right line -> putStrLn ("You chose: " ++ line) >> loop ed
        Left Interrupted -> putStrLn "CTRL-C"
        Left Eof -> putStrLn "CTRL-D"
        Left err -> putStrLn ("Error: " ++ show err)
