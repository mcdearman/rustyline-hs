{-# LANGUAGE ScopedTypeVariables #-}

-- | Haskell translation of the Idyll REPL, using @rustyline-hs@.
--
-- This is the analogue of the Rust @repl.rs@: the @run@ function is the
-- entry point (the Rust @pub fn run()@). A @main = run@ is included so the
-- module is runnable on its own; drop it (and rename the module to @Repl@)
-- if you wire @run@ into your own @Main@.
module Main (main, run) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, isSuffixOf)
import Rustyline
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr)

--------------------------------------------------------------------------------
-- Placeholder for your own @crate::pipeline@ module.
-- Translate these to match your real Pipeline; they're stubbed here so the
-- REPL compiles and runs end to end.
--------------------------------------------------------------------------------

data InputMode = Interactive | NonInteractive

newtype Pipeline = Pipeline String

newPipeline :: String -> InputMode -> Pipeline
newPipeline src _mode = Pipeline src

runPipeline :: Pipeline -> IO ()
runPipeline (Pipeline src) = putStrLn ("[pipeline] " ++ src) -- replace with real work

--------------------------------------------------------------------------------
-- The helper.
--
-- In Rust, @#[derive(Completer, Helper, Highlighter, Hinter)]@ gives no-op
-- implementations and only @Validator@ is written by hand. Here that's the
-- three empty instances (which fall back to the class defaults) plus the
-- custom 'Validator'. 'Helper' itself follows from the blanket instance.
--------------------------------------------------------------------------------

data TermValidator = TermValidator

instance Completer TermValidator

instance Hinter TermValidator

instance Highlighter TermValidator

instance Validator TermValidator where
  validate _ ctx
    | "\n" `isSuffixOf` vcInput ctx = pure (Valid Nothing)
    | otherwise = pure Incomplete

--------------------------------------------------------------------------------
-- The REPL.
--------------------------------------------------------------------------------

historyFile :: FilePath
historyFile = ".repl_history"

run :: IO ()
run = do
  ed <- new :: IO (Editor TermValidator)
  setHelper ed (Just TermValidator)

  -- rustyline's load_history returns a Result that errors when the file is
  -- missing; the port's loadHistory treats a missing file as empty history,
  -- so we check existence ourselves to keep the "No previous history." path.
  exists <- doesFileExist historyFile
  if exists
    then loadHistory ed historyFile
    else hPutStrLn stderr "No previous history."

  putStrLn "Welcome to Idyll!"
  loop ed
  saveHistory ed historyFile
  where
    loop :: Editor TermValidator -> IO ()
    loop ed = do
      readline ed "> " >>= \r -> case r of
        Right line ->
          -- Match on the trimmed line, but keep the original for history
          -- and the pipeline (exactly as the Rust does).
          case trim line of
            ":q" -> pure () -- break
            ":quit" -> pure () -- break
            "clear" -> clearHistory ed >> loop ed -- continue
            _ -> do
              _ <- addHistoryEntry ed line
              runPipeline (newPipeline line Interactive)
              loop ed
        Left Interrupted -> putStrLn "CTRL-C" -- break
        Left Eof -> putStrLn "CTRL-D" -- break
        Left err -> putStrLn ("Error: " ++ show err) -- break

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

main :: IO ()
main = run
