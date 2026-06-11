-- | Pure tests for the parts of @rustyline-hs@ that don't need a terminal:
-- the 'LineBuffer' editing core and the 'History' store. Uses a tiny
-- assertion harness so it depends only on GHC boot libraries.
module Main (main) where

import Rustyline.LineBuffer
import qualified Rustyline.History as H
import Rustyline.Config (defaultConfig)

import Control.Monad (forM_, unless)
import Data.IORef
import System.Exit (exitFailure)

main :: IO ()
main = do
  failed <- newIORef (0 :: Int)
  let check :: (Eq a, Show a) => String -> a -> a -> IO ()
      check name got want
        | got == want = putStrLn ("ok   - " ++ name)
        | otherwise   = do
            modifyIORef' failed (+1)
            putStrLn ("FAIL - " ++ name ++ "\n   expected: " ++ show want
                                          ++ "\n   but got:  " ++ show got)

  -- LineBuffer: insertion
  check "insertChar advances cursor"
    (let b = insertChar 'c' (insertChar 'b' (insertChar 'a' empty))
     in (asString b, lbPos b)) ("abc", 3)

  check "insertChar in the middle"
    (let b = insertChar 'X' (moveLeft (fromString "ab"))
     in (asString b, lbPos b)) ("aXb", 2)

  check "insertStr"
    (let b = insertStr "hello" empty in (asString b, lbPos b)) ("hello", 5)

  -- LineBuffer: deletion
  check "backspace at end"
    (asString (backspace (fromString "abc"))) "ab"
  check "backspace at start is a no-op"
    (let b = backspace (moveHome (fromString "abc")) in (asString b, lbPos b)) ("abc", 0)
  check "deleteChar under cursor"
    (asString (deleteChar (moveHome (fromString "abc")))) "bc"
  check "deleteChar at end is a no-op"
    (asString (deleteChar (fromString "abc"))) "abc"

  -- LineBuffer: transpose (Ctrl-T)
  check "transpose last two at end"
    (asString (transpose (fromString "abc"))) "acb"
  check "transpose in the middle"
    (asString (transpose (moveLeft (moveLeft (fromString "abc"))))) "bac"
  check "transpose too short is a no-op"
    (asString (transpose (fromString "a"))) "a"

  -- LineBuffer: movement
  check "moveHome / moveEnd"
    (let b = fromString "abc" in (lbPos (moveHome b), lbPos (moveEnd (moveHome b)))) (0, 3)
  check "moveLeft clamps at 0"
    (lbPos (moveLeft (moveLeft (moveHome (fromString "ab"))))) 0
  check "moveRight clamps at len"
    (lbPos (moveRight (fromString "ab"))) 2

  -- LineBuffer: word movement
  check "moveBackwardWord"
    (lbPos (moveBackwardWord (fromString "foo bar"))) 4
  check "moveBackwardWord twice"
    (lbPos (moveBackwardWord (moveBackwardWord (fromString "foo bar")))) 0
  check "moveForwardWord from home"
    (lbPos (moveForwardWord (moveHome (fromString "foo bar")))) 3

  -- LineBuffer: killing
  check "killToEnd"
    (let b = killToEnd (moveLeft (fromString "abc")) in (asString b, lbPos b)) ("ab", 2)
  check "killToHome"
    (let b = killToHome (fromString "abc") in (asString b, lbPos b)) ("", 0)
  check "killWordBackward"
    (asString (killWordBackward (fromString "foo bar"))) "foo "
  check "killWordForward"
    (asString (killWordForward (moveHome (fromString "foo bar")))) " bar"

  -- History: add, dedup, retrieve
  check "addEntry stores"
    (let (h, added) = H.addEntry defaultConfig "one" (H.emptyHistory defaultConfig)
     in (H.len h, added)) (1, True)
  check "addEntry ignores consecutive duplicate"
    (let (h1, _) = H.addEntry defaultConfig "one" (H.emptyHistory defaultConfig)
         (h2, added) = H.addEntry defaultConfig "one" h1
     in (H.len h2, added)) (1, False)
  check "getEntry by index"
    (let h = H.fromList defaultConfig ["a", "b", "c"] in H.getEntry h 1) (Just "b")
  check "toList round-trips"
    (H.toList (H.fromList defaultConfig ["a", "b"])) ["a", "b"]

  n <- readIORef failed
  putStrLn ("\n" ++ show n ++ " failure(s)")
  unless (n == 0) exitFailure
