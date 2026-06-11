{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Port of @rustyline::completion@.
--
-- rustyline's @Completer@ trait has an associated @Candidate@ type; we mirror
-- that with a type family 'CandidateOf'. A completion returns the start
-- position to replace from and a list of candidates, exactly like
-- @fn complete(&self, line, pos, ctx) -> Result<(usize, Vec<Self::Candidate>)>@.
module Rustyline.Completion
  ( Candidate (..),
    Pair (..),
    Completer (..),
    FilenameCompleter (..),
  )
where

import Control.Exception (SomeException, catch)
import Data.List (isPrefixOf)
import Rustyline.Context (Context)
import System.Directory (doesDirectoryExist, getDirectoryContents)

-- | A completion candidate (rustyline's @Candidate@ trait): a value with a
-- display string and the text that actually replaces the line.
class Candidate a where
  -- | Text shown in the candidate list.
  display :: a -> String

  -- | Text substituted into the line when chosen.
  replacement :: a -> String

-- | The simplest candidate: a bare 'String' is both display and replacement
-- (rustyline implements @Candidate@ for @String@).
instance Candidate [Char] where
  display = id
  replacement = id

-- | A display\/replacement pair (rustyline's @completion::Pair@).
data Pair = Pair
  { pairDisplay :: String,
    pairReplacement :: String
  }
  deriving (Eq, Show)

instance Candidate Pair where
  display = pairDisplay
  replacement = pairReplacement

-- | Provides completion candidates. Equivalent to rustyline's @Completer@.
--
-- The default 'complete' offers nothing, so a no-op completer is just
-- @instance Completer MyType@.
class (Candidate (CandidateOf c)) => Completer c where
  -- | The candidate type (rustyline's associated @type Candidate@).
  type CandidateOf c

  type CandidateOf c = Pair

  -- | @complete c line pos ctx@ returns @(start, candidates)@ where @start@
  -- is the char index from which the candidates replace the line.
  complete :: c -> String -> Int -> Context -> IO (Int, [CandidateOf c])
  default complete :: c -> String -> Int -> Context -> IO (Int, [CandidateOf c])
  complete _ _ pos _ = pure (pos, [])

-- | The unit type is a completer that offers nothing — mirrors rustyline's
-- use of @()@ as the "no helper" type.
instance Completer () where
  type CandidateOf () = Pair

-- | A built-in completer for file paths in the current directory
-- (rustyline's @FilenameCompleter@). Completes the word under the cursor.
data FilenameCompleter = FilenameCompleter

instance Completer FilenameCompleter where
  type CandidateOf FilenameCompleter = Pair
  complete _ line pos _ = do
    let (before, _after) = splitAt pos line
        word = reverse (takeWhile (not . isBreak) (reverse before))
        start = pos - length word
    names <- listDir `catch` \(_ :: SomeException) -> pure []
    let matches = [Pair n n | n <- names, word `isPrefixOf` n]
    pure (start, matches)
    where
      isBreak c = c == ' ' || c == '\t'
      listDir = do
        ok <- doesDirectoryExist "."
        if ok
          then filter (`notElem` [".", ".."]) <$> getDirectoryContents "."
          else pure []
