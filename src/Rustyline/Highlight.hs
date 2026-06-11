-- | Port of @rustyline::highlight@.
--
-- A 'Highlighter' colourises the line, the prompt, hints and completion
-- candidates by inserting ANSI escape codes. All methods have identity
-- defaults, so @instance Highlighter MyType@ is a no-op highlighter, exactly
-- like rustyline's blanket default implementations.
module Rustyline.Highlight
  ( Highlighter (..)
  , MatchingBracketHighlighter (..)
    -- * ANSI helpers
  , dim
  , bold
  , withColor
  , reset
  ) where

import Rustyline.Config (CompletionType)

-- | Adds visual styling via ANSI escapes. Equivalent to rustyline's
-- @Highlighter@ trait.
class Highlighter h where
  -- | Style the whole line. @pos@ is the cursor position.
  highlight :: h -> String -> Int -> String
  highlight _ line _ = line

  -- | Style the prompt. @isDefault@ is true for the main prompt.
  highlightPrompt :: h -> String -> Bool -> String
  highlightPrompt _ prompt _ = prompt

  -- | Style an inline hint (rustyline dims these by default).
  highlightHint :: h -> String -> String
  highlightHint _ = dim

  -- | Style a completion candidate in the list.
  highlightCandidate :: h -> String -> CompletionType -> String
  highlightCandidate _ cand _ = cand

  -- | Whether 'highlight' should be re-run after the char at @pos@ changed.
  -- A cheap gate so highlighting isn't recomputed needlessly
  -- (rustyline's @highlight_char@).
  highlightChar :: h -> String -> Int -> Bool
  highlightChar _ _ _ = False

-- | @()@ applies no highlighting (the "no helper" type).
instance Highlighter ()

-- | A built-in highlighter that bolds matching brackets near the cursor
-- (a simplified @MatchingBracketHighlighter@).
data MatchingBracketHighlighter = MatchingBracketHighlighter

instance Highlighter MatchingBracketHighlighter where
  highlightChar _ _ _ = True
  highlight _ line pos =
    case matchAt line pos of
      Just j  -> emphasize line [pos', j]
      Nothing -> line
    where
      -- consider the bracket just before the cursor, as rustyline does
      pos' = pos - 1

-- | ANSI: dim text.
dim :: String -> String
dim s = "\ESC[2m" ++ s ++ reset

-- | ANSI: bold text.
bold :: String -> String
bold s = "\ESC[1m" ++ s ++ reset

-- | ANSI: wrap text in a colour given an SGR code (e.g. @32@ for green).
withColor :: Int -> String -> String
withColor code s = "\ESC[" ++ show code ++ "m" ++ s ++ reset

-- | ANSI reset.
reset :: String
reset = "\ESC[0m"

-- internal: emphasise the chars at the given indices with bold
emphasize :: String -> [Int] -> String
emphasize line idxs =
  concat [ if i `elem` idxs then bold [c] else [c]
         | (i, c) <- zip [0 ..] line ]

-- internal: if a bracket sits just before pos, find its match
matchAt :: String -> Int -> Maybe Int
matchAt line pos
  | i < 0 || i >= length line = Nothing
  | otherwise = case line !! i of
      '(' -> scan 1    (i + 1) '(' ')'
      '[' -> scan 1    (i + 1) '[' ']'
      '{' -> scan 1    (i + 1) '{' '}'
      ')' -> scan (-1) (i - 1) ')' '('
      ']' -> scan (-1) (i - 1) ']' '['
      '}' -> scan (-1) (i - 1) '}' '{'
      _   -> Nothing
  where
    i = pos - 1
    scan dir start closeChar openChar = go start (1 :: Int)
      where
        go k depth
          | k < 0 || k >= length line = Nothing
          | line !! k == closeChar    = go (k + dir) (depth + 1)
          | line !! k == openChar     = if depth == 1 then Just k
                                                       else go (k + dir) (depth - 1)
          | otherwise                 = go (k + dir) depth
