-- | Port of @rustyline::line_buffer@: the pure editing buffer.
--
-- This is deliberately IO-free so the editing logic is unit-testable
-- independently of any terminal. Positions are character indices into the
-- line. Every operation returns a new 'LineBuffer'.
module Rustyline.LineBuffer
  ( LineBuffer (..)
  , empty
  , fromString
  , asString
  , setLine
    -- * Queries
  , bufLen
  , atStart
  , atEnd
    -- * Editing
  , insertChar
  , insertStr
  , backspace
  , deleteChar
  , transpose
    -- * Movement
  , moveLeft
  , moveRight
  , moveHome
  , moveEnd
  , moveBackwardWord
  , moveForwardWord
    -- * Killing
  , killToEnd
  , killToHome
  , killWordBackward
  , killWordForward
  ) where

import Data.Char (isSpace)

-- | A line of text plus a cursor. @lbPos@ is a char index in @0 .. length@.
data LineBuffer = LineBuffer
  { lbText :: !String
  , lbPos  :: !Int
  } deriving (Eq, Show)

-- | An empty buffer.
empty :: LineBuffer
empty = LineBuffer "" 0

-- | A buffer holding @s@ with the cursor at the end.
fromString :: String -> LineBuffer
fromString s = LineBuffer s (length s)

-- | The buffer's text.
asString :: LineBuffer -> String
asString = lbText

-- | Replace the text, clamping the cursor to the given position.
setLine :: String -> Int -> LineBuffer
setLine s p = LineBuffer s (clamp 0 (length s) p)

bufLen :: LineBuffer -> Int
bufLen = length . lbText

atStart :: LineBuffer -> Bool
atStart b = lbPos b <= 0

atEnd :: LineBuffer -> Bool
atEnd b = lbPos b >= bufLen b

clamp :: Int -> Int -> Int -> Int
clamp lo hi = max lo . min hi

-- | Insert a char at the cursor and advance.
insertChar :: Char -> LineBuffer -> LineBuffer
insertChar c (LineBuffer t p) =
  let (l, r) = splitAt p t in LineBuffer (l ++ c : r) (p + 1)

-- | Insert a string at the cursor and advance past it.
insertStr :: String -> LineBuffer -> LineBuffer
insertStr s (LineBuffer t p) =
  let (l, r) = splitAt p t in LineBuffer (l ++ s ++ r) (p + length s)

-- | Delete the char before the cursor (Backspace).
backspace :: LineBuffer -> LineBuffer
backspace b@(LineBuffer t p)
  | p <= 0    = b
  | otherwise = let (l, r) = splitAt p t
                in LineBuffer (init l ++ r) (p - 1)

-- | Delete the char under the cursor.
deleteChar :: LineBuffer -> LineBuffer
deleteChar b@(LineBuffer t p)
  | p >= length t = b
  | otherwise     = let (l, r) = splitAt p t in LineBuffer (l ++ drop 1 r) p

-- | Swap the two characters around the cursor (Ctrl-T).
transpose :: LineBuffer -> LineBuffer
transpose b@(LineBuffer t p)
  | length t < 2 = b
  | p <= 0       = b
  | otherwise =
      let q = min p (length t - 1)         -- cursor at end: swap last two
          xs = t
          a = xs !! (q - 1)
          c = xs !! q
          pre  = take (q - 1) xs
          post = drop (q + 1) xs
      in LineBuffer (pre ++ [c, a] ++ post) (min (q + 1) (length t))

moveLeft :: LineBuffer -> LineBuffer
moveLeft (LineBuffer t p) = LineBuffer t (max 0 (p - 1))

moveRight :: LineBuffer -> LineBuffer
moveRight (LineBuffer t p) = LineBuffer t (min (length t) (p + 1))

moveHome :: LineBuffer -> LineBuffer
moveHome (LineBuffer t _) = LineBuffer t 0

moveEnd :: LineBuffer -> LineBuffer
moveEnd (LineBuffer t _) = LineBuffer t (length t)

-- | Move to the start of the previous word.
moveBackwardWord :: LineBuffer -> LineBuffer
moveBackwardWord (LineBuffer t p) = LineBuffer t (prevWord t p)

-- | Move to the end of the next word.
moveForwardWord :: LineBuffer -> LineBuffer
moveForwardWord (LineBuffer t p) = LineBuffer t (nextWord t p)

-- | Delete from the cursor to the end of the line (Ctrl-K).
killToEnd :: LineBuffer -> LineBuffer
killToEnd (LineBuffer t p) = LineBuffer (take p t) p

-- | Delete from the start of the line to the cursor (Ctrl-U).
killToHome :: LineBuffer -> LineBuffer
killToHome (LineBuffer t p) = LineBuffer (drop p t) 0

-- | Delete the word before the cursor (Ctrl-W).
killWordBackward :: LineBuffer -> LineBuffer
killWordBackward (LineBuffer t p) =
  let start = prevWord t p
      (l, r) = splitAt start t
  in LineBuffer (l ++ drop (p - start) r) start

-- | Delete the word after the cursor (Alt-D).
killWordForward :: LineBuffer -> LineBuffer
killWordForward (LineBuffer t p) =
  let end = nextWord t p
      (l, r) = splitAt p t
  in LineBuffer (l ++ drop (end - p) r) p

-- internal word-boundary scanning (whitespace-delimited)

prevWord :: String -> Int -> Int
prevWord t p =
  let i  = skipWhile isSpace (p - 1) (-1)
      j  = skipWhile (not . isSpace) i (-1)
  in j + 1
  where
    skipWhile pr = go
      where
        go k step
          | k < 0 || k >= length t = k
          | pr (t !! k)            = go (k + step) step
          | otherwise              = k

nextWord :: String -> Int -> Int
nextWord t p =
  let i = skipWhile isSpace p 1
      j = skipWhile (not . isSpace) i 1
  in j
  where
    skipWhile pr = go
      where
        go k step
          | k < 0 || k >= length t = k
          | pr (t !! k)            = go (k + step) step
          | otherwise              = k
