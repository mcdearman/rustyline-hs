{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}

-- | Port of @rustyline::hint@.
--
-- A 'Hinter' offers an inline suggestion shown after the cursor (dimmed),
-- like fish-shell autosuggestions. Mirrors rustyline's @Hinter@ trait with
-- its associated @Hint@ type.
module Rustyline.Hint
  ( Hint (..)
  , Hinter (..)
  , HistoryHinter (..)
  ) where

import Rustyline.Context (Context, historyEntries)
import Data.List (isPrefixOf, find)

-- | A hint value (rustyline's @Hint@ trait): the text to display, and
-- optionally the text to insert if the user completes the hint.
class Hint a where
  -- | Text displayed after the cursor.
  hintDisplay :: a -> String
  -- | Text to insert when the hint is accepted (e.g. via End), if any.
  hintCompletion :: a -> Maybe String
  hintCompletion _ = Nothing

-- | A bare 'String' is a hint that only displays.
instance Hint [Char] where
  hintDisplay = id
  hintCompletion = Just

-- | Produces inline hints. Equivalent to rustyline's @Hinter@.
--
-- The default 'hint' returns 'Nothing', so a no-op hinter is just
-- @instance Hinter MyType@.
class Hint (HintOf h) => Hinter h where
  -- | The hint type (rustyline's associated @type Hint@).
  type HintOf h
  type HintOf h = String

  -- | @hint h line pos ctx@ optionally returns a suggestion for the line.
  hint :: h -> String -> Int -> Context -> IO (Maybe (HintOf h))
  default hint :: h -> String -> Int -> Context -> IO (Maybe (HintOf h))
  hint _ _ _ _ = pure Nothing

-- | @()@ hints nothing (the "no helper" type).
instance Hinter () where
  type HintOf () = String

-- | A built-in hinter that suggests the most recent history entry starting
-- with the current line (rustyline's @HistoryHinter@).
data HistoryHinter = HistoryHinter

instance Hinter HistoryHinter where
  type HintOf HistoryHinter = String
  hint _ line pos ctx
    | null line || pos /= length line = pure Nothing
    | otherwise =
        pure $ do
          entry <- find (line `isPrefixOf`) (reverse (historyEntries ctx))
          let suffix = drop (length line) entry
          if null suffix then Nothing else Just suffix
