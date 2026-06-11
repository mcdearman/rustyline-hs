-- | Port of @rustyline::history@.
--
-- rustyline exposes a @History@ trait with a default @FileHistory@. Here we
-- provide the concrete default history (the one nearly everyone uses) as a
-- plain value, plus file load\/save. Entries are stored oldest-first.
module Rustyline.History
  ( History
  , emptyHistory
  , addEntry
  , getEntry
  , len
  , isEmpty
  , clearHistory
  , toList
  , fromList
  , saveTo
  , loadFrom
  , appendTo
  ) where

import Rustyline.Config
  ( Config (maxHistorySize, historyDuplicates, historyIgnoreSpace)
  , HistoryDuplicates (..)
  )
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>))
import qualified Data.Foldable as F
import Data.Char (isSpace)
import System.Directory (doesFileExist)

-- | A bounded history of input lines, oldest-first.
data History = History
  { histSeq :: !(Seq String)
  , histMax :: !Int
  }

-- | An empty history sized from the 'Config'.
emptyHistory :: Config -> History
emptyHistory cfg = History Seq.empty (maxHistorySize cfg)

-- | Number of entries.
len :: History -> Int
len = Seq.length . histSeq

isEmpty :: History -> Bool
isEmpty = Seq.null . histSeq

-- | @getEntry h i@ returns entry @i@ (0 = oldest).
getEntry :: History -> Int -> Maybe String
getEntry h i = Seq.lookup i (histSeq h)

-- | All entries, oldest first.
toList :: History -> [String]
toList = F.toList . histSeq

-- | Build a history from a list (oldest first) honouring a max size.
fromList :: Config -> [String] -> History
fromList cfg = foldl (\h s -> fst (addEntry cfg s h)) (emptyHistory cfg)

-- | Add a line. Returns the new history and whether it was actually added
-- (matching rustyline's @add@, which returns a @bool@). Honours
-- @history_ignore_space@, @history_ignore_dups@ and @max_history_size@.
addEntry :: Config -> String -> History -> (History, Bool)
addEntry cfg line h
  | ignoreSpace, leadingSpace        = (h, False)
  | null line                        = (h, False)
  | dropDup, lastEntry == Just line  = (h, False)
  | otherwise                        = (trimmed, True)
  where
    ignoreSpace  = historyIgnoreSpace cfg
    leadingSpace = case line of (c:_) -> isSpace c; _ -> False
    dropDup      = historyDuplicates cfg == IgnoreConsecutive
    lastEntry    = case Seq.viewr (histSeq h) of
                     _ Seq.:> x -> Just x
                     Seq.EmptyR -> Nothing
    appended     = histSeq h |> line
    overflow     = Seq.length appended - histMax h
    trimmed      = h { histSeq = if overflow > 0
                                   then Seq.drop overflow appended
                                   else appended }

-- | Remove all entries.
clearHistory :: History -> History
clearHistory h = h { histSeq = Seq.empty }

-- | Write history to a file, one entry per line (rustyline @save_history@).
saveTo :: FilePath -> History -> IO ()
saveTo path h = writeFile path (unlines (Rustyline.History.toList h))

-- | Append entries to a file (rustyline @append_history@).
appendTo :: FilePath -> History -> IO ()
appendTo path h = appendFile path (unlines (Rustyline.History.toList h))

-- | Load history from a file, replacing the in-memory entries
-- (rustyline @load_history@). Missing files are treated as empty.
loadFrom :: Config -> FilePath -> IO History
loadFrom cfg path = do
  exists <- doesFileExist path
  if not exists
    then pure (emptyHistory cfg)
    else do
      contents <- readFile path
      let entries = filter (not . null) (lines contents)
      pure $! fromList cfg entries
