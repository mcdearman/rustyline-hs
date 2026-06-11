-- | Port of @rustyline::Context@ — the read-only view handed to a
-- 'Rustyline.Completion.Completer', 'Rustyline.Hint.Hinter' and
-- 'Rustyline.Validate.Validator' so they can consult history and the cursor.
module Rustyline.Context
  ( Context (..)
  , historyEntries
  , cursorPos
  ) where

-- | Read-only context. In rustyline this borrows the history; here it carries
-- an immutable snapshot, which is enough for completion\/hinting.
data Context = Context
  { ctxHistory :: [String]  -- ^ History snapshot, oldest first.
  , ctxPos     :: !Int      -- ^ Cursor position (char index) in the line.
  }

-- | The history entries visible to the helper.
historyEntries :: Context -> [String]
historyEntries = ctxHistory

-- | The cursor position within the current line.
cursorPos :: Context -> Int
cursorPos = ctxPos
