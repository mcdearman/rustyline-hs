-- | Port of rustyline's editing commands (@rustyline::command::Cmd@).
--
-- A 'Cmd' is the action that a 'Rustyline.KeyEvent.KeyEvent' is bound to.
-- The default Emacs key map maps keys to these; you can override bindings
-- with 'Rustyline.Editor.bindSequence'.
module Rustyline.Command
  ( Cmd (..)
  , Movement (..)
  , Anchor (..)
  ) where

-- | Where a kill\/move acts relative to the cursor.
data Anchor = Forward | Backward
  deriving (Eq, Show)

-- | A unit of movement.
data Movement
  = MoveChar Anchor
  | MoveWord Anchor
  | MoveLineStart
  | MoveLineEnd
  deriving (Eq, Show)

-- | An editing command. This is a representative subset of rustyline's
-- @Cmd@ enum covering the Emacs key map.
data Cmd
  = SelfInsert Char          -- ^ Insert a character (rustyline: @SelfInsert@).
  | Move Movement            -- ^ Move the cursor.
  | Kill Movement            -- ^ Delete text in the given direction\/extent.
  | BackwardDeleteChar       -- ^ Delete the char before the cursor (Backspace).
  | DeleteChar               -- ^ Delete the char under the cursor.
  | TransposeChars           -- ^ Swap the two chars around the cursor (Ctrl-T).
  | PreviousHistory          -- ^ Recall the previous history entry.
  | NextHistory              -- ^ Recall the next history entry.
  | AcceptLine               -- ^ Finish editing and return the line (Enter).
  | Interrupt                -- ^ Ctrl-C — abort with 'Rustyline.Error.Interrupted'.
  | EndOfFile                -- ^ Ctrl-D on empty line — 'Rustyline.Error.Eof'.
  | Complete                 -- ^ Trigger completion (Tab).
  | ClearScreen              -- ^ Clear the screen (Ctrl-L).
  | Noop                     -- ^ Do nothing.
  deriving (Eq, Show)
