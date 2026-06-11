-- | Port of rustyline's editing commands (@rustyline::command::Cmd@).
--
-- A 'Cmd' is the action that a 'Rustyline.KeyEvent.KeyEvent' is bound to.
-- The default Emacs key map maps keys to these; you can override bindings
-- with 'Rustyline.Editor.bindSequence'.
module Rustyline.Command
  ( Cmd (..),
    Movement (..),
    Anchor (..),
  )
where

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
  = -- | Insert a character (rustyline: @SelfInsert@).
    SelfInsert Char
  | -- | Move the cursor.
    Move Movement
  | -- | Delete text in the given direction\/extent.
    Kill Movement
  | -- | Delete the char before the cursor (Backspace).
    BackwardDeleteChar
  | -- | Delete the char under the cursor.
    DeleteChar
  | -- | Swap the two chars around the cursor (Ctrl-T).
    TransposeChars
  | -- | Recall the previous history entry.
    PreviousHistory
  | -- | Recall the next history entry.
    NextHistory
  | -- | Finish editing and return the line (Enter).
    AcceptLine
  | -- | Ctrl-C — abort with 'Rustyline.Error.Interrupted'.
    Interrupt
  | -- | Ctrl-D on empty line — 'Rustyline.Error.Eof'.
    EndOfFile
  | -- | Trigger completion (Tab).
    Complete
  | -- | Clear the screen (Ctrl-L).
    ClearScreen
  | -- | Do nothing.
    Noop
  deriving (Eq, Show)
