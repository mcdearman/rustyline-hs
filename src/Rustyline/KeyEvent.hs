-- | Port of rustyline's key types (@rustyline::keys@): 'KeyCode',
-- 'Modifiers' and 'KeyEvent'.
module Rustyline.KeyEvent
  ( KeyCode (..)
  , Modifiers (..)
  , KeyEvent (..)
  , noMod
  , ctrl
  , alt
  , key
  , ctrlKey
  , altKey
  ) where

import Data.Bits ((.|.))

-- | A physical key, independent of modifiers.
data KeyCode
  = Char Char        -- ^ A printable character.
  | Enter
  | Tab
  | Backspace
  | Delete
  | Esc
  | Home
  | End
  | LeftArrow
  | RightArrow
  | UpArrow
  | DownArrow
  | PageUp
  | PageDown
  | Null              -- ^ An unrecognised\/empty key.
  deriving (Eq, Ord, Show)

-- | Active modifier keys. Mirrors the bit-flag @Modifiers@ struct in
-- rustyline (here a simple record, with a 'Monoid' to combine them).
data Modifiers = Modifiers
  { modCtrl  :: !Bool
  , modAlt   :: !Bool
  , modShift :: !Bool
  } deriving (Eq, Ord, Show)

instance Semigroup Modifiers where
  Modifiers a b c <> Modifiers d e f =
    Modifiers (a .|. d) (b .|. e) (c .|. f)
    where (.|.) = (||)

instance Monoid Modifiers where
  mempty = noMod

-- | No modifiers held.
noMod :: Modifiers
noMod = Modifiers False False False

-- | The Control modifier.
ctrl :: Modifiers
ctrl = noMod { modCtrl = True }

-- | The Alt\/Meta modifier.
alt :: Modifiers
alt = noMod { modAlt = True }

-- | A key press: a 'KeyCode' plus its 'Modifiers'.
-- Equivalent to rustyline's @KeyEvent(KeyCode, Modifiers)@.
data KeyEvent = KeyEvent KeyCode Modifiers
  deriving (Eq, Ord, Show)

-- | Smart constructor for an unmodified key.
key :: KeyCode -> KeyEvent
key c = KeyEvent c noMod

-- | @ctrlKey 'a'@ is Ctrl-A. Equivalent to rustyline's @KeyEvent::ctrl('a')@.
ctrlKey :: Char -> KeyEvent
ctrlKey c = KeyEvent (Char c) ctrl

-- | @altKey 'b'@ is Alt-B. Equivalent to rustyline's @KeyEvent::alt('b')@.
altKey :: Char -> KeyEvent
altKey c = KeyEvent (Char c) alt
