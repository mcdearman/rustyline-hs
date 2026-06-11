-- | Port of @rustyline::config@.
--
-- Rust uses a builder: @Config::builder().max_history_size(50).build()@.
-- The Haskell idiom is a record with defaults ('defaultConfig') updated by
-- record syntax, plus chainable @set*@ combinators that mirror the builder
-- method names so the two styles read alike:
--
-- @
-- cfg = defaultConfig & setMaxHistorySize 50 & setEditMode Emacs
-- -- or:
-- cfg = defaultConfig { maxHistorySize = 50, editMode = Emacs }
-- @
module Rustyline.Config
  ( Config (..)
  , defaultConfig
  , EditMode (..)
  , CompletionType (..)
  , HistoryDuplicates (..)
  , BellStyle (..)
  , ColorMode (..)
    -- * Builder-style combinators
  , setMaxHistorySize
  , setHistoryIgnoreSpace
  , setHistoryDuplicates
  , setCompletionType
  , setEditMode
  , setAutoAddHistory
  , setBellStyle
  , setColorMode
  , (&)
  ) where

import Data.Function ((&))

-- | Emacs or Vi key bindings (rustyline @EditMode@).
data EditMode = Emacs | Vi
  deriving (Eq, Show)

-- | How completion candidates are presented (rustyline @CompletionType@).
data CompletionType
  = Circular  -- ^ Cycle through candidates on repeated Tab (rustyline default).
  | List      -- ^ List all candidates at once.
  deriving (Eq, Show)

-- | History de-duplication policy (rustyline @HistoryDuplicates@).
data HistoryDuplicates
  = AlwaysAdd          -- ^ Always add new entries.
  | IgnoreConsecutive  -- ^ Drop an entry equal to the previous one.
  deriving (Eq, Show)

-- | Audible/visible bell behaviour (rustyline @BellStyle@).
data BellStyle = AudibleBell | NoBell | VisibleBell
  deriving (Eq, Show)

-- | When to emit ANSI colour (rustyline @ColorMode@).
data ColorMode = Enabled | Forced | Disabled
  deriving (Eq, Show)

-- | The editor configuration. Field names follow rustyline's builder methods.
data Config = Config
  { maxHistorySize     :: !Int                -- ^ @max_history_size@
  , historyDuplicates  :: !HistoryDuplicates  -- ^ @history_ignore_dups@
  , historyIgnoreSpace :: !Bool               -- ^ @history_ignore_space@
  , completionType     :: !CompletionType     -- ^ @completion_type@
  , completionPromptLimit :: !Int             -- ^ @completion_prompt_limit@
  , editMode           :: !EditMode           -- ^ @edit_mode@
  , autoAddHistory     :: !Bool               -- ^ @auto_add_history@
  , bellStyle          :: !BellStyle          -- ^ @bell_style@
  , colorMode          :: !ColorMode          -- ^ @color_mode@
  , tabStop            :: !Int                 -- ^ @tab_stop@
  } deriving (Eq, Show)

-- | rustyline's @Config::default()@.
defaultConfig :: Config
defaultConfig = Config
  { maxHistorySize        = 100
  , historyDuplicates     = IgnoreConsecutive
  , historyIgnoreSpace    = False
  , completionType        = Circular
  , completionPromptLimit = 100
  , editMode              = Emacs
  , autoAddHistory        = False
  , bellStyle             = AudibleBell
  , colorMode             = Enabled
  , tabStop               = 8
  }

setMaxHistorySize :: Int -> Config -> Config
setMaxHistorySize n c = c { maxHistorySize = n }

setHistoryIgnoreSpace :: Bool -> Config -> Config
setHistoryIgnoreSpace b c = c { historyIgnoreSpace = b }

setHistoryDuplicates :: HistoryDuplicates -> Config -> Config
setHistoryDuplicates d c = c { historyDuplicates = d }

setCompletionType :: CompletionType -> Config -> Config
setCompletionType t c = c { completionType = t }

setEditMode :: EditMode -> Config -> Config
setEditMode m c = c { editMode = m }

setAutoAddHistory :: Bool -> Config -> Config
setAutoAddHistory b c = c { autoAddHistory = b }

setBellStyle :: BellStyle -> Config -> Config
setBellStyle s c = c { bellStyle = s }

setColorMode :: ColorMode -> Config -> Config
setColorMode m c = c { colorMode = m }
