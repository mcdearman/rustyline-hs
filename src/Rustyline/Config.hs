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
  ( Config (..),
    defaultConfig,
    EditMode (..),
    CompletionType (..),
    HistoryDuplicates (..),
    BellStyle (..),
    ColorMode (..),

    -- * Builder-style combinators
    setMaxHistorySize,
    setHistoryIgnoreSpace,
    setHistoryDuplicates,
    setCompletionType,
    setEditMode,
    setAutoAddHistory,
    setBellStyle,
    setColorMode,
    (&),
  )
where

import Data.Function ((&))

-- | Emacs or Vi key bindings (rustyline @EditMode@).
data EditMode = Emacs | Vi
  deriving (Eq, Show)

-- | How completion candidates are presented (rustyline @CompletionType@).
data CompletionType
  = -- | Cycle through candidates on repeated Tab (rustyline default).
    Circular
  | -- | List all candidates at once.
    List
  deriving (Eq, Show)

-- | History de-duplication policy (rustyline @HistoryDuplicates@).
data HistoryDuplicates
  = -- | Always add new entries.
    AlwaysAdd
  | -- | Drop an entry equal to the previous one.
    IgnoreConsecutive
  deriving (Eq, Show)

-- | Audible/visible bell behaviour (rustyline @BellStyle@).
data BellStyle = AudibleBell | NoBell | VisibleBell
  deriving (Eq, Show)

-- | When to emit ANSI colour (rustyline @ColorMode@).
data ColorMode = Enabled | Forced | Disabled
  deriving (Eq, Show)

-- | The editor configuration. Field names follow rustyline's builder methods.
data Config = Config
  { -- | @max_history_size@
    maxHistorySize :: !Int,
    -- | @history_ignore_dups@
    historyDuplicates :: !HistoryDuplicates,
    -- | @history_ignore_space@
    historyIgnoreSpace :: !Bool,
    -- | @completion_type@
    completionType :: !CompletionType,
    -- | @completion_prompt_limit@
    completionPromptLimit :: !Int,
    -- | @edit_mode@
    editMode :: !EditMode,
    -- | @auto_add_history@
    autoAddHistory :: !Bool,
    -- | @bell_style@
    bellStyle :: !BellStyle,
    -- | @color_mode@
    colorMode :: !ColorMode,
    -- | @tab_stop@
    tabStop :: !Int
  }
  deriving (Eq, Show)

-- | rustyline's @Config::default()@.
defaultConfig :: Config
defaultConfig =
  Config
    { maxHistorySize = 100,
      historyDuplicates = IgnoreConsecutive,
      historyIgnoreSpace = False,
      completionType = Circular,
      completionPromptLimit = 100,
      editMode = Emacs,
      autoAddHistory = False,
      bellStyle = AudibleBell,
      colorMode = Enabled,
      tabStop = 8
    }

setMaxHistorySize :: Int -> Config -> Config
setMaxHistorySize n c = c {maxHistorySize = n}

setHistoryIgnoreSpace :: Bool -> Config -> Config
setHistoryIgnoreSpace b c = c {historyIgnoreSpace = b}

setHistoryDuplicates :: HistoryDuplicates -> Config -> Config
setHistoryDuplicates d c = c {historyDuplicates = d}

setCompletionType :: CompletionType -> Config -> Config
setCompletionType t c = c {completionType = t}

setEditMode :: EditMode -> Config -> Config
setEditMode m c = c {editMode = m}

setAutoAddHistory :: Bool -> Config -> Config
setAutoAddHistory b c = c {autoAddHistory = b}

setBellStyle :: BellStyle -> Config -> Config
setBellStyle s c = c {bellStyle = s}

setColorMode :: ColorMode -> Config -> Config
setColorMode m c = c {colorMode = m}
