-- | A Haskell port of the public API of the Rust
-- <https://crates.io/crates/rustyline rustyline> line-editing library.
--
-- This umbrella module re-exports the user-facing surface, mirroring the
-- re-exports in rustyline's @lib.rs@. The central type is 'Editor', built
-- from a 'Config' and optionally parameterised by a 'Helper' that bundles a
-- completer, hinter, highlighter and validator.
--
-- == Quick start
--
-- @
-- import Rustyline
--
-- main :: IO ()
-- main = do
--   ed <- 'newDefaultEditor'
--   let loop = do
--         r <- 'readline' ed "\\xBB "
--         case r of
--           Left 'Eof'         -> putStrLn "<EOF>"
--           Left 'Interrupted' -> putStrLn "<Ctrl-C>"
--           Left _           -> pure ()
--           Right line       -> do
--             _ <- 'addHistoryEntry' ed line
--             putStrLn ("Line: " ++ line)
--             loop
--   loop
-- @
--
-- == Concept mapping (Rust → Haskell)
--
-- * @trait Completer@ \/ @Hinter@ \/ @Highlighter@ \/ @Validator@ → the
--   typeclasses of the same name. Their Rust /associated types/ become
--   /associated type families/ ('CandidateOf', 'HintOf').
-- * @trait Helper@ → the 'Helper' class; @()@ is the \"no helper\" type.
-- * @Editor<H>@ with @&mut self@ methods → @'Editor' h@ holding 'IORef's,
--   with module-level functions in place of methods.
-- * @Result<String, ReadlineError>@ → @'Either' 'ReadlineError' 'String'@.
module Rustyline
  ( -- * The editor
    module Rustyline.Editor,

    -- * Configuration
    module Rustyline.Config,

    -- * Errors
    module Rustyline.Error,

    -- * Keys and commands
    module Rustyline.KeyEvent,
    module Rustyline.Command,

    -- * Helper traits
    module Rustyline.Completion,
    module Rustyline.Hint,
    module Rustyline.Highlight,
    module Rustyline.Validate,
    module Rustyline.Helper,
    module Rustyline.Context,

    -- * History
    History,
    emptyHistory,
    getEntry,
    fromList,
    toList,
  )
where

import Rustyline.Command
import Rustyline.Completion
import Rustyline.Config
import Rustyline.Context
import Rustyline.Editor
import Rustyline.Error
import Rustyline.Helper
import Rustyline.Highlight
import Rustyline.Hint
import Rustyline.History
  ( History,
    emptyHistory,
    fromList,
    getEntry,
    toList,
  )
import Rustyline.KeyEvent
import Rustyline.Validate
