-- | Port of @rustyline::error::ReadlineError@.
--
-- In Rust, @readline@ returns @Result<String, ReadlineError>@. The Haskell
-- analogue is @IO (Either ReadlineError String)@, so this module defines the
-- error sum type that inhabits the @Left@.
module Rustyline.Error
  ( ReadlineError (..)
  ) where

import Control.Exception (IOException)

-- | The error type returned by line reading.
--
-- Mirrors rustyline's variants. The two you handle most often are
-- 'Eof' (the user pressed Ctrl-D on an empty line) and 'Interrupted'
-- (the user pressed Ctrl-C) — exactly as in rustyline.
data ReadlineError
  = -- | End of file / input stream (Ctrl-D on an empty line).
    Eof
  | -- | The user interrupted with Ctrl-C.
    Interrupted
  | -- | An underlying I\/O error.
    Io IOException
  | -- | The terminal was resized while reading (rustyline's @WindowResized@).
    WindowResized
  | -- | Any other error, with a human-readable message.
    Errno String
  deriving (Show)
