{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Port of @rustyline::Helper@.
--
-- In rustyline @Helper@ is a marker trait requiring @Completer + Hinter +
-- Highlighter + Validator@, usually produced by @#[derive(Helper)]@. Here it
-- is a class with those four as superclasses; any type implementing all four
-- gets a free 'Helper' instance via the blanket instance below.
module Rustyline.Helper
  ( Helper,
  )
where

import Rustyline.Completion (Completer)
import Rustyline.Highlight (Highlighter)
import Rustyline.Hint (Hinter)
import Rustyline.Validate (Validator)

-- | The composite constraint a custom helper must satisfy. Implement the four
-- component classes and you get @Helper@ for free.
class (Completer h, Hinter h, Highlighter h, Validator h) => Helper h

instance (Completer h, Hinter h, Highlighter h, Validator h) => Helper h
