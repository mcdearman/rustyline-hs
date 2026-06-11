{-# LANGUAGE DefaultSignatures #-}

-- | Port of @rustyline::validate@.
--
-- A 'Validator' decides, when the user presses Enter, whether the input is
-- complete. Returning 'Incomplete' lets the user keep typing on a new line
-- (e.g. an unterminated bracket or quote).
module Rustyline.Validate
  ( ValidationResult (..)
  , ValidationContext (..)
  , Validator (..)
  , MatchingBracketValidator (..)
  ) where

-- | Outcome of validation (rustyline's @ValidationResult@).
data ValidationResult
  = Valid (Maybe String)    -- ^ Accept the line; optional message to show.
  | Invalid (Maybe String)  -- ^ Reject; optional message to show.
  | Incomplete              -- ^ Keep editing (insert a newline).
  deriving (Eq, Show)

-- | The input under validation (rustyline's @ValidationContext@).
data ValidationContext = ValidationContext
  { vcInput :: String  -- ^ The full current input.
  , vcPos   :: Int     -- ^ Cursor position.
  }

-- | Validates input on accept. Equivalent to rustyline's @Validator@.
--
-- The default 'validate' always accepts, so @instance Validator MyType@ is a
-- permissive validator.
class Validator v where
  -- | Decide whether the current input is acceptable.
  validate :: v -> ValidationContext -> IO ValidationResult
  default validate :: v -> ValidationContext -> IO ValidationResult
  validate _ _ = pure (Valid Nothing)

  -- | Whether to validate on every keystroke (rustyline
  -- @validate_while_typing@). Defaults to 'False'.
  validateWhileTyping :: v -> Bool
  validateWhileTyping _ = False

-- | @()@ always accepts (the "no helper" type).
instance Validator ()

-- | A built-in validator that treats input with unbalanced @()[]{}@ as
-- 'Incomplete' (rustyline's @MatchingBracketValidator@).
data MatchingBracketValidator = MatchingBracketValidator

instance Validator MatchingBracketValidator where
  validate _ ctx = pure $
    case balance (vcInput ctx) [] of
      Balanced     -> Valid Nothing
      Unclosed     -> Incomplete
      Mismatched c -> Invalid (Just ("Mismatched bracket: " ++ [c]))

data Balance = Balanced | Unclosed | Mismatched Char

balance :: String -> [Char] -> Balance
balance [] []      = Balanced
balance [] (_:_)   = Unclosed
balance (c:cs) stack
  | c `elem` "([{" = balance cs (c : stack)
  | c `elem` ")]}" =
      case stack of
        (o:rest) | matches o c -> balance cs rest
        _                      -> Mismatched c
  | otherwise = balance cs stack
  where
    matches '(' ')' = True
    matches '[' ']' = True
    matches '{' '}' = True
    matches _   _   = False
