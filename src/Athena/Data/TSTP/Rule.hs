
-- | Data.TSTP.Rule module

module Data.TSTP.Rule where

------------------------------------------------------------------------------

-- | Deduction rule applied.
data Rule = Canonicalize
          | Conjunct
          | Negate
          | NewRule String
          | Resolve
          | Simplify
          | Skolemize
          | Specialize
          | Strip
          deriving (Eq, Ord, Show, Read)
