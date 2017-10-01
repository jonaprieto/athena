-- | Data.TSTP.Rule module.
-- Adapted from https://github.com/agomezl/tstp2agda.

module Data.TSTP.Rule
  ( Rule
    ( Canonicalize
    , Clausify
    , Conjunct
    , Negate
    , NewRule
    , Resolve
    , Simplify
    , Skolemize
    , Specialize
    , Strip
    )
  ) where

------------------------------------------------------------------------------

import Athena.Utils.PrettyPrint ( Pretty ( pretty ), hypen, (<>) )

------------------------------------------------------------------------------

-- | Deduction rule applied.
data Rule = Canonicalize
          | Clausify
          | Conjunct
          | Negate
          | NewRule String
          | Resolve
          | Simplify
          | Skolemize
          | Specialize
          | Strip
          deriving (Eq, Ord, Read, Show)

instance Pretty Rule where
  pretty Canonicalize = pretty "thm" <> hypen <> pretty "canonicalize"
  pretty Clausify     = pretty "thm" <> hypen <> pretty "clausify"
  pretty Conjunct     = pretty "thm" <> hypen <> pretty "conjunct"
  pretty Negate       = pretty "thm" <> hypen <> pretty "negate"
  pretty Resolve      = pretty "thm" <> hypen <> pretty "resolve"
  pretty Simplify     = pretty "thm" <> hypen <> pretty "simplify"
  pretty Strip        = pretty "thm" <> hypen <> pretty "strip"
  pretty _            = error "Rule no supported yet"