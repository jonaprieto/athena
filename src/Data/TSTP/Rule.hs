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
  pretty Canonicalize = pretty "atp" <> hypen <> pretty "canonicalize"
  pretty Clausify     = pretty "atp" <> hypen <> pretty "clausify"
  pretty Conjunct     = pretty "atp" <> hypen <> pretty "conjunct"
  pretty Negate       = pretty "atp" <> hypen <> pretty "negate"
  pretty (NewRule r)  = pretty "atp" <> hypen <> pretty r
  pretty Resolve      = pretty "atp" <> hypen <> pretty "resolve"
  pretty Simplify     = pretty "atp" <> hypen <> pretty "simplify"
  pretty Skolemize    = pretty "atp" <> hypen <> pretty "skolemize"
  pretty Specialize   = pretty "atp" <> hypen <> pretty "specialize"
  pretty Strip        = pretty "atp" <> hypen <> pretty "strip"
