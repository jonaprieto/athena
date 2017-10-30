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
  pretty Canonicalize = pretty (pretty "canonicalize-thm")
  pretty Clausify     = pretty (pretty "clausify-thm")
  pretty Conjunct     = pretty (pretty "conjunct-thm")
  pretty Negate       = pretty (pretty "negate-thm")
  pretty Resolve      = pretty (pretty "resolve-thm")
  pretty Simplify     = pretty (pretty "simplify-thm")
  pretty Strip        = pretty (pretty "strip-thm")
  pretty _            = error "Rule no supported yet"
