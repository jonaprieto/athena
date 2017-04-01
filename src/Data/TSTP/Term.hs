
-- | Data.TSTP.Term module.
-- Adapted from https://github.com/agomezl/tstp2agda.

module Data.TSTP.Term where

------------------------------------------------------------------------------

import Athena.Utils.PrettyPrint
  ( Pretty(pretty)
  , rational
  )

import Data.TSTP.AtomicWord ( AtomicWord(..) )
import Data.TSTP.V          ( V(..) )

------------------------------------------------------------------------------

-- | First-order logic terms.
data Term = Var V                             -- ^ Variable
          | NumberLitTerm Rational            -- ^ Number literal
          | DistinctObjectTerm String         -- ^ Double-quoted item
          | FunApp AtomicWord [Term]          -- ^ Function symbol application
                                              -- (constants are encoded as
                                              -- nullary functions)
          deriving (Eq, Ord, Read, Show)

instance Pretty Term where
  pretty (Var             (V v))      = pretty v
  pretty (NumberLitTerm      r )      = rational r
  pretty (DistinctObjectTerm t )      = pretty t
  pretty (FunApp (AtomicWord w ) [])  = pretty w
  pretty (FunApp (AtomicWord _ ) _)   = error "Don't really know what this is"
