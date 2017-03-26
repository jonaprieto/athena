
-- | Data.TSTP.Term module.

module Data.TSTP.Term where

------------------------------------------------------------------------------

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
          deriving (Eq, Ord, Read)

instance Show Term where
  show (Var             (V v))      =      v
  show (NumberLitTerm      r )      = show r
  show (DistinctObjectTerm t )      =      t
  show (FunApp (AtomicWord w ) [])  =      w
  show (FunApp (AtomicWord _ ) _) = error "Don't really know what this is"
