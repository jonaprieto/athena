-- | Data.TSTP.Formula module.
-- Adapted from https://github.com/agomezl/tstp2agda.

{-# LANGUAGE UnicodeSyntax #-}

module Data.TSTP.Formula where

------------------------------------------------------------------------------

import Athena.Utils.PrettyPrint
  ( (<+>)
  , parens
  , Pretty(pretty)
  )
import Data.Function        ( on )
import Data.Monoid          ( mappend )
import Data.Set
  ( Set
  , difference
  , empty
  , fromList
  , singleton
  , toList
  , unions
  )
import Data.TSTP.AtomicWord ( AtomicWord(..) )
import Data.TSTP.BinOp      ( BinOp(..) )
import Data.TSTP.InfixPred  ( InfixPred(..) )
import Data.TSTP.Quant      ( Quant(..) )
import Data.TSTP.Term       ( Term(..))
import Data.TSTP.V          ( V(..) )

------------------------------------------------------------------------------

-- | First-order logic formula.
data Formula = BinOp Formula BinOp Formula    -- ^ Binary connective application
             | InfixPred Term InfixPred Term  -- ^ Infix predicate application
             | PredApp AtomicWord [Term]      -- ^ Predicate application
             | Quant Quant [V] Formula        -- ^ Quantified Formula
             | (:~:) Formula                  -- ^ Negation
             deriving (Eq, Ord, Read, Show)

instance Pretty Formula where
  pretty ((:~:) f )          = parens $ pretty "¬" <+> pretty f
  pretty (BinOp f₁ op f₂)    = parens $ pretty f₁ <+> pretty op <+> pretty f₂
  pretty (InfixPred t₁ r t₂) = parens $ pretty t₁ <+> pretty r <+> pretty t₂
  pretty (PredApp a [])      = pretty a
  pretty (PredApp p l )      = pretty p <+> parens (pretty l)
  pretty (Quant All x p)     = pretty All <+> pretty x <+> parens (pretty p)
  pretty (Quant Exists x p ) = pretty Exists <+> pretty x <+> parens (pretty p)

-- | 'freeVarsF f', returns a 'Set' of all free variables of 'f'.
freeVarsF ∷ Formula → Set V
freeVarsF ((:~:) x)                           = freeVarsF x
freeVarsF (BinOp x _ y)                       = (mappend `on` freeVarsF) x y
freeVarsF (InfixPred x _ y)                   = (mappend `on` freeVarsT) x y
freeVarsF (PredApp (AtomicWord "$false") [])  = empty
freeVarsF (PredApp (AtomicWord v) [])         = singleton $ V v
freeVarsF (PredApp _ args)                    = unions (fmap freeVarsT args)
freeVarsF (Quant _ vars x)                    = difference fvarsx lvars
  where
    fvarsx ∷ Set V
    fvarsx = freeVarsF x

    lvars ∷ Set V
    lvars  = fromList vars

-- | 'freeVarsT t', returns a 'Set' of all free variables of 't'.
freeVarsT ∷ Term → Set V
freeVarsT (FunApp _ args) = unions (fmap freeVarsT args)
freeVarsT (Var x)         = singleton x
freeVarsT _               = empty

-- | 'getFreeVars f', given a list of formulas 'f' return all free
-- variables in the formulas.
getFreeVars ∷ [Formula] → [V]
getFreeVars = toList . unions . map freeVarsF
