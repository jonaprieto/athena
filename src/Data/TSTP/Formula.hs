
-- | Data.TSTP.Formula module.
-- Adapted from https://github.com/agomezl/tstp2agda.

{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# LANGUAGE UnicodeSyntax                #-}

module Data.TSTP.Formula where

------------------------------------------------------------------------------

import Athena.Utils.PrettyPrint
  ( (<>)
  , cparentesis
  , Pretty(pretty)
  , spaces
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

-- | first-order logic formula.
data Formula = BinOp Formula BinOp Formula    -- ^ Binary connective application
             | InfixPred Term InfixPred Term  -- ^ Infix predicate application
             | PredApp AtomicWord [Term]      -- ^ Predicate application
             | Quant Quant [V] Formula        -- ^ Quantified Formula
             | (:~:) Formula                  -- ^ Negation
             deriving (Eq, Ord, Read, Show)

instance Pretty Formula where
  pretty ((:~:) f )          =
    cparentesis $ pretty "¬ " <> pretty f
  pretty (BinOp f₁ op f₂)    =
    cparentesis $ pretty f₁ <> spaces (pretty op) <> pretty f₂
  pretty (InfixPred t₁ r t₂) =
    cparentesis $ pretty t₁ <> spaces (pretty r) <> pretty t₂
  pretty (PredApp a [])      = pretty a
  pretty (PredApp p l )      =
    pretty p <> cparentesis (pretty l)
  pretty (Quant All x p)     =
    pretty All <> pretty x <> cparentesis (pretty p)
  pretty (Quant Exists x p ) =
    pretty Exists <> pretty x <> cparentesis (pretty p)

-- | 'freeVarsF' 'f', returns a 'Set' of all free variables of 'f'.
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

    lvars  = fromList vars

-- | 'freeVarsT' 't', returns a 'Set' of all free variables of 't'.
freeVarsT ∷ Term → Set V
freeVarsT (FunApp _ args) = unions (fmap freeVarsT args)
freeVarsT (Var x)         = singleton x
freeVarsT _               = empty

-- | 'getFreeVars' 'f', given a list of formulas 'f' return all free
-- variables in the formulas.
getFreeVars ∷ [Formula] → [V]
getFreeVars = toList . unions . map freeVarsF
