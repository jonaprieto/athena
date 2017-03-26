
-- | Data.TSTP.Formula module.

{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# LANGUAGE UnicodeSyntax                #-}

module Data.TSTP.Formula where

------------------------------------------------------------------------------

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

-- The following code is based on:
-- https://github.com/DanielSchuessler/logic-TPTP
-- and is just a simplified version of the the datatypes
-- (without the Indirect composite)

-- | first-order logic formula.
data Formula = BinOp Formula BinOp Formula    -- ^ Binary connective application
             | InfixPred Term InfixPred Term  -- ^ Infix predicate application
             | PredApp AtomicWord [Term]      -- ^ Predicate application
             | Quant Quant [V] Formula        -- ^ Quantified Formula
             | (:~:) Formula                  -- ^ Negation
             deriving (Eq, Ord, Read)

-- TODO: use of PrettyPrinter
instance Show Formula where
  show ((:~:) f )            = "(" ++ "¬ " ++ show f ++ ")"
  show (BinOp f₁ (:=>:) f₂)  = "(" ++ show f₁ ++ " ⇒ " ++ show f₂ ++ ")"
  show (BinOp f₁ (:<=>:) f₂) = "(" ++ show f₁ ++ " ⇔ " ++ show f₂ ++ ")"
  show (BinOp f₁ op f₂)      = "(" ++ show f₁ ++ " " ++ show op ++ " " ++ show f₂ ++ ")"
  show (InfixPred t₁ r t₂)   = "(" ++ show t₁ ++ " " ++ show r  ++ " " ++ show t₂ ++ ")"
  show (PredApp (AtomicWord "$false") []) = "⊥"
  show (PredApp (AtomicWord "$true")  []) = "⊤"
  show (PredApp (AtomicWord p)  [])       = p
  show (PredApp ρ ϕ )            = "-- not supported yet."
  show (Quant     All []     _ ) = "-- not supported yet."
  show (Quant     All _      _ ) = "-- not supported yet."
  show (Quant     Exists _   _ ) = "-- not supported yet."

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
