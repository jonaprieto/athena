
-- | Translation.Rule.Conjunct module.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Athena.Translation.Rules.Conjunct ( atpConjunct ) where

------------------------------------------------------------------------------

import Data.TSTP
  ( Formula(..)
  )
import Data.TSTP.BinOp ( BinOp(..) )

------------------------------------------------------------------------------

atpConjunct ∷ Formula → Formula → String
atpConjunct (BinOp f₁ (:&:) f₂) g
  | f₂ /= g   = "∧-proj₁ $" ++ if null next then "\n" else " " ++ next
  | otherwise = "∧-proj₂ $ -- (" ++ show f₂ ++" ≟ " ++ show g ++ ")\n"
  where
    next ∷ String
    next = atpConjunct f₁ g

atpConjunct BinOp{} _       = "-- case 1. \n"
atpConjunct InfixPred{} _   = "-- case 2. \n"
atpConjunct (PredApp _ _) _ = "-- case 3. \n"
atpConjunct Quant{} _       = "-- case 4. \n"
atpConjunct ((:~:) _) _     = "-- case 5. \n"
