
-- | Athena.Translation.Rule.Strip module.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Athena.Translation.Rules.Strip
  ( atpSplit
  , split
  , unshunt
  )
  where

------------------------------------------------------------------------------

import Data.Proof
  ( ProofMap
  , ProofTree
  , ProofTreeGen ( Root, Leaf )
  )

import Data.TSTP
import qualified Data.Map as Map

------------------------------------------------------------------------------

unshunt ∷ Formula → Formula
unshunt (BinOp x (:=>:) (BinOp y (:=>:) z)) =
  unshunt (BinOp (BinOp x (:&:) y) (:=>:) z)
unshunt (BinOp x (:=>:) (BinOp y (:&:) z))  =
  BinOp   (unshunt (BinOp x (:=>:) y))
    (:&:) (unshunt (BinOp x (:=>:) z))
unshunt fm = fm

split ∷  Formula → Formula
split (BinOp φ₁ (:&:) φ₂)           =
  BinOp
    (unshunt (split φ₁))
    (:&:)
    (unshunt (BinOp φ₁ (:=>:) (split φ₂)))

split (BinOp φ₁ (:|:) φ₂)           =
  unshunt (BinOp ((:~:) φ₁) (:=>:) (split φ₂))

split (BinOp φ₁ (:=>:) φ₂)          = unshunt (BinOp φ₁ (:=>:) (split φ₂))
split (BinOp φ₁ (:<=>:) φ₂)         =
  BinOp
    (unshunt (BinOp φ₁ (:=>:) (split φ₂)))
    (:&:)
    (unshunt (BinOp φ₂ (:=>:) (split φ₁)))
split ((:~:) (BinOp φ₁ (:&:) φ₂))   =
  unshunt (BinOp φ₁ (:=>:) (split ((:~:) φ₂)))
split ((:~:) (BinOp φ₁ (:|:) φ₂))   =
  BinOp
    (unshunt (split ((:~:) φ₁)))
    (:&:)
    (unshunt (BinOp ((:~:) φ₁) (:=>:) (split ((:~:) φ₂))))

split ((:~:) (BinOp φ₁ (:=>:) φ₂))  =
  BinOp
    (unshunt (split φ₁))
    (:&:)
    (unshunt (BinOp φ₁ (:=>:) (split ((:~:) φ₂))))

split ((:~:) (BinOp φ₁ (:<=>:) φ₂)) =
  BinOp
    (unshunt (BinOp φ₁ (:=>:) (split ((:~:) φ₂))))
    (:&:)
    (unshunt (BinOp φ₂ (:=>:) (split ((:~:) φ₁))))

split ((:~:) (PredApp (AtomicWord "$false") [])) = PredApp (AtomicWord "$true") []
split ((:~:) (PredApp (AtomicWord "$true") []))  = PredApp (AtomicWord "$false") []
split fm = fm


atpSplit ∷ Formula → ProofMap → Formula
atpSplit _ _ = undefined



