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

import Athena.Translation.Utils  ( stdName )
import Athena.Utils.PrettyPrint
  ( (<+>)
  , (<>)
  , (<@>)
  , Doc
  , Pretty(pretty)
--  , braces
  , colon
  , comma
  , comment
  , dot
  , empty
  , encloseSep
  , equals
  , hashtag
  , hypenline
  , indent
  , int
  , lbracket
  , line
  , parens
  , rbracket
  , space
  , vsep
  )

------------------------------------------------------------------------------

unshunt ∷ Formula → Formula
unshunt (BinOp x (:=>:) (BinOp y (:=>:) z)) =
  unshunt (BinOp (BinOp x (:&:) y) (:=>:) z)
unshunt (BinOp x (:=>:) (BinOp y (:&:) z))  =
  BinOp   (unshunt (BinOp x (:=>:) y))
    (:&:) (unshunt (BinOp x (:=>:) z))
unshunt fm = fm

split ∷  Formula → Formula
split (BinOp φ₁ (:&:) φ₂) =
  BinOp
    (unshunt $ split φ₁)
    (:&:)
    (unshunt $ BinOp φ₁ (:=>:) (split φ₂))

split (BinOp φ₁ (:|:) φ₂) =
  unshunt $ BinOp ((:~:) φ₁) (:=>:) (split φ₂)

split (BinOp φ₁ (:=>:) φ₂) =
  unshunt (BinOp φ₁ (:=>:) (split φ₂))

split (BinOp φ₁ (:<=>:) φ₂) =
  BinOp
    (unshunt $ BinOp φ₁ (:=>:) (split φ₂))
    (:&:)
    (unshunt $ BinOp φ₂ (:=>:) (split φ₁))

split ((:~:) (BinOp φ₁ (:&:) φ₂)) =
  unshunt $ BinOp φ₁ (:=>:) (split ((:~:) φ₂))

split ((:~:) (BinOp φ₁ (:|:) φ₂))   =
  BinOp
    (unshunt $ split ((:~:) φ₁))
    (:&:)
    (unshunt $ BinOp ((:~:) φ₁) (:=>:) (split ((:~:) φ₂)))

split ((:~:) (BinOp φ₁ (:=>:) φ₂))  =
  BinOp
    (unshunt $ split φ₁)
    (:&:)
    (unshunt $ BinOp φ₁ (:=>:) (split ((:~:) φ₂)))

split ((:~:) (BinOp φ₁ (:<=>:) φ₂))  =
  BinOp
    (unshunt $ BinOp φ₁ (:=>:) (split ((:~:) φ₂)))
    (:&:)
    (unshunt $ BinOp ((:~:) φ₂) (:=>:) (split φ₁))
split ((:~:) ((:~:)  φ₁))  = split φ₁
split ((:~:) (PredApp (AtomicWord "$false") [])) = PredApp (AtomicWord "$true") []
split ((:~:) (PredApp (AtomicWord "$true") []))  = PredApp (AtomicWord "$false") []
split fm = fm


proofSplit :: Formula → [Formula] → Int → (Doc, [Formula], Int)
proofSplit _ [] n  = (empty, [], n)
proofSplit _ [_] n = (pretty . stdName $ "proof" ++ show n, [], n+1)
proofSplit φ@(BinOp ψ (:&:) γ) gs@(goal : goals) n
  | φ == goal = (pretty (stdName ("proof" ++ show n)), goals, n+1)
  | otherwise = (docφ, lgoals, ln)
  where
    (docψ, cgoals, cn) = proofSplit ψ gs n
    (docγ, lgoals, ln) = proofSplit γ cgoals cn

    docφ ∷ Doc
    docφ = parens (pretty "∧-intro" <> line
             <> indent 2 (docψ <> line <> docγ))

proofSplit φ gs@(goal : goals) n
  | φ == goal = (pretty (stdName $ "proof" ++ show n), goals, n+1)
  | otherwise = (empty,gs,n)


atpSplit ∷ Formula → [Formula] → Doc
atpSplit _ []     = pretty '?'
atpSplit _ [_]    = pretty "proof₀"
atpSplit _ [_,_]  =
  parens $ pretty "∧-intro"
    <+> pretty "proof₀"
    <+> pretty "proof₁"
atpSplit φ sgoals = doc
  where
    splitted ∷ Formula
    splitted  = split φ

    doc ∷ Doc
    (doc, _, _) = proofSplit (split φ) sgoals 0
