-- | Data.TSTP.BinOp module.
-- Adapted from https://github.com/agomezl/tstp2agda.

{-# LANGUAGE UnicodeSyntax #-}

module Data.TSTP.BinOp
  ( BinOp
    ( (:<=>:)
    , (:=>:)
    , (:<=:)
    , (:&:)
    , (:|:)
    , (:~&:)
    , (:~|:)
    , (:<~>:)
    )
  ) where

------------------------------------------------------------------------------

import Athena.Utils.PrettyPrint ( Pretty(pretty) )

------------------------------------------------------------------------------

-- | Binary formula connectives.
data BinOp = (:<=>:)  -- ^ ⇔ /Equivalence/
           | (:=>:)   -- ^ ⊃ /Implication/
           | (:<=:)   -- ^ ⇐ /Reverse Implication/
           | (:&:)    -- ^ ∧ /AND/
           | (:|:)    -- ^ ∨ /OR/
           | (:~&:)   -- ^ ⊼ /NAND/
           | (:~|:)   -- ^ ⊽ /NOR/
           | (:<~>:)  -- ^ ⊕ /XOR/
           deriving (Eq, Ord, Read, Show)

instance Pretty BinOp where
  pretty (:<=>:) = pretty "⇔"
  pretty (:=>:)  = pretty "⊃"
  pretty (:<=:)  = pretty "⇐"
  pretty (:&:)   = pretty "∧"
  pretty (:|:)   = pretty "∨"
  pretty (:~&:)  = pretty "⊼"
  pretty (:~|:)  = pretty "⊽"
  pretty (:<~>:) = pretty "⊕"
