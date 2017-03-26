
-- | Data.TSTP.BinOp module.

{-# LANGUAGE UnicodeSyntax #-}

module Data.TSTP.BinOp where

------------------------------------------------------------------------------

-- | Binary formula connectives.
data BinOp = (:<=>:)  -- ^ ↔ /Equivalence/
           | (:=>:)   -- ^ → /Implication/
           | (:<=:)   -- ^ ← /Reverse Implication/
           | (:&:)    -- ^ ∧ /AND/
           | (:|:)    -- ^ ∨ /OR/
           | (:~&:)   -- ^ ⊼ /NAND/
           | (:~|:)   -- ^ ⊽ /NOR/
           | (:<~>:)  -- ^ ⊕ /XOR/
           deriving (Eq, Ord, Read)

instance Show BinOp where
  show (:<=>:) = "↔"
  show (:=>:)  = "→"
  show (:<=:)  = "←"
  show (:&:)   = "∧"
  show (:|:)   = "∨"
  show (:~&:)  = "⊼"
  show (:~|:)  = "⊽"
  show (:<~>:) = "⊕"
