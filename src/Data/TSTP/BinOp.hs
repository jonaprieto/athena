
-- | Data.TSTP.BinOp module.
-- Adapted from https://github.com/agomezl/tstp2agda.

{-# LANGUAGE UnicodeSyntax #-}

module Data.TSTP.BinOp where

------------------------------------------------------------------------------

-- | Binary formula connectives.
data BinOp = (:<=>:)  -- ^ ⇔ /Equivalence/
           | (:=>:)   -- ^ ⇒ /Implication/
           | (:<=:)   -- ^ ⇐ /Reverse Implication/
           | (:&:)    -- ^ ∧ /AND/
           | (:|:)    -- ^ ∨ /OR/
           | (:~&:)   -- ^ ⊼ /NAND/
           | (:~|:)   -- ^ ⊽ /NOR/
           | (:<~>:)  -- ^ ⊕ /XOR/
           deriving (Eq, Ord, Read)

instance Show BinOp where
  show (:<=>:) = "⇔"
  show (:=>:)  = "⇒"
  show (:<=:)  = "⇐"
  show (:&:)   = "∧"
  show (:|:)   = "∨"
  show (:~&:)  = "⊼"
  show (:~|:)  = "⊽"
  show (:<~>:) = "⊕"
