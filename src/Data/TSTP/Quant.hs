
-- | Data.TSTP.Quant module.

{-# LANGUAGE UnicodeSyntax #-}

module Data.TSTP.Quant where

------------------------------------------------------------------------------

-- | Quantifier specification.
data Quant = All    -- ^ ∀
           | Exists -- ^ ∃
           deriving (Eq, Ord, Show, Read)
