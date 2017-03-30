
-- | Data.TSTP.Quant module.
-- Adapted from https://github.com/agomezl/tstp2agda.

{-# LANGUAGE UnicodeSyntax #-}

module Data.TSTP.Quant where

------------------------------------------------------------------------------

-- | Quantifier specification.
data Quant = All    -- ^ ∀
           | Exists -- ^ ∃
           deriving (Eq, Ord, Show, Read)
