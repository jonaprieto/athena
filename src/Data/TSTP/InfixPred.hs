
-- | Data.TSTP.InfixPred module.
-- Adapted from https://github.com/agomezl/tstp2agda.

{-# LANGUAGE UnicodeSyntax #-}

module Data.TSTP.InfixPred where

------------------------------------------------------------------------------

-- | Infix connectives of the form /Term → Term → Formula/.
data InfixPred = (:=:)  -- ^ =
               | (:!=:) -- ^ ≠
               deriving (Eq, Ord, Read)

instance Show InfixPred where
  show (:=:)  = "="
  show (:!=:) = "≠"
