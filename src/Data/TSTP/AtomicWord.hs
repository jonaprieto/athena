
-- | Data.TSTP.AtomicWord module.
-- Adapted from https://github.com/agomezl/tstp2agda.

{-# LANGUAGE UnicodeSyntax #-}

module Data.TSTP.AtomicWord where

------------------------------------------------------------------------------

newtype AtomicWord = AtomicWord String
    deriving (Eq, Ord, Read)

instance Show AtomicWord where
  show (AtomicWord "$false") = "⊥"
  show (AtomicWord a) = a
