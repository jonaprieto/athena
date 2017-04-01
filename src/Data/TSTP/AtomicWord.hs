
-- | Data.TSTP.AtomicWord module.
-- Adapted from https://github.com/agomezl/tstp2agda.

{-# LANGUAGE UnicodeSyntax #-}

module Data.TSTP.AtomicWord where

------------------------------------------------------------------------------

import Athena.Utils.PrettyPrint ( Pretty(pretty) )

------------------------------------------------------------------------------

newtype AtomicWord = AtomicWord String
    deriving (Eq, Ord, Read, Show)

instance Pretty AtomicWord where
  pretty (AtomicWord "$false") = pretty "⊥"
  pretty (AtomicWord a)        = pretty a
