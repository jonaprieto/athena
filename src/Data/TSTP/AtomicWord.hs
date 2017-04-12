-- | Data.TSTP.AtomicWord module.
-- Adapted from https://github.com/agomezl/tstp2agda.

{-# LANGUAGE UnicodeSyntax #-}

module Data.TSTP.AtomicWord
  ( AtomicWord ( AtomicWord )
  ) where

------------------------------------------------------------------------------

import Athena.Utils.PrettyPrint ( Pretty(pretty) )
import Athena.Translation.Utils ( stdName )

------------------------------------------------------------------------------

newtype AtomicWord = AtomicWord String
    deriving (Eq, Ord, Read, Show)

instance Pretty AtomicWord where
  pretty (AtomicWord "$false") = pretty "⊥"
  pretty (AtomicWord a)        = pretty . stdName $ a
