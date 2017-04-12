-- | Data.TSTP.Quant module.
-- Adapted from https://github.com/agomezl/tstp2agda.

{-# LANGUAGE UnicodeSyntax #-}

module Data.TSTP.Quant
  ( Quant
    ( All
    , Exists
    )
  ) where

------------------------------------------------------------------------------

import Athena.Utils.PrettyPrint ( Pretty(pretty) )

------------------------------------------------------------------------------

-- | Quantifier specification.
data Quant = All    -- ^ ∀
           | Exists -- ^ ∃
           deriving (Eq, Ord, Read, Show)

instance Pretty Quant where
  pretty All    = pretty "∀"
  pretty Exists = pretty "∃"
