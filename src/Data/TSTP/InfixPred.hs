-- | Data.TSTP.InfixPred module.
-- Adapted from https://github.com/agomezl/tstp2agda.

{-# LANGUAGE UnicodeSyntax #-}

module Data.TSTP.InfixPred
  ( InfixPred
    ( (:=:)
    , (:!=:)
    )
  )
  where

------------------------------------------------------------------------------

import Athena.Utils.PrettyPrint ( Pretty(pretty) )

------------------------------------------------------------------------------

-- | Infix connectives of the form /Term → Term → Formula/.
data InfixPred = (:=:)  -- ^ =
               | (:!=:) -- ^ ≠
               deriving (Eq, Ord, Read, Show)

instance Pretty InfixPred where
  pretty (:=:)  = pretty "="
  pretty (:!=:) = pretty "≠"
