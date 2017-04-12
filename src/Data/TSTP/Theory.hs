-- | Data.TSTP.Theory module.
-- Adapted from https://github.com/agomezl/tstp2agda.

module Data.TSTP.Theory
  ( Theory
    ( AC
    , Equality
    )
  ) where

------------------------------------------------------------------------------

data Theory = AC | Equality
            deriving (Eq, Ord, Read, Show)
