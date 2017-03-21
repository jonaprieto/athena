
-- | Data.TSTP.V module

module Data.TSTP.V where

------------------------------------------------------------------------------

-- The following code is from:
-- https://github.com/DanielSchuessler/logic-TPTP
-- and is used with the propose of reuses his
-- alex/happy parser in a more simple way

-- | Variable
newtype V = V String
    deriving (Eq, Ord, Read)

instance Show V where
    show (V a) = a
