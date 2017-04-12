-- | Data.TSTP.V module.
-- Adapted from https://github.com/agomezl/tstp2agda.

module Data.TSTP.V
  ( V
    ( V )
  ) where

------------------------------------------------------------------------------

import Athena.Utils.PrettyPrint  ( Pretty ( pretty ) )
import Athena.Translation.Utils  ( stdName )

------------------------------------------------------------------------------

-- The following code is from:
-- https://github.com/DanielSchuessler/logic-TPTP
-- and is used with the propose of reuses his
-- alex/happy parser in a more simple way

-- | Variable
newtype V = V String
          deriving (Eq, Ord, Read, Show)

instance Pretty V where
  pretty (V a) = pretty . stdName $ a
