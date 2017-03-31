
-- | Data.TSTP.F module.
-- Adapted from https://github.com/agomezl/tstp2agda.

{-# LANGUAGE UnicodeSyntax #-}

module Data.TSTP.F where

------------------------------------------------------------------------------

import Athena.Utils.PrettyPrint ( Pretty(pretty) )

import Data.TSTP.Formula ( Formula(..) )
import Data.TSTP.Role    ( Role(..) )
import Data.TSTP.Source  ( Source(..) )

import qualified Text.Show.Pretty as Pr

------------------------------------------------------------------------------

-- | Main formula type, it contains all the elements and information
-- of a TSTP formula definition. While 'name', 'role', and 'formula'
-- are self-explanatory, 'source' is a messy meta-language in itself,
-- different ATPs may embed different amounts of information in it.
data F = F
  { formula ∷ Formula
  , name    ∷ String
  , role    ∷ Role
  , source  ∷ Source
  }
  deriving (Eq, Ord, Read, Show)

instance Pretty F where
  pretty = Pr.ppDoc
