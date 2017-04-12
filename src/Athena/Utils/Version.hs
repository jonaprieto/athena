-- | Athena.Utils.Version module.
-- Utilities related to representation of versions.

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Athena.Utils.Version ( progNameVersion ) where

------------------------------------------------------------------------------

import Athena.Utils.CommitVersion ( getVersion )

import Data.Char                  ( toUpper )
import Data.Version               ( showVersion )

import Paths_athena               ( version )
import System.Environment         ( getProgName )

------------------------------------------------------------------------------

toUpperFirst ∷ String → String
toUpperFirst []       = []
toUpperFirst (x : xs) = toUpper x : xs

-- | Return program name and version information.
progNameVersion ∷ IO String
progNameVersion = do
 commitversion ← getVersion version
 progName ← getProgName
 return $ toUpperFirst progName ++ " version " ++ showVersion commitversion
