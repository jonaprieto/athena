
-- | Utilities related to representation of versions.

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Athena.Utils.Version ( progNameVersion ) where

------------------------------------------------------------------------------

import Data.Char          ( toUpper )
import Data.Version       ( showVersion )

import Paths_athena       ( version )
import System.Environment ( getProgName )

------------------------------------------------------------------------------

toUpperFirst ∷ String → String
toUpperFirst []       = []
toUpperFirst (x : xs) = toUpper x : xs

-- Uncomment this in order to use ghcid with Main.hs

--progNameVersion ∷ IO String
--progNameVersion = return "dev"

-- | Return program name and version information.
progNameVersion ∷ IO String
progNameVersion = do
 progName ← getProgName
 return $ toUpperFirst progName ++ " version " ++ showVersion version
