
-- | Main module

{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Main ( main ) where

------------------------------------------------------------------------------

import Athena.Options
  (
    Options
    ( optHelp
    , optInputFile
    , optVersion
    )
    , printUsage
    , processOptions
    )

import Athena.Translation.Core   ( mainCore )

import Athena.Utils.Monad        ( die )
import Athena.Utils.PrettyPrint  ( text )
import Athena.Utils.Version      ( progNameVersion )

import System.Environment        ( getArgs )
import System.Exit               ( exitSuccess )


------------------------------------------------------------------------------

main ∷ IO ()
main = do
  args ← getArgs
  opts ← case processOptions args of
    Left err → die err
    Right o  → return o

  if  | optHelp opts → printUsage >> exitSuccess

      | optVersion opts → do
        v ← progNameVersion
        putStrLn v  >> exitSuccess

      | otherwise → do
          _ ← case optInputFile opts of
            Nothing → die $ text "missing input file (try --help)"
            Just f  → return f
          mainCore opts
