-- | Main module

{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Main (main) where

------------------------------------------------------------------------------

import Options
  (
    Options
    ( optHelp
    , optInputFile
    , optVersion
    )
    , printUsage
    , processOptions
    )

import System.Environment ( getArgs )
import System.Exit        ( exitSuccess )

import Translation.Core   ( mainCore )

import Utils.Monad        ( die )
import Utils.PrettyPrint  ( text )
import Utils.Version      ( progNameVersion )

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
