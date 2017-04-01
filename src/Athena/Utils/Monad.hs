
-- | Athena.Utils.Monads utilities.
-- Adapted from https://github.com/asr/apia.

{-# LANGUAGE UnicodeSyntax #-}

module Athena.Utils.Monad
  ( die
  , failureMsg
  , stdout2file
  ) where

------------------------------------------------------------------------------

import Athena.Utils.PrettyPrint  ( Doc, pretty , (<>), hPutDoc )

import System.Environment ( getProgName )
import System.Exit        ( exitFailure )

import GHC.IO.Handle      ( hDuplicateTo )
import System.IO
  ( hPutStrLn
  , stderr
  , IOMode(WriteMode)
  , stdout
  , openFile
  )

------------------------------------------------------------------------------

-- | Failure message.
failureMsg ∷ Doc → IO ()
failureMsg err =
  getProgName >>= \prg → hPutDoc stderr $ pretty prg <> pretty ": " <> err

-- | Exit with an error message.
die ∷ Doc → IO a
die err = failureMsg err >> exitFailure

-- | Redirect all stdout output into a file or do nothing (in case of
-- 'Nothing')
stdout2file ∷ Maybe FilePath → IO ()
stdout2file Nothing  = return ()
stdout2file (Just o) = openFile o WriteMode >>= flip hDuplicateTo stdout
