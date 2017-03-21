
-- | Monads utilities.

{-# LANGUAGE UnicodeSyntax #-}

module Utils.Monad
  ( die
  , failureMsg
  , stdout2file
  ) where

------------------------------------------------------------------------------

import Utils.PrettyPrint  ( Doc, prettyShow )

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
  getProgName >>= \prg → hPutStrLn stderr $ prg ++ ": " ++ prettyShow err

-- | Exit with an error message.
die ∷ Doc → IO a
die err = failureMsg err >> exitFailure

-- | Redirect all stdout output into a file or do nothing (in case of
-- 'Nothing')
stdout2file ∷ Maybe FilePath → IO ()
stdout2file Nothing  = return ()
stdout2file (Just o) = openFile o WriteMode >>= flip hDuplicateTo stdout
