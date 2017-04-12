-- | Commit version.
-- Adapted from Apia.Utils.CommitVersion.hs
-- http://github.com/asr/apia.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

-- The constructor @versionTagsq (see GHC ticket #2496) is deprecated
-- (at least in GHC 8.0.1). See Issue #83.
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Athena.Utils.CommitVersion ( getVersion ) where

------------------------------------------------------------------------------

import Control.Exception ( IOException, try )
import Data.Version      ( Version ( versionTags ) )
import System.Exit       ( ExitCode ( ExitSuccess ) )
import System.Process    ( readProcessWithExitCode )

------------------------------------------------------------------------------
-- | If inside a `git` repository, then @'getVersion' x@ will return
-- @x@ plus the hash of the top commit used for
-- compilation. Otherwise, only @x@ will be returned.
getVersion ∷ Version → IO Version
getVersion version = do
  commit ∷ Maybe String ← commitInfo
  case commit of
    Nothing  → return version
    Just rev → return $ gitVersion version rev

tryIO ∷ IO a → IO (Either IOException a)
tryIO = try

commitInfo ∷ IO (Maybe String)
commitInfo = do
  res ← tryIO $
    readProcessWithExitCode "git" ["log", "--format=%h", "-n", "1"] ""
  case res of
    Right (ExitSuccess, hash, _) → do
      (_, _, _) ← readProcessWithExitCode "git" ["diff", "--quiet"] ""
      return $ Just (init hash)
    _ → return Nothing

gitVersion ∷ Version → String → Version
gitVersion version hash = version { versionTags = [take 7 hash] }
