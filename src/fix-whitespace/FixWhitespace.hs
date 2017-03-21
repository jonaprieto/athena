-- Adapted from Agda sources.

{-# LANGUAGE UnicodeSyntax #-}

module Main ( main ) where

import Control.Monad                  ( liftM, when )
import Data.Char as Char              ( isSpace )
import Data.Text                      ( Text )
import qualified Data.Text as Text    ( dropWhileEnd, lines, null, unlines )
import qualified Data.Text.IO as Text ( hGetContents, hPutStr ) -- Strict IO.
import System.Directory               ( getCurrentDirectory )
import System.Environment             ( getArgs )
import System.Exit                    ( exitFailure )

import System.FilePath.Find
  ( (||?)
  , (==?)
  , extension
  , fileName
  , find
  , FindClause
  , RecursionPredicate
 )

import System.IO
  ( hPutStr
  , hPutStrLn
  , IOMode(ReadMode, WriteMode)
  , stderr
  , withFile
  )

------------------------------------------------------------------------------
-- Configuration parameters.

extensions ∷ [String]
extensions =
  [ ".agda"
  , ".cabal"
  , ".el"
  , ".java"
  , ".hs"
  , ".hs-boot"
  , ".md"
  , ".smt"
  , ".smt2"
  , ".tex"
  , ".thy"
--  , ".test"
  , ".tptp"
  , ".txt"
  , ".v"
  , ".x"
  , ".y"
  ]

-- ASR (16 June 2014). In test/succeed/LineEndings/ we test that Agda
-- can handle various kinds of whitespace (pointed out by Nils), so we
-- exclude this directory.
excludedDirs :: [String]
excludedDirs =
 ["_darcs", ".git", "dist", "LineEndings", "MAlonzo", "std-lib"]

-- Auxiliary functions.

filesFilter :: FindClause Bool
filesFilter = foldr1 (||?) $ map (extension ==?) extensions

-- ASR (12 June 2014). Adapted from the examples of fileManip 0.3.6.2.
--
-- A recursion control predicate that will avoid recursing into the
-- @excludeDirs@ directories list.
nonRCS :: RecursionPredicate
nonRCS = (`notElem` excludedDirs) `liftM` fileName

-- Modes.

data Mode
  = Fix    -- ^ Fix whitespace issues.
  | Check  -- ^ Check if there are any whitespace issues.
    deriving Eq

main ∷ IO ()
main = do
  args ← getArgs
  mode ← case args of
    []          → return Fix
    ["--check"] → return Check
    _           → hPutStr stderr usage >> exitFailure

  dir <- getCurrentDirectory
  changes <- mapM (fix mode) =<< find nonRCS filesFilter dir

  when (or changes && mode == Check) exitFailure

-- | Usage info.

usage ∷ String
usage = unlines
  [ "fix-agda-whitespace: Fixes whitespace issues."
  , ""
  , "Usage: fix-agda-whitespace [--check]"
  , ""
  , "This program should be run in the base directory."
  , ""
  , "By default the program does the following for every"
  , list extensions ++ " file under the current directory:"
  , "* Removes trailing whitespace."
  , "* Removes trailing lines containing nothing but whitespace."
  , "* Ensures that the file ends in a newline character."
  , ""
  , "With the --check flag the program does not change any files,"
  , "it just checks if any files would have been changed. In this"
  , "case it returns with a non-zero exit code."
  , ""
  , "Background: Agda was reported to fail to compile on Windows"
  , "because a file did not end with a newline character (Agda"
  , "uses -Werror)."
  ]

  where
  list ∷ [String] → String
  list [x]      = x
  list [x, y]   = x ++ " and " ++ y
  list (x : xs) = x ++ ", " ++ list xs
  list _        = error "Impossible list"

-- | Fix a file. Only performs changes if the mode is 'Fix'. Returns
-- 'True' iff any changes would have been performed in the 'Fix' mode.

fix ∷ Mode → FilePath → IO Bool
fix mode f = do
  new ← withFile f ReadMode $ \h → do
    s ← Text.hGetContents h
    let s' = transform s
    return $ if s' == s then Nothing else Just s'
  case new of
    Nothing → return False
    Just s  → do
      hPutStrLn stderr $
        "Whitespace violation " ++
        (if mode == Fix then "fixed" else "detected") ++
        " in " ++ f ++ "."
      when (mode == Fix) $
        withFile f WriteMode $ \h → Text.hPutStr h s
      return True

-- | Transforms the contents of a file.
transform ∷ Text → Text
transform =
  Text.unlines .
  removeFinalEmptyLinesExceptOne .
  map removeTrailingWhitespace .
  Text.lines

  where
  removeFinalEmptyLinesExceptOne ∷ [Text] → [Text]
  removeFinalEmptyLinesExceptOne = reverse . dropWhile1 Text.null . reverse

  removeTrailingWhitespace ∷ Text → Text
  removeTrailingWhitespace = Text.dropWhileEnd Char.isSpace

-- | 'dropWhile' except keep the first of the dropped elements
dropWhile1 :: (a → Bool) → [a] → [a]
dropWhile1 _ [] = []
dropWhile1 p (x : xs)
  | p x       = x : dropWhile p xs
  | otherwise = x : xs
