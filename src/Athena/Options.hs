-- | Athena.Options module.

{-# LANGUAGE UnicodeSyntax #-}

module Athena.Options
  ( options
  , OM
  , Options
    ( Options --Improve Haddock information.
    , optDebug
    , optHelp
    , optInputFile
    , optOutputFile
    , optVersion
    )
  , printUsage
  , processOptions
  ) where

------------------------------------------------------------------------------

import Athena.Utils.PrettyPrint  ( Doc, Pretty(pretty), squotes, (<>) )

import Data.List                 ( foldl' )

import System.Console.GetOpt
  ( ArgDescr ( NoArg, ReqArg )
  , ArgOrder ( ReturnInOrder )
  , getOpt
  , OptDescr ( Option )
  , usageInfo
  )
import System.Environment        ( getProgName )

------------------------------------------------------------------------------

-- | Program command-line options.
data Options = Options
  { optDebug      ∷ Bool
  , optHelp       ∷ Bool
  , optInputFile  ∷ Maybe FilePath
  , optOutputFile ∷ Maybe FilePath
  , optVersion    ∷ Bool
  }

defaultOptions ∷ Options
defaultOptions = Options
  { optDebug      = False
  , optHelp       = False
  , optInputFile  = Nothing
  , optOutputFile = Nothing
  , optVersion    = False
  }

-- | 'Options' monad.
type OM = Options → Either Doc Options

debugOpt ∷ OM
debugOpt opts = Right opts { optDebug = True }

helpOpt ∷ OM
helpOpt opts = Right opts { optHelp = True }

inputFileOpt ∷ FilePath → OM
inputFileOpt file opts =
  case optInputFile opts of
    Nothing → Right opts { optInputFile = Just file }
    Just _  → Left $ pretty "only one input file allowed"

outputFileOpt ∷ FilePath → OM
outputFileOpt file opts =
  case optOutputFile opts of
    Nothing → Right opts { optOutputFile = Just file }
    Just _  → Left $ pretty "only one input file allowed"

versionOpt ∷ OM
versionOpt opts = Right opts { optVersion = True }

-- | Description of the command-line 'Options'.
options ∷ [OptDescr OM]
options =
  [ Option []    ["debug"] (NoArg debugOpt)
      ""
  , Option ['h'] ["help"] (NoArg helpOpt)
      "Prints help message"
  , Option []    ["version"] (NoArg versionOpt)
      "Show version number"
  ]

usageHeader ∷ String → String
usageHeader prgName = "Usage: " ++ prgName ++ " [OPTIONS] FILE\n"

-- | Print usage information.
printUsage ∷ IO ()
printUsage = do
  progName ← getProgName
  putStrLn $ usageInfo (usageHeader progName) options

processOptionsHelper ∷ [String] → (FilePath → OM) → OM
processOptionsHelper argv f defaults =
  case getOpt (ReturnInOrder f) options argv of
    (o, _, [])   → foldl' (>>=) (return defaults) o
    (_, _, errs) → Left . pretty $ unlines errs

-- | Processing the command-line 'Options'.
processOptions ∷ [String] → Either Doc Options
processOptions argv = processOptionsHelper argv inputFileOpt defaultOptions
