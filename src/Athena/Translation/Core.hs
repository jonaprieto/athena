-- | Athena.Translation.Core module.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Athena.Translation.Core ( mainCore ) where

------------------------------------------------------------------------------

import Athena.Translation.AgdaFile
  ( AgdaFile
     ( AgdaFile
     , fileAxioms
     , fileConjecture
     , fileDict
     , fileName
     , filePremises
     , fileSubgoals
     , fileTrees
     , fileVariables
     )
  , docHeader
  , getAxioms
  , getConjeture
  , getRefutes
  , getSubgoals
  )
import Athena.Utils.PrettyPrint ( hPutDoc, Doc, pretty )
import Athena.Options           ( Options ( optInputFile, optOutputFile ) )
import Athena.TSTP              ( parseFile )
import Data.Maybe               ( fromJust, fromMaybe )

import Data.Proof
  ( buildProofMap
  , buildProofTree
  , ProofMap
  , ProofTree
  )
import Data.TSTP
  ( F(..)
  , Formula(..)
  )
import Data.TSTP.Formula        ( getFreeVars )

import System.FilePath          ( replaceExtension )
import System.IO                ( hClose, IOMode( WriteMode ), openFile )

------------------------------------------------------------------------------

mainCore ∷ Options → IO ()
mainCore opts = do

  tstp ∷ [F] ← parseFile . fromJust $ optInputFile opts

  let conj ∷ F
      conj = fromMaybe
        (error "Couldn't find a conjecture, or it was not unique")
        (getConjeture tstp)

  let rulesMap ∷ ProofMap
      rulesMap = buildProofMap tstp

  let refutes ∷ [F]
      refutes = getRefutes tstp

  let trees ∷ [ProofTree]
      trees = fmap (buildProofTree rulesMap) refutes

  let formulas ∷ [Formula]
      formulas = fmap formula tstp

  let filename :: FilePath
      filename =
        fromMaybe
          (replaceExtension (fromJust (optInputFile opts)) ".agda")
          (optOutputFile opts)

  -- Agda file.
  hAgdaFile <- openFile filename WriteMode

  -- Header.
  header :: Doc <- docHeader opts
  hPutDoc hAgdaFile header

  let problem :: AgdaFile
      problem = AgdaFile
        { fileAxioms     = getAxioms tstp
        , fileConjecture = conj
        , fileDict       = rulesMap
        , fileName       = filename
        , filePremises   = getAxioms tstp
        , fileSubgoals   = getSubgoals tstp
        , fileTrees      = trees
        , fileVariables  = getFreeVars formulas
        }

  hPutDoc hAgdaFile (pretty problem)

  -- Close the file.
  hClose hAgdaFile
