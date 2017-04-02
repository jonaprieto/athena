
-- | Athena.Translation.Core module.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Athena.Translation.Core ( mainCore ) where

------------------------------------------------------------------------------

import Athena.Translation.Functions
  ( docAxioms
  , docConjecture
  , docHeader
  , docImports
  , docPremises
  , docSubgoals
  , docVars
  , getAxioms
  , getConjeture
  , getRefutes
  , getSubgoals
  )
import Athena.Translation.AgdaFile
  ( AgdaFile
     ( AgdaFile
     , fileAxioms
     , fileConjecture
     , fileInfo
     , filePremises
     , fileSubgoals
     , fileTree
     , fileVariables
     )
  )
import Athena.Utils.PrettyPrint  ( hPutDoc, Doc, pretty, comment )
import Athena.Options
  ( Options
    ( optInputFile
    , optOutputFile
    )
  )
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
import Data.TSTP.V              ( V(..) )

import System.FilePath          ( replaceExtension )
import System.IO
  (
  hClose
  , IOMode(WriteMode)
  , openFile
  )

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

  let tree ∷ [ProofTree]
      tree = fmap (buildProofTree rulesMap) refutes

  let formulas ∷ [Formula]
      formulas = fmap formula tstp

  let filename :: FilePath
      filename =
        fromMaybe
          (replaceExtension (fromJust (optInputFile opts)) ".agda")
          (optOutputFile opts)

  -- Agda file.
  agdaFile <- openFile filename WriteMode

  -- Header.
  header :: Doc <- docHeader opts
  hPutDoc agdaFile header

  let problem :: AgdaFile
      problem = AgdaFile
        { fileAxioms     = getAxioms tstp
        , fileConjecture = conj
        , fileInfo       = rulesMap
        , filePremises   = getAxioms tstp
        , fileSubgoals   = getSubgoals tstp
        , fileTree       = tree
        , fileVariables  = getFreeVars formulas
        }

  hPutDoc agdaFile (pretty problem)

  -- Close the file.
  hClose agdaFile
