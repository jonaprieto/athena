
-- | Athena.Translation.Core module.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Athena.Translation.Core ( mainCore ) where

------------------------------------------------------------------------------

import Athena.Translation.Functions
  (
    getAxioms
  , getConjeture
  -- , getRefutes
  , getSubgoals
  , docAxioms
  , docConjecture
  , docHeader
  -- , printPremises
  -- , printProof
  , docSubgoals
  , docVars
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

-- import Data.Proof
--   ( buildProofMap
--   , buildProofTree
--   , ProofMap
--   , ProofTree
--   )
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

  let subgoals ∷ [F]
      subgoals = getSubgoals tstp

  -- let refutes ∷ [F]
  --     refutes = getRefutes tstp

  let axioms ∷ [F]
      axioms = getAxioms tstp

  let conj ∷ F
      conj = fromMaybe
        (error "Couldn't find a conjecture, or it was not unique")
        (getConjeture tstp)

  -- let rulesMap ∷ ProofMap
  --     rulesMap = buildProofMap tstp

  -- let rulesTrees ∷ [ProofTree]
  --     rulesTrees = fmap (buildProofTree rulesMap) refutes

  let formulas ∷ [Formula]
      formulas = fmap formula tstp

  let freevars ∷ [V]
      freevars = getFreeVars formulas

  let filename :: FilePath
      filename =
        fromMaybe
          (replaceExtension (fromJust (optInputFile opts)) ".agda")
          (optOutputFile opts)

  -- Agda file.
  agdaFile <- openFile filename WriteMode

  -- Header.
  header :: Doc <- docHeader opts (length freevars)
  hPutDoc agdaFile header

  -- Variables.
  hPutDoc agdaFile (docVars freevars)

  -- Axioms.
  hPutDoc agdaFile (docAxioms axioms)

  -- Conjecture.
  hPutDoc agdaFile (docConjecture conj)

  -- Subgoals.
  hPutDoc agdaFile (docSubgoals subgoals)

  -- Close the file.
  hClose agdaFile
