
-- | Athena.Translation.Core module.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Athena.Translation.Core ( mainCore ) where

------------------------------------------------------------------------------

import Athena.Translation.Functions
  ( getAxioms
  , getConjeture
  , getRefutes
  , getSubGoals
  , printAxioms
  , printConjecture
  , printPreamble
  , printPremises
  , printProof
  , printSubGoals
  , printVars
  )
import Athena.Utils.Monad       ( stdout2file )
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
------------------------------------------------------------------------------

mainCore ∷ Options → IO ()
mainCore opts = do

  tstp ∷ [F] ← parseFile . fromJust $ optInputFile opts

  let subgoals ∷ [F]
      subgoals = getSubGoals tstp

  let refutes ∷ [F]
      refutes = getRefutes tstp

  let axioms ∷ [F]
      axioms = getAxioms tstp

  let conj ∷ F
      conj = fromMaybe
        (error "Couldn't find a conjecture, or it was not unique")
        (getConjeture tstp)

  let rulesMap ∷ ProofMap
      rulesMap = buildProofMap tstp

  let rulesTrees ∷ [ProofTree]
      rulesTrees = fmap (buildProofTree rulesMap) refutes

  stdout2file $ Just (fromMaybe
    (replaceExtension (fromJust (optInputFile opts)) ".agda")
    (optOutputFile opts)
    )

  let formulas ∷ [Formula]
      formulas = fmap formula tstp

  let freevars ∷ [V]
      freevars = getFreeVars formulas

  printPreamble $ length freevars

  putStrLn "-- Vars"
  _ <- printVars freevars 0

  printAxioms axioms
  printPremises axioms
  printConjecture conj
  printSubGoals subgoals
  printProof axioms subgoals conj rulesMap rulesTrees
