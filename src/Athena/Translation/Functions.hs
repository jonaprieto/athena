
-- | T2A module

{-# OPTIONS -fno-warn-missing-signatures  #-}
{-# LANGUAGE UnicodeSyntax                #-}

module Translation.Functions (
   -- * Getters
     getSubGoals
   , getAxioms
   , getConjeture
   , getRefutes
   , printPreamble
   ) where

------------------------------------------------------------------------------

import Data.List ( isPrefixOf )

import Data.TSTP
  ( F ( name, role )
  , Role ( Axiom, Conjecture )
  )
------------------------------------------------------------------------------

-- | Extract subgoals from a list of formulae.
getSubGoals ∷ [F] → [F]
getSubGoals = filter (isPrefixOf "subgoal" . name)

-- | Extract refuting steps from a list of formulae.
getRefutes ∷ [F] → [F]
getRefutes = filter (isPrefixOf "refute"  . name)

-- | Extract axioms from a list of formulae.
getAxioms ∷ [F] → [F]
getAxioms = filter ((==) Axiom . role)

-- | Try to extract a conjecture from a list of formulae and checks
-- for uniqueness.
getConjeture ∷ [F] → Maybe F
getConjeture rules =
  case filter ((==) Conjecture . role) rules of
    [l] → Just l
    _   → Nothing

printPreamble ∷ Int → IO ()
printPreamble n = do
  putStrLn "\n-- tstp2agda proof\n"
  putStrLn $ "open import Data.FOL.Deep " ++ show n ++" public"
  putStrLn $ "open import Data.FOL.Deep.ATP.Metis " ++ show n ++ " public\n"
