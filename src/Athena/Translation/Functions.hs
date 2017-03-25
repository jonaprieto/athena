
-- | Athena.Translation.Functions module.

{-# OPTIONS -fno-warn-missing-signatures  #-}
{-# LANGUAGE UnicodeSyntax                #-}

module Athena.Translation.Functions
   ( getAxioms
   , getConjeture
   , getRefutes
   , getSubGoals
   , printAxiom
   , printAxioms
   , printConjecture
   , printPreamble
   , printSubGoals
   , printVar
   , printVars
   ) where

------------------------------------------------------------------------------

import Athena.Translation.Utils ( stdName )

import Data.List                ( isPrefixOf, intercalate )

import Data.TSTP
  ( F    ( name, role, formula )
  , Role ( Axiom, Conjecture )
  )

import Data.TSTP.V              ( V(..) )

------------------------------------------------------------------------------

-- Vars.

printVar ∷ V → Int → String
printVar f n =
  intercalate "\n"
    [ show f ++ " : Prop"
    , show f ++ varStr
    ]
    where
      varStr ∷ String
      varStr = case show f of
        "$true"  → " = ⊤"
        "$false" → " = ⊥"
        _        → " = Var (# " ++ show n ++ ")"

printVars ∷ [V] → Int → IO String
printVars [] _       = return ""
printVars (f : fs) n = do
  putStrLn $ printVar f n ++ "\n"
  printVars fs (n+1)

-- Axioms.

-- | Extract axioms from a list of formulae.
getAxioms ∷ [F] → [F]
getAxioms = filter ((==) Axiom . role)

-- | Print an axiom.
printAxiom ∷ F → String
printAxiom f =
  let axiom  = stdName $ name f
  in concat
    [  axiom , " : Prop\n"
    ,  axiom , " = " ,  show (formula f) , "\n"
    ]

printAxioms ∷ [F] → IO ()
printAxioms []  = return ()
printAxioms [a] = do
  putStrLn "-- Axiom"
  putStrLn $ printAxiom a ++ "\n"
printAxioms as  = do
  putStrLn "-- Axioms"
  putStrLn . intercalate "\n\n" $ map printAxiom as
  putStrLn ""

-- Conjecture.

-- | Try to extract a conjecture from a list of formulae and checks
-- for uniqueness.
getConjeture ∷ [F] → Maybe F
getConjeture rules =
  case filter ((==) Conjecture . role) rules of
    [l] → Just l
    _   → Nothing

printConjecture ∷ F → IO ()
printConjecture f = putStrLn $
  concat
    [ "-- Conjecture\n"
    , printAxiom f , "\n"
    ]

-- Subgoals.

-- | Extract subgoals from a list of formulae.
getSubGoals ∷ [F] → [F]
getSubGoals = filter (isPrefixOf "subgoal" . name)

printSubGoals ∷ [F] → IO ()
printSubGoals []       = return ()
printSubGoals subgoals = putStrLn $
  concat
    [ "-- Subgoal", if length subgoals < 2 then "" else "s" , "\n"
    , intercalate "\n\n" (map printAxiom subgoals)
    ]

-- | Extract refuting steps from a list of formulae.
getRefutes ∷ [F] → [F]
getRefutes = filter (isPrefixOf "refute"  . name)

printPreamble ∷ Int → IO ()
printPreamble n = do
  putStrLn "\n-- tstp2agda proof\n"
  putStrLn $ "open import Data.FOL.Deep " ++ show n ++" public"
  putStrLn $ "open import Data.FOL.Deep.ATP.Metis " ++ show n ++ " public\n"
