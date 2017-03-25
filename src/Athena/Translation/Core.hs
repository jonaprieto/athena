
-- | Athena.Translation.Core module

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Athena.Translation.Core ( mainCore ) where

------------------------------------------------------------------------------

import Athena.Translation.Functions
  ( getAxioms
  , getConjeture
  , getRefutes
  , getSubGoals
  , printAxiom
  , printAxioms
  , printConjecture
  , printPreamble
  -- , printSubGoals
  -- , printVar
  , printVars
  )
import Athena.Translation.Rules
  (
    -- atpCanonicalize
  -- , atpClausify
    atpConjunct
  -- , atpNegate
  , atpResolve
  -- , atpSimplify
  -- , atpStrip
  )
import Athena.Translation.Utils ( Ident, getIdent, stdName)
import Athena.Utils.Monad       ( stdout2file )
import Athena.Options
  ( Options
    ( optInputFile
    , optOutputFile
    )
  )
import Athena.TSTP              ( parseFile )

import Data.List
import qualified Data.Map as Map
import Data.Maybe               ( fromJust, fromMaybe, Maybe )

import Data.Proof
  ( buildProofMap
  , buildProofTree
  , ProofMap
  , ProofTree
  , ProofTreeGen(..)
  )
import Data.TSTP
  ( F(..)
  , Formula(..)
  , Rule(..)
  , Role(..)
  )
import Data.TSTP.Formula        ( getFreeVars )
import Data.TSTP.V              ( V(..) )

------------------------------------------------------------------------------

debug :: Bool
debug = True

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

  stdout2file $ optOutputFile opts

  let formulas ∷ [Formula]
      formulas = fmap formula tstp

  let freevars ∷ [V]
      freevars = getFreeVars formulas

  printPreamble (length freevars)

  putStrLn "-- Vars"
  _ <- printVars freevars 0

  printAxioms axioms

  printPremises axioms

  printConjecture conj

  printSubGoals' subgoals

  printProof axioms subgoals conj rulesMap rulesTrees



printPremises ∷ [F] → IO ()
printPremises premises = do
  putStrLn $ "-- Premise" ++ (if length premises < 2 then "" else "s")
  putStrLn "Γ : Ctxt"
  case premises of
    []  → putStrLn "Γ = ∅"
    [p] → putStrLn $ "Γ = [ " ++ name p ++ " ]"
    ps  → putStrLn $ "Γ = ∅ , " ++ intercalate " , " (map name ps)
  putStrLn ""


printSubGoals' ∷ [F] → IO ()
printSubGoals' [] = return ()
printSubGoals' subgoals = putStrLn $
  concat
    [ "-- Subgoal", if length subgoals < 2 then "" else "s" , "\n"
    , intercalate "\n\n" (map printAxiom subgoals)
    ]

printProof ∷ [F] → [F] → F → ProofMap → [ProofTree] → IO ()
printProof _ _  _ _ [] = return ()
printProof axioms subgoals goal rmap rtree = do
  putStrLn "-- Metis Proof."
  printProofSubgoal 0 axioms subgoals goal rmap rtree
  printProofGoal subgoals goal rmap rtree

printProofSubgoal ∷ Int → [F] → [F] → F → ProofMap → [ProofTree] → IO ()
printProofSubgoal _ _ _ _ _ [] = return ()
printProofSubgoal no axioms subgoals goal rmap (tree:strees) = do
  let strNo       = stdName $ show no
  let proofName   = stdName $ "proof" ++ strNo
  let subgoalName = "subgoal" ++ strNo
  let proof ∷ String
      proof = concat
        [ proofName , " : Γ ⊢ " , subgoalName , "\n"
        , proofName , " =\n"
        , "  RAA $" , "\n"
        , printSteps subgoalName 2 [tree] rmap goal axioms
        ]
  putStrLn proof
  printProofSubgoal (no+1) axioms subgoals goal rmap strees

printInnerFormula ∷ Ident → ProofMap → String → String -> String
printInnerFormula n dict tag ctxt =
  if debug
    then do
      let fm ∷ Maybe F
          fm = Map.lookup tag dict
      let strFm ∷ String
          strFm = show . formula $ fromJust fm
      concat [ getIdent n , "-- " , ctxt , " ⊢ " , strFm ]
    else ""

printSteps ∷ String → Ident → [ProofTree] → ProofMap → F → [F] → String
printSteps sname n [Root Negate tag [Root Strip subgoalname _]] dict _ _ =
  concat
    [ getIdent n , "atp-strip $" , printInnerFormula 1 dict subgoalname "Γ" , "\n"
    , getIdent (n+1) , "assume {Γ = Γ} $" ,  printInnerFormula 1 dict tag "Γ" , "\n"
    , getIdent (n+2) , "atp-neg " , sname , "\n"
    ]

printSteps sname n [Root Simplify tag subtree] dict goal axioms =
  concat
    [ getIdent n , "atp-simplify $" , printInnerFormula 1 dict tag "Γ" , "\n"
    , getIdent (n+1) , "∧-intro\n"
    , andIntro (n+2) subtree
    ]
    where
      innerStep m step = concat
        [ getIdent m , "(\n"
        , printSteps sname m [step] dict goal axioms
        , getIdent m , ")\n"
        ]

      andIntro _ []     = ""
      andIntro m [x]    = printSteps sname m [x] dict goal axioms
      andIntro m [x,y]  = concatMap (innerStep m) [x , y]
      andIntro m (x:xs) = concat
        [ innerStep m x
        , getIdent m , "(\n"
        , getIdent m , "∧-intro\n"
        , andIntro (m+1) xs
        , getIdent m , ")\n"
        ]

printSteps sname n [Root Resolve tag ((left@(Root _ fTag _)):(right@(Root _ gTag _)):_)] dict goal axioms =
  concat [ getIdent n , resolveCase , "\n" ] ++
    if not swap
      then concat
          [ getIdent (n+1) , "(\n"
          , getIdent (n+1) , printSteps sname (n+2) [left] dict goal axioms
          , getIdent (n+1) , ")\n"
          , getIdent (n+1) , "(\n"
          , getIdent (n+1) , printSteps sname (n+2) [right] dict goal axioms
          , getIdent (n+1) , ")\n"
          ]
      else concat
          [ getIdent (n+1) , "(\n"
          , getIdent (n+1) , printSteps sname (n+2) [right] dict goal axioms
          , getIdent (n+1) , ")\n"
          , getIdent (n+1) , "(\n"
          , getIdent (n+1) , printSteps sname (n+2) [left] dict goal axioms
          , getIdent (n+1) , ")\n"
          ]

    where
      ϕ  ∷ Formula
      ϕ = formula . fromJust $ Map.lookup tag dict

      --  (f)      (g)
      --  _|_      _|_
      -- /   \    /    \
      -- ϕ₁ ∨ ℓ  ϕ₂ ∨ ¬ ℓ
      -- ---------------- resolve ℓ
      --     ϕ₁ ∨ ϕ₂
      --     \____/
      --        |
      ---       ϕ

      f , g ∷ Formula
      f = formula . fromJust $ Map.lookup fTag dict
      g = formula . fromJust $ Map.lookup gTag dict

      -- ℓ ∷ Formula
      -- ℓ = let sourceInfo ∷ Source
      --         sourceInfo = source . fromJust $ Map.lookup tag dict
      --     in getResolveLiteral sourceInfo

      resolveCase ∷ String
      swap ∷ Bool
      (resolveCase, swap) = atpResolve f g ϕ

      -- getResolveLiteral ∷ Source → Formula
      -- getResolveLiteral
      --   (Inference Resolve (Function _ (GTerm (GWord l):_) :_) _) =
      --     PredApp l []
      -- getResolveLiteral _ = PredApp (AtomicWord "$false") []

printSteps sname n [Root Conjunct tag subtree@[Root _ fms _]] dict goal axioms =
 concat
   [ getIdent n , atpConjunct ψ ϕ
   , printSteps sname (n+1) subtree dict goal axioms
   ]
   where
     ϕ , ψ ∷ Formula
     ϕ = formula . fromJust $ Map.lookup tag dict
     ψ = formula . fromJust $ Map.lookup fms dict

printSteps sname n [Root inf tag subtree] dict goal axioms =
  concat
    [ getIdent n , inferenceName , " $" , printInnerFormula 1 dict tag "Γ" , "\n"
    , printSteps sname (n+1) subtree dict goal axioms
    ]
  where
    inferenceName ∷ String
    inferenceName = case inf of
      Canonicalize → "atp-canonicalize"
      Strip        → "atp-strip"
      _            → "? -- inference rule no supported yet"

-- TODO: check the output formula, and use atp-conjuct with this output and the
-- original formula, the atp-conjuct is a specific implemention of projections of ∧.
printSteps _ n [Leaf Conjecture gname] _ _ _ =
  concat
    [ getIdent n , gname , "\n"
    ]

printSteps sname n [Leaf Axiom gname] _ _ _ =
  concat
    [ getIdent n , "weaken (atp-neg " , stdName sname , ") $\n"
    , getIdent (n+1) , "(assume {Γ = ∅} " , gname , ")\n"
    ]
printSteps _ n _ _ _ _ = getIdent n ++ "? -- no supported yet\n"


andIntroSubgoals ∷ Ident → Int → [F] → String
andIntroSubgoals _ _ []       = ""
andIntroSubgoals m n [_]      = getIdent m ++ "subgoal" ++ stdName (show n)
andIntroSubgoals m n [_,_]    =
  concat
    [ getIdent m , "subgoal" , stdName (show n) , "\n"
    , getIdent m , "subgoal" , stdName (show (n+1)) , "\n"
    ]
andIntroSubgoals m n (_:xs) =
  concat
    [ getIdent m , "subgoal", stdName (show n) , "\n"
    , getIdent m, "(\n"
    , getIdent m , "∧-intro\n"
    , andIntroSubgoals (m+1) (n+1) xs
    , getIdent m, ")\n"
    ]

printProofGoal ∷ [F] → F → ProofMap → [ProofTree] → IO ()
printProofGoal [] _ _ _  = putStrLn "-- Proof not available.\n"
printProofGoal [_] _ _ _ = putStrLn $
  concat
    [ "proof : Γ ⊢ goal" , "\n"
    , "proof =" , "\n"
    , getIdent 1 , "⇒-elim", "\n"
    , getIdent 2 , "atp-splitGoal" , "\n"
    , getIdent 2 , "proof₀" , "\n"
    ]

printProofGoal subgoals _ _ _ = putStrLn $
  concat
    [ "proof : Γ ⊢ goal" , "\n"
    , "proof =" , "\n"
    , getIdent 1 , "⇒-elim", "\n"
    , getIdent 2 , "atp-splitGoal" , "\n"
    , getIdent 2 , "(\n"
    , getIdent 2 , "∧-intro\n"
    , andIntroSubgoals 3 0 subgoals
    , getIdent 2 , ")\n"
    ]
