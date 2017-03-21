-- | Main module

{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Translation.Core (mainCore) where

------------------------------------------------------------------------------

import Options
  (
    Options
    ( optHelp
    , optInputFile
    , optOutputFile
    , optProofName
    , optVersion
    )
    , printUsage
    , processOptions
    )

import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe ( fromJust, Maybe, fromMaybe )

import Data.Proof
  ( buildProofMap
  , buildProofTree
  , ProofTree(..)
  , ProofMap(..)
  , ProofTreeGen(..)
  )
import Data.TSTP
  ( F(..)
  , Formula(..)
  , Rule(..)
  , Role(..)
  , Info(Function)
  , GData(..)
  , GTerm(..)
  , Source(..)
  )
import Data.TSTP.Formula    ( getFreeVars )
import Data.TSTP.AtomicWord ( AtomicWord(..) )
import Data.TSTP.BinOp      ( BinOp(..) )
import Data.TSTP.InfixPred  ( InfixPred(..) )
import Data.TSTP.Quant      ( Quant(..) )
import Data.TSTP.Term       ( Term(..) )
import Data.TSTP.V          ( V(..) )

import System.Environment ( getArgs )
import System.Exit        ( exitSuccess )

import Utils.Monad        ( die , stdout2file )
import Utils.PrettyPrint  ( text )
import Utils.Version      ( progNameVersion )

import Translation.Functions
  ( getAxioms
  , getConjeture
  , getRefutes
  , getSubGoals
  , printPreamble
  )

import TSTP (parseFile)

------------------------------------------------------------------------------

debug = True

mainCore ∷ Options → IO ()
mainCore opts = do

  tstp ∷ [F] ← parseFile $ fromJust $ optInputFile opts

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
      rulesTrees = map (buildProofTree rulesMap) refutes


  stdout2file $ optOutputFile opts

  let formulas ∷ [Formula]
      formulas = map formula tstp

  let freevars ∷ [V]
      freevars = getFreeVars formulas

  printPreamble (length freevars)

  putStrLn "-- Vars"
  printVars freevars 0

  printAxioms axioms

  printPremises axioms

  printConjecture conj

  printSubGoals' subgoals

  printProof axioms subgoals conj rulesMap rulesTrees

printVar ∷ V → Int → String
printVar f n = intercalate "\n"
  [  show f ++ " : Prop"
  ,  show f ++ case show f of
       "$true" → " = ⊤"
       "$false"→ " = ⊥"
       s → " = Var (# " ++ show n ++ ")"
  ]

printVars ∷ [V] → Int → IO String
printVars [] _ = return ""
printVars (f : fs) n = do
  putStrLn $ printVar f n ++ "\n"
  printVars fs (n+1)

subIndex ∷ Char → Char
subIndex '0' = '₀'
subIndex '1' = '₁'
subIndex '2' = '₂'
subIndex '3' = '₃'
subIndex '4' = '₄'
subIndex '5' = '₅'
subIndex '6' = '₆'
subIndex '7' = '₇'
subIndex '8' = '₈'
subIndex '9' = '₉'
subIndex s   = s

stdName ∷ String → String
stdName nm = map subIndex $ concat $ splitOn "-" nm

printAxiom ∷ F → String
printAxiom f =
  let axiom = stdName $ name f
  in concat
    [  axiom , " : Prop\n"
    ,  axiom , " = " ,  show (formula f) , "\n"
    ]

printAxioms ∷ [F] → IO ()
printAxioms [] = return ()
printAxioms [a] = do
  putStrLn "-- Axiom"
  putStrLn $ printAxiom a ++ "\n"
printAxioms as = do
  putStrLn "-- Axioms"
  putStrLn $ intercalate "\n\n" $ map printAxiom as
  putStrLn ""

printPremises ∷ [F] → IO ()
printPremises premises = do
  putStrLn $ "-- Premise" ++ (if length premises < 2 then "" else "s")
  putStrLn "Γ : Ctxt"
  case premises of
    []  → putStrLn "Γ = ∅"
    [p] → putStrLn $ "Γ = [ " ++ name p ++ " ]"
    ps  → putStrLn $ "Γ = ∅ , " ++ intercalate " , " (map name ps)
  putStrLn ""

printConjecture ∷ F → IO ()
printConjecture f = putStrLn $
  concat
    [  "-- Conjecture\n"
    , printAxiom f , "\n"
    ]

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

type Ident = Int

getIdent ∷ Ident → String
getIdent n = concat $ replicate (2 * n) " "

printInnerFormula ∷ Ident → ProofMap → String → String -> String
printInnerFormula n dict tag ctxt =
  if debug
    then do
      let fm ∷ Maybe F
          fm = Map.lookup tag dict
      let strFm ∷ String
          strFm = show $ formula $ fromJust fm
      concat [ getIdent n , "-- " , ctxt , " ⊢ " , strFm ]
    else ""

printSteps ∷ String → Ident → [ProofTree] → ProofMap → F → [F] → String
printSteps sname n [Root Negate tag [Root Strip subgoalname st]] dict goal axioms =
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

      andIntro m []       = ""
      andIntro m [x]      = printSteps sname m [x] dict goal axioms
      andIntro m (x:y:[]) = concatMap (innerStep m) [x , y]
      andIntro m (x:xs) = concat
        [ innerStep m x
        , getIdent m , "(\n"
        , getIdent m , "∧-intro\n"
        , andIntro (m+1) xs
        , getIdent m , ")\n"
        ]

printSteps sname n [Root Resolve tag ((left@(Root _ fTag _)):(right@(Root _ gTag _)):_)] dict goal axioms =
  (concat
    [ getIdent n , resolveCase , "\n" ]) ++
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
      ϕ = formula $ fromJust $ Map.lookup tag dict

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
      f = formula $ fromJust $ Map.lookup fTag dict
      g = formula $ fromJust $ Map.lookup gTag dict

      ℓ ∷ Formula
      ℓ = let sourceInfo ∷ Source
              sourceInfo = source $ fromJust $ Map.lookup tag dict
          in getResolveLiteral $ sourceInfo

      resolveCase ∷ String
      swap ∷ Bool
      (resolveCase, swap) = atpResolve f g ϕ

      atpResolve ∷ Formula → Formula → Formula → (String, Bool)
      atpResolve f g  (PredApp (AtomicWord "$false") [])
        | f == ((:~:) g) = ("atp-resolve₈", False)
        | ((:~:) f) == g = ("atp-resolve₈", True)
      -- I guess l literal is always positive.
      atpResolve (BinOp f₁ (:|:) f₂) (BinOp g₁ (:|:) g₂) l
        | f₁ == l && g₁ == ((:~:) l) = ("atp-resolve₀", False)
        | f₂ == l && g₂ == ((:~:) l) = ("atp-resolve₁", False)
        | f₁ == l && g₂ == ((:~:) l) = ("atp-resolve₂", False)
        | f₂ == l && g₁ == ((:~:) l) = ("atp-resolve₃", False)
        | otherwise = ("id -- resolve 1.", False)
      atpResolve (BinOp f₁ (:|:) f₂) g l
        | f₁ == ((:~:) l) && g == l = ("atp-resolve₄", False)
        | f₂ == ((:~:) l) && g == l = ("atp-resolve₅", False)
        | f₂ == l && g == ((:~:) l) = ("atp-resolve₆", False)
        | f₁ == l && g == ((:~:) l) = ("atp-resolve₇", False)
        | otherwise = ("id -- resolve 2.", False)
      atpResolve f (BinOp g₁ (:|:) g₂) l
        | f == l && g₁ == ((:~:) l) = ("atp-resolve₄", True)
        | f == l && g₂ == ((:~:) l) = ("atp-resolve₅", True)
        | f == ((:~:) l) && g₂ == l = ("atp-resolve₆", True)
        | f == ((:~:) l) && g₁ == l = ("atp-resolve₇", True)
        | otherwise = ("id -- resolve 3.", False)
      atpResolve _ _ _ = ("id -- resolve 4.", False)

      getResolveLiteral ∷ Source → Formula
      getResolveLiteral
        (Inference Resolve ((Function _ (GTerm (GWord l):_) ) :_) _) =
          PredApp l []
      getResolveLiteral _ = (PredApp (AtomicWord "$false") [])



printSteps sname n [Root Conjunct tag subtree@[Root _ fms _]] dict goal axioms =
 concat
   [ getIdent n , atpConjunct ψ ϕ
   , printSteps sname (n+1) subtree dict goal axioms
   ]
   where
     ϕ , ψ∷ Formula
     ϕ = formula $ fromJust $ Map.lookup tag dict
     ψ = formula $ fromJust $ Map.lookup fms dict

     atpConjunct ∷ Formula → Formula → String
     atpConjunct (BinOp f₁ (:&:) f₂) ϕ
       | f₂ /= ϕ = "∧-proj₁ $" ++ if null next then "\n" else " " ++ next
       | otherwise = "∧-proj₂ $ -- (" ++ show f₂ ++" ≟ " ++ show ϕ ++ ")\n"
       where
         next ∷ String
         next = atpConjunct f₁ ϕ
         atpConjunct fm@(BinOp f₁ _ f₂) ϕ = "-- 1: "++ show fm ++ "\n"
         atpConjunct fm@(InfixPred _ _ _) _ ="-- 2: " ++ show fm ++ "\n"
         atpConjunct fm@(PredApp _ _) _ = "-- 3: " ++ show fm ++ "\n"
         atpConjunct fm@(Quant _ _ _) _ = "-- 4: " ++ show fm ++ "\n"
         atpConjunct fm@((:~:) _) _ = "-- 5: " ++ show fm ++ "\n"


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
printSteps sname n [Leaf Conjecture gname] dict goal axioms =
  concat
    [ getIdent n , gname , "\n"
    ]

printSteps sname n [Leaf Axiom gname] dict goal axioms =
  concat
    [ getIdent n , "weaken (atp-neg " , stdName sname , ") $\n"
    , getIdent (n+1) , "(assume {Γ = ∅} " , gname , ")\n"
    ]
printSteps _ n _ _ _ _ = getIdent n ++ "? -- no supported yet\n"


andIntroSubgoals ∷ Ident → Int → [F] → String
andIntroSubgoals m n []       = ""
andIntroSubgoals m n [x]      = getIdent m ++ "subgoal" ++ stdName (show n)
andIntroSubgoals m n (x:y:[]) =
  concat
    [ getIdent m , "subgoal" , stdName (show n) , "\n"
    , getIdent m , "subgoal" , stdName (show (n+1)) , "\n"
    ]
andIntroSubgoals m n (x:xs) =
  concat
    [ getIdent m , "subgoal", stdName (show n) , "\n"
    , getIdent m, "(\n"
    , getIdent m , "∧-intro\n"
    , andIntroSubgoals (m+1) (n+1) xs
    , getIdent m, ")\n"
    ]

printProofGoal ∷ [F] → F → ProofMap → [ProofTree] → IO ()
printProofGoal [] _ _ _  = putStrLn "-- Proof not available.\n"
printProofGoal [s] _ _ _ = putStrLn $
  concat
    [ "proof : Γ ⊢ goal" , "\n"
    , "proof =" , "\n"
    , getIdent 1 , "⇒-elim", "\n"
    , getIdent 2 , "atp-splitGoal" , "\n"
    , getIdent 2 , "proof₀" , "\n"
    ]

printProofGoal subgoals goal rmap rtree = putStrLn $
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

{-
orComm ∷ String → Formula → Formula → String
orComm name (BinOp f₁ (:|:) f₂) ((:~:) ϕ)
  | f₁ /= ϕ = "∨-comm $" ++ orComm name f₂ ϕ
  | otherwise = name
orComm name (BinOp ((:~:) f₁) (:|:) f₂) ϕ
  | f₁ /= ϕ = "∨-comm $" ++ orComm name f₂ ϕ
  | otherwise = name
-}
