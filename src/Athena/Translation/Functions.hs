
-- | Athena.Translation.Functions module.

{-# OPTIONS -fno-warn-missing-signatures  #-}
{-# LANGUAGE UnicodeSyntax                #-}
{-# LANGUAGE ScopedTypeVariables          #-}

module Athena.Translation.Functions
   ( docAxioms
   , docConjecture
   , docHeader
   , docPremises
   , docSubgoals
   , docVars
   , getAxioms
   , getConjeture
   , getRefutes
   , getSubgoals
  --  , printProof
   ) where

------------------------------------------------------------------------------

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
import Athena.Options            ( Options ( optInputFile ) )
import Athena.Translation.Utils  ( stdName )
import Athena.Utils.PrettyPrint
  ( (<+>)
  , (<@>)
  , (<>)
  , (</>)
  , colon
  , comment
  , lbracket
  , rbracket
  , comma
  , Doc
  , dot
  , empty
  , equals
  , hashtag
  , hcat
  , hypenline
  , int
  , space
  , line
  , parens
  , Pretty(pretty)
  , putDoc
  , encloseSep
  -- , softline
  )
import Athena.Utils.Version      ( progNameVersion )

import Data.Proof
  ( ProofMap
  , ProofTree
  , ProofTreeGen(..)
  )

import Data.List                ( isPrefixOf, intercalate )
import Data.Maybe               ( fromJust, Maybe )
import qualified Data.Map as Map

import Data.TSTP
  ( F    ( name, role, formula )
  , Formula(..)
  , Role ( Axiom, Conjecture )
  , Rule(..)
  )
import Data.TSTP.V              ( V(..) )

------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Header.
------------------------------------------------------------------------------

-- | Pretty the header in the Agda file.
docHeader ∷ Options -> Int → IO Doc
docHeader opts n = do
  version :: String <- progNameVersion
  return
     (   hypenline
     <>  comment (pretty version <> dot)
     <>  comment (pretty "Tstp file:" <+> pretty (optInputFile opts) <> dot)
     <>  hypenline
     <@> hypenline
     <@> pretty "open import ATP.Metis" <+> int n <+> pretty "public" <> line
     <>  pretty "open import Data.Prop" <+> int n <+> pretty "public" <> line
     <@> hypenline
     )

------------------------------------------------------------------------------
-- Variables.
------------------------------------------------------------------------------

prettyVar :: V -> Int -> Doc
prettyVar var n = case show var of
  "$true"  → pretty '⊤'
  "$false" → pretty '⊥'
  _        → pretty "Var" <+> parens (hashtag <+> int n)

-- | Pretty a list of variables.
docVars :: [V] -> Doc
docVars []   = empty
docVars vars =
     line
  <> comment (pretty "Variable" <> s <> dot) <> line
  <> docVars' vars 0
  where
    s :: Doc
    s = if length vars > 1 then pretty 's' else empty

    docVars' :: [V] -> Int -> Doc
    docVars' [] _         = empty
    docVars' (var : vs) n =
          pretty var <+> colon  <+> pretty "Prop" <> line
      <>  pretty var <+> equals <+> (prettyVar var n) <> line
      <@> docVars' vs (n+1)

------------------------------------------------------------------------------
-- Axioms.
------------------------------------------------------------------------------

definition :: F -> Doc
definition fm =
     pretty fm <+> colon  <+> pretty "Prop"   <> line
  <> pretty fm <+> equals <+> pretty formulaF <> line
  where
    formulaF :: Formula
    formulaF = formula fm

-- | Extract axioms from a list of formulae.
getAxioms ∷ [F] → [F]
getAxioms = filter ((==) Axiom . role)

-- | Pretty a list of axioms.
docAxioms :: [F] -> Doc
docAxioms []  = empty
docAxioms fms =
     line
  <> comment (pretty "Axiom" <> s <> dot) <> line
  <> docAxioms' fms
  where
    s :: Doc
    s = if length fms > 1 then pretty 's' else empty

    docAxioms' :: [F] -> Doc
    docAxioms' []         = empty
    docAxioms' (fm : fms) =
          definition fm
      <@> docAxioms' fms

------------------------------------------------------------------------------
-- Premises.
------------------------------------------------------------------------------

-- | Pretty a list of premises.
docPremises ∷ [F] → Doc
docPremises premises =
    line
  <> comment (pretty "Premise" <> s <> dot) <> line
  <> pretty 'Γ' <+> colon  <+> pretty "Ctxt" <> line
  <> pretty 'Γ' <+> equals <+> pms <> line
  where
    s :: Doc
    s = if length premises > 1 then pretty 's' else empty

    pms :: Doc
    pms = case premises of
      []  → pretty '∅'
      [p] → lbracket <+> pretty (stdName (name p)) <+> rbracket
      ps  → pretty '∅' <+> comma <+> f (map pretty ps)

    f :: [Doc]  -> Doc
    f = encloseSep empty empty (space <> comma <> space)

------------------------------------------------------------------------------
-- Conjecture.
------------------------------------------------------------------------------

-- | Try to extract a conjecture from a list of formulae and checks
-- for uniqueness.
getConjeture ∷ [F] → Maybe F
getConjeture rules =
  case filter ((==) Conjecture . role) rules of
    [l] → Just l
    _   → Nothing

-- | Pretty the conjecture.
docConjecture ∷ F → Doc
docConjecture fm =
     line
  <> comment (pretty "Conjecture" <> dot) <> line
  <> definition fm

------------------------------------------------------------------------------
-- Subgoals.
------------------------------------------------------------------------------

-- | Extract subgoals from a list of formulae.
getSubgoals ∷ [F] → [F]
getSubgoals = filter (isPrefixOf "subgoal" . name)

docSubgoals :: [F] -> Doc
docSubgoals []  = empty
docSubgoals fms =
     line
  <> comment (pretty "Subgoal" <> s <> dot) <> line
  <> docSubgoals' fms
  where
    s :: Doc
    s = if length fms > 1 then pretty 's' else empty

    docSubgoals' :: [F] -> Doc
    docSubgoals' []         = empty
    docSubgoals' (fm : fms) =
          definition fm
      <@> docSubgoals' fms

------------------------------------------------------------------------------
-- Refutes.
------------------------------------------------------------------------------

-- | Extract refuting steps from a list of formulae.
getRefutes ∷ [F] → [F]
getRefutes = filter (isPrefixOf "refute"  . name)


--
-- -- | Print out a formula by name with a identation.
-- printInnerFormula ∷ Ident → ProofMap → String → String -> String
-- printInnerFormula n dict tag ctxt =
--   if debug
--     then do
--       let fm ∷ Maybe F
--           fm = Map.lookup tag dict
--       let strFm ∷ String
--           strFm = stdName . show . formula $ fromJust fm
--       concat [ getIdent n , "-- " , ctxt , " ⊢ " , strFm ]
--     else ""
--
-- -- Proof.
--
-- printProof ∷ [F] → [F] → F → ProofMap → [ProofTree] → IO ()
-- printProof _ _  _ _ [] = return ()
-- printProof axioms subgoals goal rmap rtree = do
--   putStrLn ""
--   putStrLn $ replicate 78 '-'
--   putStrLn "-- Proof"
--   putStrLn $ replicate 78 '-'
--   putStrLn "\n"
--   printProofSubgoal 0 axioms subgoals goal rmap rtree
--   printProofGoal subgoals goal rmap rtree
--
-- printProofSubgoal ∷ Int → [F] → [F] → F → ProofMap → [ProofTree] → IO ()
-- printProofSubgoal _ _ _ _ _ [] = return ()
-- printProofSubgoal no axioms subgoals goal rmap (tree:strees) = do
--   let strNo       = stdName $ show no
--   let proofName   = stdName $ "proof" ++ strNo
--   let subgoalName = "subgoal" ++ strNo
--   let proof ∷ String
--       proof = concat
--         [ proofName , " : Γ ⊢ " , subgoalName , "\n"
--         , proofName , " =\n"
--         , "  RAA $" , "\n"
--         , printSteps subgoalName 2 [tree] rmap goal axioms
--         ]
--   putStrLn proof
--   printProofSubgoal (no+1) axioms subgoals goal rmap strees
--
--
-- printSteps ∷ String → Ident → [ProofTree] → ProofMap → F → [F] → String
-- printSteps sname n [Root Negate tag [Root Strip subgoalname _]] dict _ _ =
--   concat
--     [ getIdent n , "atp-strip $" , printInnerFormula 1 dict subgoalname "Γ" , "\n"
--     , getIdent (n+1) , "assume {Γ = Γ} $" ,  printInnerFormula 1 dict tag "Γ" , "\n"
--     , getIdent (n+2) , "atp-neg " , sname , "\n"
--     ]
--
-- printSteps sname n [Root Simplify tag subtree] dict goal axioms =
--   concat
--     [ getIdent n , "atp-simplify $" , printInnerFormula 1 dict tag "Γ" , "\n"
--     , getIdent (n+1) , "∧-intro\n"
--     , andIntro (n+2) subtree
--     ]
--     where
--       innerStep m step = concat
--         [ getIdent m , "(\n"
--         , printSteps sname m [step] dict goal axioms
--         , getIdent m , ")\n"
--         ]
--
--       andIntro _ []     = ""
--       andIntro m [x]    = printSteps sname m [x] dict goal axioms
--       andIntro m [x,y]  = concatMap (innerStep m) [x , y]
--       andIntro m (x:xs) = concat
--         [ innerStep m x
--         , getIdent m , "(\n"
--         , getIdent m , "∧-intro\n"
--         , andIntro (m+1) xs
--         , getIdent m , ")\n"
--         ]
--
-- printSteps sname n [Root Resolve tag ((left@(Root _ fTag _)):(right@(Root _ gTag _)):_)] dict goal axioms =
--   concat [ getIdent n , resolveCase , "\n" ] ++
--     if not swap
--       then concat
--           [ getIdent (n+1) , "(\n"
--           , getIdent (n+1) , printSteps sname (n+2) [left] dict goal axioms
--           , getIdent (n+1) , ")\n"
--           , getIdent (n+1) , "(\n"
--           , getIdent (n+1) , printSteps sname (n+2) [right] dict goal axioms
--           , getIdent (n+1) , ")\n"
--           ]
--       else concat
--           [ getIdent (n+1) , "(\n"
--           , getIdent (n+1) , printSteps sname (n+2) [right] dict goal axioms
--           , getIdent (n+1) , ")\n"
--           , getIdent (n+1) , "(\n"
--           , getIdent (n+1) , printSteps sname (n+2) [left] dict goal axioms
--           , getIdent (n+1) , ")\n"
--           ]
--
--     where
--       ϕ  ∷ Formula
--       ϕ = formula . fromJust $ Map.lookup tag dict
--
--       --  (f)      (g)
--       --  _|_      _|_
--       -- /   \    /    \
--       -- ϕ₁ ∨ ℓ  ϕ₂ ∨ ¬ ℓ
--       -- ---------------- resolve ℓ
--       --     ϕ₁ ∨ ϕ₂
--       --     \____/
--       --        |
--       ---       ϕ
--
--       f , g ∷ Formula
--       f = formula . fromJust $ Map.lookup fTag dict
--       g = formula . fromJust $ Map.lookup gTag dict
--
--       -- ℓ ∷ Formula
--       -- ℓ = let sourceInfo ∷ Source
--       --         sourceInfo = source . fromJust $ Map.lookup tag dict
--       --     in getResolveLiteral sourceInfo
--
--       resolveCase ∷ String
--       swap ∷ Bool
--       (resolveCase, swap) = atpResolve f g ϕ
--
--       -- getResolveLiteral ∷ Source → Formula
--       -- getResolveLiteral
--       --   (Inference Resolve (Function _ (GTerm (GWord l):_) :_) _) =
--       --     PredApp l []
--       -- getResolveLiteral _ = PredApp (AtomicWord "$false") []
--
-- printSteps sname n [Root Conjunct tag subtree@[Root _ fms _]] dict goal axioms =
--  concat
--    [ getIdent n , atpConjunct ψ ϕ
--    , printSteps sname (n+1) subtree dict goal axioms
--    ]
--    where
--      ϕ , ψ ∷ Formula
--      ϕ = formula . fromJust $ Map.lookup tag dict
--      ψ = formula . fromJust $ Map.lookup fms dict
--
-- printSteps sname n [Root inf tag subtree] dict goal axioms =
--   concat
--     [ getIdent n , inferenceName , " $" , printInnerFormula 1 dict tag "Γ" , "\n"
--     , printSteps sname (n+1) subtree dict goal axioms
--     ]
--   where
--     inferenceName ∷ String
--     inferenceName = case inf of
--       Canonicalize → "atp-canonicalize"
--       Strip        → "atp-strip"
--       _            → "? -- inference rule no supported yet"
--
-- -- TODO: check the output formula, and use atp-conjuct with this output and the
-- -- original formula, the atp-conjuct is a specific implemention of projections of ∧.
-- printSteps _ n [Leaf Conjecture gname] _ _ _ =
--   concat
--     [ getIdent n , gname , "\n"
--     ]
--
-- printSteps sname n [Leaf Axiom gname] _ _ _ =
--   concat
--     [ getIdent n , "weaken (atp-neg " , stdName sname , ") $\n"
--     , getIdent (n+1) , "(assume {Γ = ∅} " , stdName gname , ")\n"
--     ]
-- printSteps _ n _ _ _ _ = getIdent n ++ "? -- no supported yet\n"
--
--
-- andIntroSubgoals ∷ Ident → Int → [F] → String
-- andIntroSubgoals _ _ []    = ""
-- andIntroSubgoals m n [_]   = getIdent m ++ "subgoal" ++ stdName (show n)
-- andIntroSubgoals m n [_,_] =
--   concat
--     [ getIdent m , "subgoal" , stdName (show n) , "\n"
--     , getIdent m , "subgoal" , stdName (show (n+1)) , "\n"
--     ]
-- andIntroSubgoals m n (_:xs) =
--   concat
--     [ getIdent m , "subgoal", stdName (show n) , "\n"
--     , getIdent m, "(\n"
--     , getIdent m , "∧-intro\n"
--     , andIntroSubgoals (m+1) (n+1) xs
--     , getIdent m, ")\n"
--     ]
--
-- printProofGoal ∷ [F] → F → ProofMap → [ProofTree] → IO ()
-- printProofGoal [] _ _ _  = putStrLn "-- Proof not available.\n"
-- printProofGoal [_] _ _ _ = putStrLn $
--   concat
--     [ "proof : Γ ⊢ goal" , "\n"
--     , "proof =" , "\n"
--     , getIdent 1 , "⇒-elim", "\n"
--     , getIdent 2 , "atp-splitGoal" , "\n"
--     , getIdent 2 , "proof₀" , "\n"
--     ]
--
-- printProofGoal subgoals _ _ _ = putStrLn $
--   concat
--     [ "proof : Γ ⊢ goal" , "\n"
--     , "proof =" , "\n"
--     , getIdent 1 , "⇒-elim", "\n"
--     , getIdent 2 , "atp-splitGoal" , "\n"
--     , getIdent 2 , "(\n"
--     , getIdent 2 , "∧-intro\n"
--     , andIntroSubgoals 3 0 subgoals
--     , getIdent 2 , ")\n"
--     ]
