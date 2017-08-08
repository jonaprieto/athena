-- | Athena.Translation.Functions module.

{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE UnicodeSyntax                #-}
{-# OPTIONS -fno-warn-missing-signatures  #-}

module Athena.Translation.AgdaFile
   (
   AgdaFile
      ( AgdaFile
      , fileVariables
      , fileAxioms
      , fileConjecture
      , fileDict
      , fileName
      , filePremises
      , fileSubgoals
      , fileTrees
      )
   , docAxioms
   , docConjecture
   , docHeader
   , docImports
   , docPremises
   , docSubgoals
   , docProofGoal
   , docVars
   , getAxioms
   , getConjeture
   , getRefutes
   , getSubgoals
   ) where

------------------------------------------------------------------------------

import Athena.Translation.Rules
  (
--   -- atpCanonicalize
--   -- , atpClausify
--     atpConjunct
--   -- , atpNegate
  atpResolve
--   -- , atpSimplify
  -- , atpSplit
  )
import Athena.Translation.Rules.Strip        ( atpSplit, unshunt, split )

import Athena.Options            ( Options ( optInputFile ) )
import Athena.Translation.Utils  ( stdName )
import Athena.Utils.PrettyPrint
  ( (<+>)
  , (<>)
  , (<@>)
  , Doc
  , Pretty(pretty)
--  , braces
  , colon
  , comma
  , comment
  , dot
  , empty
  , encloseSep
  , equals
  , hashtag
  , hypenline
  , indent
  , int
  , lbracket
  , line
  , parens
  , rbracket
  , space
  , vsep
  )
import Athena.Utils.Version      ( progNameVersion )

import Data.Proof
  ( ProofMap
  , ProofTree
  , ProofTreeGen ( Root, Leaf )
  )

import Data.List                ( isPrefixOf, nub)
import Data.Maybe               ( fromJust )
import qualified Data.Map as Map

import Data.TSTP
  ( F ( name, role, formula, source )
  , Formula(..)
  , GData(..)
  , GTerm(..)
  , Info ( Function )
  , Role ( Axiom, Conjecture )
  , Rule
    ( Canonicalize
    , Clausify
    , Conjunct
    , Negate
    , Simplify
    , Strip
    , Resolve
    , Skolemize
    , Specialize
    , NewRule
    )
  , Source(..)
  , V(..)
  )

import System.FilePath          ( takeBaseName )

------------------------------------------------------------------------------

-- | Agda file.
data AgdaFile = AgdaFile
  { fileAxioms     ∷ [F]
  , fileConjecture ∷ F
  , fileDict       ∷ ProofMap
  , fileName       ∷ FilePath
  , filePremises   ∷ [F]
  , fileSubgoals   ∷ [F]
  , fileTrees      ∷ [ProofTree]
  , fileVariables  ∷ [V]
  }

instance Pretty AgdaFile where
  pretty problem =
   vsep
     [ docModule (fileName problem)
     , docImports (length (fileVariables problem))
     , docVars (fileVariables problem)
     , docAxioms (fileAxioms problem)
     , docPremises (filePremises problem)
     , docConjecture (fileConjecture problem)
     , docSubgoals (fileSubgoals problem)
     , docProof problem
     ]


getFormula ∷ AgdaFile → String → Formula
getFormula agdaFile tag = φ
  where
     φ ∷ Formula
     φ = formula . fromJust $ Map.lookup tag dict

     dict ∷ ProofMap
     dict = fileDict agdaFile

getFormulaByTag ∷ AgdaFile → String → Doc
getFormulaByTag agdaFile tag = pretty φ
  where
     φ ∷ Formula
     φ = formula . fromJust $ Map.lookup tag dict

     dict ∷ ProofMap
     dict = fileDict agdaFile

------------------------------------------------------------------------------
-- Header.
------------------------------------------------------------------------------

-- | Pretty the header in the Agda file.
docHeader ∷ Options → IO Doc
docHeader opts = do
  version ∷ String ← progNameVersion
  return
     (  hypenline
     <> comment (pretty version <> dot)
     <> comment (pretty "TSTP file:" <+> pretty (optInputFile opts) <> dot)
     <> hypenline <> line
     )

------------------------------------------------------------------------------
-- Module.
------------------------------------------------------------------------------

docModule ∷ FilePath → Doc
docModule filename =
  pretty "module" <+> nameModule <+> pretty "where" <> line
  where
    nameModule ∷ Doc
    nameModule = pretty . takeBaseName $ filename

------------------------------------------------------------------------------
-- Imports.
------------------------------------------------------------------------------

-- | This includes the imports necessary to load the Agda file.
docImports ∷ Int → Doc
docImports n =
       hypenline
   <@> pretty "open import ATP.Metis" <+> int n <+> pretty "public" <> line
   <>  pretty "open import Data.Prop" <+> int n <+> pretty "public" <> line
   <@> hypenline

------------------------------------------------------------------------------
-- Variables.
------------------------------------------------------------------------------

prettyVar ∷ V → Int → Doc
prettyVar (V v) n = case v of
  "$true"  → pretty '⊤'
  "$false" → pretty '⊥'
  _        → pretty "Var" <+> parens (hashtag <+> int n)

-- | Pretty a list of variables.
docVars ∷ [V] → Doc
docVars []   = empty
docVars vars =
     comment (pretty "Variable" <> s <> dot) <> line
  <> docVars' vars 0
  where
    s ∷ Doc
    s = if length vars > 1 then pretty 's' else empty

    docVars' ∷ [V] → Int → Doc
    docVars' [] _         = empty
    docVars' (var : vs) n =
          pretty var <+> colon  <+> pretty "Prop" <> line
      <>  pretty var <+> equals <+> prettyVar var n <> line
      <@> docVars' vs (n+1)

------------------------------------------------------------------------------
-- Axioms.
------------------------------------------------------------------------------

definition ∷ F → Doc
definition fm =
     pretty fm <+> colon  <+> pretty "Prop"   <> line
  <> pretty fm <+> equals <+> pretty formulaF <> line
  where
    formulaF ∷ Formula
    formulaF = formula fm

-- | Extract axioms from a list of formulae.
getAxioms ∷ [F] → [F]
getAxioms = filter ((==) Axiom . role)

-- | Pretty a list of axioms.
docAxioms ∷ [F] → Doc
docAxioms []  = empty
docAxioms fms =
     comment (pretty "Axiom" <> s <> dot) <> line
  <> docAxioms' fms
  where
    s ∷ Doc
    s = if length fms > 1 then pretty 's' else empty

    docAxioms' ∷ [F] → Doc
    docAxioms' = foldr ((<@>) . definition) empty

------------------------------------------------------------------------------
-- Premises.
------------------------------------------------------------------------------

toCtxt ∷ [Doc]  → Doc
toCtxt = encloseSep empty empty (space <> comma <> space)


-- | Pretty a list of premises.
docPremises ∷ [F] → Doc
docPremises premises =
     comment (pretty "Premise" <> s <> dot) <> line
  <> pretty 'Γ' <+> colon  <+> pretty "Ctxt" <> line
  <> pretty 'Γ' <+> equals <+> pms <> line
  where
    s ∷ Doc
    s = if length premises > 1 then pretty 's' else empty

    pms ∷ Doc
    pms = case premises of
      []  → pretty '∅'
      [p] → lbracket <+> pretty (stdName (name p)) <+> rbracket
      ps  → pretty '∅' <+> comma <+> toCtxt (map pretty ps)

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
     comment (pretty "Conjecture" <> dot) <> line
  <> definition fm
------------------------------------------------------------------------------
-- Sub-goals.
------------------------------------------------------------------------------

-- | Extract subgoals from a list of formulae.
getSubgoals ∷ [F] → [F]
getSubgoals = filter (isPrefixOf "subgoal" . name)

docSubgoals ∷ [F] → Doc
docSubgoals []       = empty
docSubgoals formulas =
     comment (pretty "Subgoal" <> s <> dot) <> line
  <> docSubgoals' formulas
  where
    s ∷ Doc
    s = if length formulas > 1 then pretty 's' else empty

    docSubgoals' ∷ [F] → Doc
    docSubgoals' = foldr ((<@>) . definition) empty

------------------------------------------------------------------------------
-- Refutes.
------------------------------------------------------------------------------

-- | Extract refuting steps from a list of formulae.
getRefutes ∷ ProofMap → [F] → [F]
getRefutes dict tstp = map (\tag → fromJust (Map.lookup tag dict)) names
  where
    refutes ∷ [F]
    refutes = filter (isPrefixOf "refute" . name) tstp

    refutesID ∷ [(Int, Int)]
    refutesID = map (\ r → extractRefuteId (name r)) refutes

    numRefutes ∷ [Int]
    numRefutes = nub $ map fst refutesID

    names ∷ [ String ]
    names = [ "refute-" ++ show n ++ "-" ++ show k | (n,k) <- rootRefutes ]

    rootRefutes ∷ [ (Int, Int) ]
    rootRefutes = [ maximum (filter (\ l -> (fst l == r)) refutesID) | r ← numRefutes ]

    extractRefuteId ∷ String → (Int, Int)
    extractRefuteId ref =
          let nid = drop 7 ref
          in ( read (takeWhile (/= '-') nid) :: Int
             , read (tail (dropWhile (/= '-') nid)) :: Int)

------------------------------------------------------------------------------
-- Proof.
------------------------------------------------------------------------------

subgoalName ∷ Int → Doc
subgoalName n = pretty "subgoal" <> (pretty . stdName . show) n

docProof ∷ AgdaFile → Doc
docProof agdaFile =
     hypenline
  <> comment (pretty "Proof" <> dot)
  <> hypenline
  <@> vsep
       [ docProofSubgoals agdaFile
      , docProofGoal agdaFile  -- TODO
       ]

docProofSubgoals ∷ AgdaFile → Doc
docProofSubgoals agdaFile =
  vsep $
    map
      (\(n, tree) → docProofSubgoal n tree agdaFile)
      (zip [0..] trees)
  where
    trees ∷ [ProofTree]
    trees = fileTrees agdaFile

docProofSubgoal ∷ Int → ProofTree → AgdaFile → Doc
docProofSubgoal n tree agdaFile =
     pName <+> colon <+> pretty "Γ ⊢" <+> pretty (subgoalName n) <> line
  <> pName <+> equals <> line
  <> indent 2 (parens (pretty "RAA" <> line <>
              indent 2 (docSteps n tree agdaFile))) <> line
  where
    pName ∷  Doc
    pName = pretty "proof" <> (pretty . stdName . show) n

docProofGoal ∷ AgdaFile → Doc
docProofGoal agdaFile =
     pretty "proof" <+> colon <+> pretty "Γ ⊢ goal" <> line
  <> pretty "proof" <+> equals <> line
  <> indent 2 (pretty "⇒-elim" <> line)
  <> indent 2 (pretty "atp-split" <> line <> nestedproofs) <> line
    where
      nestedproofs :: Doc
      nestedproofs =
        atpSplit
          (getFormula agdaFile "goal")
          (map formula (fileSubgoals agdaFile))

  -- where
  --   sgoals ∷ Doc
  --   sgoals = case fileSubgoals agdaFile of
  --     []       → pretty '?' -- TODO
  --     [_]      → pretty "proof₀"
  --     [_, _]   → parens $ pretty "∧-intro"
  --             <+> pretty "proof₀"
  --             <+> pretty "proof₁"
  --     subgoals →
  --       foldl
  --         (\x y →
  --           parens $
  --             vsep
  --               [ pretty "∧-intro"
  --               , indent 2
  --                   (vsep
  --                     [ x
  --                     , pretty "proof" <> y
  --                     ]
  --                    )
  --                ]
  --         )
  --         (pretty "proof₀")
  --         (map (pretty . stdName . show)  [1..(length subgoals - 1)])

------------------------------------------------------------------------------

-- | docSteps generates the document with the proof making recursion
-- over the deduction tree.

docSteps ∷ Int        -- ^ The number of the subgoal.
         → ProofTree  -- ^ Deduction tree.
         → AgdaFile   -- ^ Agda file.
         → Doc        -- ^ Doc of the step.

------------------------------------------------------------------------------
-- Leafs.
------------------------------------------------------------------------------

docSteps _ (Leaf Conjecture conjecture) _ = pretty conjecture

docSteps subgoalN (Leaf _ axiom) agdaFile =
  parens $
       prettyWeaken <> line
    <> indent 2 (parens prettyAssume)
  where

    dict ∷ ProofMap
    dict = fileDict agdaFile

    pAxiom ∷ Doc
    pAxiom = pretty . stdName $ axiom

    premises ∷ [F]
    premises = filePremises agdaFile

    aᵢ ∷ F
    aᵢ = fromJust . Map.lookup axiom $ dict

    toWeak ∷ [F]
    toWeak =
      case dropWhile (/= aᵢ) premises of
        []  → []
        [_] → []
        ps  → tail ps

    prettyWeaken ∷ Doc
    prettyWeaken =
      case toWeak of
        [] → pretty "weaken" <+> parens (pretty "¬" <+> subgoalName subgoalN)
        ps → pretty "weaken-Δ₁" <> line <>
          indent 2 (parens (toCtxt (
                [pretty '∅']
             ++ map pretty ps
             ++ [pretty "¬" <+> subgoalName subgoalN])))

    toAssume ∷ [F]
    toAssume = takeWhile (/= aᵢ) premises

    prettyAssume ∷ Doc
    prettyAssume =
      case toAssume of
        []  → pretty "assume {Γ = ∅}" <+> pAxiom
        [a] → pretty "assume {Γ = [" <+> pretty a <+> pretty "]}" <+> pAxiom
        axs → pretty "assume" <> line
          <> indent 2
          (pretty "{Γ = ∅ ," <+> toCtxt (map pretty axs) <> pretty "}" <+> pAxiom)


------------------------------------------------------------------------------
-- Canonicalize.
------------------------------------------------------------------------------

docSteps subgoalN (Root Canonicalize tag [subtree]) agdaFile =
  parens $
       pretty Canonicalize <+> getFormulaByTag agdaFile tag <> line
    <> indent 2 (docSteps subgoalN subtree agdaFile)

------------------------------------------------------------------------------
-- Clausify.
------------------------------------------------------------------------------

docSteps subgoalN (Root Clausify tag [subtree]) agdaFile =
  parens $
       pretty Clausify <+> getFormulaByTag agdaFile tag <> line
       <> indent 2 (docSteps subgoalN subtree agdaFile)

------------------------------------------------------------------------------
-- Conjunct.
------------------------------------------------------------------------------

docSteps subgoalN (Root Conjunct tag [subtree]) agdaFile =
   parens $
        pretty Conjunct <+> getFormulaByTag agdaFile tag <> line
     <> indent 2 (docSteps subgoalN subtree agdaFile)

------------------------------------------------------------------------------
-- Negate.
------------------------------------------------------------------------------

docSteps subgoalN (Root Negate _ [subtree@(Root Strip _ _)]) agdaFile =
  parens $ pretty "assume {Γ = Γ}" <> line
    <> indent 2 (parens (pretty "¬" <+> docSteps subgoalN subtree agdaFile))

------------------------------------------------------------------------------
-- Resolve.
------------------------------------------------------------------------------

{-
     left         right
   ────────     ──────────
   f: ϕ₁ ∨ ℓ    g: ϕ₂ ∨ ¬ ℓ
   ────────────────────────  (resolve ℓ)
        φ: ϕ₁ ∨ ϕ₂

-}

docSteps subgoalN
         (Root Resolve tag
           [ left@(Root _ fTag _)
           , right@(Root _ gTag _)
           ])
         agdaFile =
  parens $ pretty Resolve <+> getFormulaByTag agdaFile tag
  <+> pretty "-- " <> pretty f <+> pretty g <+> pretty thm <+> pretty swap
      <> line <>
      indent 2 ( pretty l <> line <>
    if swap
    then (docSteps subgoalN right agdaFile  <> line <>
         docSteps subgoalN left agdaFile)
    else (docSteps subgoalN left agdaFile <> line <>
            docSteps subgoalN right agdaFile))
  where
    dict ∷ ProofMap
    dict = fileDict agdaFile

    -- ϕ  ∷ Formula
    -- ϕ = formula . fromJust $ Map.lookup tag dict

    f , g ∷ Formula
    f = formula . fromJust $ Map.lookup fTag dict
    g = formula . fromJust $ Map.lookup gTag dict

    thm ∷ String
    swap ∷ Bool
    (thm, swap) = atpResolve f g l

    l ∷ Formula
    l = let sourceInfo ∷ Source
            sourceInfo = source . fromJust $ Map.lookup tag dict
        in  getResolveLiteral sourceInfo

    getResolveLiteral ∷ Source → Formula
    getResolveLiteral
      (Inference Resolve (Function _ (GTerm (GWord lit):_) :_) _) =
        PredApp lit []
    getResolveLiteral _ = error "I expected a literal, nothing more."

------------------------------------------------------------------------------
-- Simplify.
------------------------------------------------------------------------------

docSteps subgoalN (Root Simplify tag nodes) agdaFile =
  simplification
  where
    rNodes :: [ProofTree]
    rNodes = case nodes of
      []       → []
      [x]      → [x]
      [x,y]    → [x,y]
      (x:y:ys) → reverse ys ++ [x, y]

    simplification :: Doc
    simplification =
      foldr
        (\node y →
          parens $
            vsep
              [ pretty Simplify <+> getFormulaByTag agdaFile tag
              , indent 2
                  (vsep
                    [ docSteps subgoalN node agdaFile
                    , y
                    ]
                   )
               ]
        )
        (docSteps subgoalN (last rNodes) agdaFile)
        (init rNodes)

------------------------------------------------------------------------------
-- Strip.
------------------------------------------------------------------------------

docSteps subgoalN (Root Strip _ _) _ = subgoalName subgoalN

docSteps _ (Root Skolemize _ _ ) _   = pretty "? -- skolemie"
docSteps _ (Root Specialize _ _ ) _  = pretty "? -- specialize"
docSteps _ (Root (NewRule r) _ _ ) _ = pretty "? -- newrule"

-- docSteps _ _ _ = pretty "?" -- pretty inf <+> pretty r <> line
