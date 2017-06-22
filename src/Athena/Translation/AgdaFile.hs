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
--   -- , atpStrip
  )
import Athena.Options            ( Options ( optInputFile ) )
import Athena.Translation.Utils  ( stdName )
import Athena.Utils.PrettyPrint
  ( (<+>)
  , (<>)
  , (<@>)
  , Doc
  , Pretty(pretty)
  , braces
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

import Data.List                ( isPrefixOf)
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
getRefutes ∷ [F] → [F]
getRefutes = filter (isPrefixOf "refute" . name)

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
       , docProofGoal agdaFile
       ]

docProofSubgoal ∷ Int → ProofTree → AgdaFile → Doc
docProofSubgoal n tree agdaFile =
     pName <+> colon <+> pretty "Γ ⊢" <+> pretty (subgoalName n) <> line
  <> pName <+> equals <> line
  <> indent 2  (parens (pretty "RAA" <> line <>
              indent 2 (docSteps n tree agdaFile))) <> line
  where
    pName ∷  Doc
    pName = pretty "proof" <> (pretty . stdName . show) n

docProofSubgoals ∷ AgdaFile → Doc
docProofSubgoals agdaFile =
  vsep $
    map
      (\(n, tree) → docProofSubgoal n tree agdaFile)
      (zip [0..] trees)
  where
    trees ∷ [ProofTree]
    trees = fileTrees agdaFile

docProofGoal ∷ AgdaFile → Doc
docProofGoal agdaFile =
     pretty "proof" <+> colon <+> pretty "Γ ⊢ goal" <> line
  <> pretty "proof" <+> equals <> line
  <> indent 2 (pretty "⇒-elim" <> line)
  <> indent 2 (pretty "atp-splitGoal" <> line)
  <> indent 0 sgoals <> line
  where
    sgoals ∷ Doc
    sgoals = case fileSubgoals agdaFile of
      []       → pretty '?' -- TODO
      [_]      → pretty "proof₀"
      [_, _]   → parens $ pretty "∧-intro"
              <+> pretty "proof₀"
              <+> pretty "proof₁"
      subgoals →
        foldl
          (\x y →
            parens $
              vsep
                [ pretty "∧-intro"
                , indent 2
                    (vsep
                      [ x
                      , pretty "proof" <> y
                      ]
                     )
                 ]
          )
          (pretty "proof₀")
          (map (pretty . stdName . show)  [1..(length subgoals - 1)])

------------------------------------------------------------------------------

-- | docSteps generates the document with the proof making recursion
-- over the deduction tree.

docSteps ∷ Int        -- ^ The number of the subgoal.
         → ProofTree  -- ^ Deduction tree.
         → AgdaFile   -- ^ Agda file.
         → Doc        -- ^ Doc of the step.

------------------------------------------------------------------------------
-- Axiom.
------------------------------------------------------------------------------

docSteps subgoalN (Leaf Axiom axiom) agdaFile =
  parens $
       prettyWeaken <> line
    <> indent 2 (parens $ prettyAssume <+> pAxiom)
  where

    dict ∷ ProofMap
    dict = fileDict agdaFile

    pAxiom ∷ Doc
    pAxiom = pretty . stdName $ axiom

    premises ∷ [F]
    premises = filePremises agdaFile

    aᵢ ∷ F
    aᵢ = fromJust .  Map.lookup axiom $ dict

    toWeak ∷ [F]
    toWeak =
      case dropWhile (/= aᵢ) premises of
        []  → []
        [_] → []
        ps  → tail ps

    prettyWeaken ∷ Doc
    prettyWeaken =
      case toWeak of
        [] → pretty "weaken" <+>
          parens (pretty Negate <+> subgoalName subgoalN)
        ps → pretty "weaken-Δ₁" <> line <>
          indent 2 (parens (toCtxt (
                [pretty '∅']
             ++ map pretty ps
             ++ [pretty Negate <+> subgoalName subgoalN])))

    toAssume ∷ [F]
    toAssume = takeWhile (/= aᵢ) premises

    prettyAssume ∷ Doc
    prettyAssume = pretty "assume" <+> pretty "{Γ =" <+>
      case toAssume of
        []  → pretty "∅}"
        [a] → lbracket <+> pretty a <+> rbracket <> pretty "}"
        axs → braces $ pretty 'Γ' <+> equals <+> toCtxt (map pretty axs)

------------------------------------------------------------------------------
-- Canonicalize.
------------------------------------------------------------------------------

docSteps subgoalN (Root Canonicalize _ [subtree]) agdaFile =
  parens $
       pretty Canonicalize <> line
    <> indent 2 (docSteps subgoalN subtree agdaFile)

------------------------------------------------------------------------------
-- Conjecture.
------------------------------------------------------------------------------

docSteps _ (Leaf Conjecture conjecture) _ = pretty conjecture

------------------------------------------------------------------------------
-- Clausify.
------------------------------------------------------------------------------

docSteps subgoalN (Root Clausify _ [subtree]) agdaFile =
  parens $
       pretty Clausify <> line
    <> indent 2 (docSteps subgoalN subtree agdaFile)

------------------------------------------------------------------------------
-- Conjunct.
------------------------------------------------------------------------------

docSteps subgoalN (Root Conjunct tag [subtree]) agdaFile =
   parens $
        pretty Conjunct <+> parens (pretty ω) <> line
     <> indent 2 (docSteps subgoalN subtree agdaFile)
   where
     ω ∷ Formula
     ω = formula . fromJust $ Map.lookup tag dict

     dict ∷ ProofMap
     dict = fileDict agdaFile

------------------------------------------------------------------------------
-- Negate.
------------------------------------------------------------------------------

docSteps subgoalN (Root Negate _ [subtree@(Root Strip _ _)]) agdaFile =
  parens $ pretty "assume {Γ = Γ}" <> line
        <> indent 2 (parens (pretty Negate <> line <>
                    indent 2 (docSteps subgoalN subtree agdaFile)))

------------------------------------------------------------------------------
-- Resolve.
------------------------------------------------------------------------------

{-
     left         right
   ────────     ──────────
   f: ϕ₁ ∨ ℓ    g:ϕ₂ ∨ ¬ ℓ
   ────────────────────────  (resolve ℓ)
        φ: ϕ₁ ∨ ϕ₂

-}

docSteps subgoalN
         (Root Resolve tag
           [ left@(Root _ fTag _)
           , right@(Root _ gTag _)
           ])
         agdaFile =
  parens $
    pretty thm <+> parens (pretty l) <> line <>
    if swap
    then indent 2 (docSteps subgoalN left agdaFile)  <> line
      <> indent 0 (docSteps subgoalN right agdaFile)
    else indent 2 (docSteps subgoalN right agdaFile) <> line
      <> indent 0 (docSteps subgoalN left agdaFile)
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

docSteps subgoalN (Root Simplify _ nodes) agdaFile =
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
              [ pretty Simplify
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

docSteps subgoalN (Root Strip _ _) _ =
     parens $ pretty "strip" <+> subgoalName subgoalN

docSteps _ _ _ = pretty "?" <> line
