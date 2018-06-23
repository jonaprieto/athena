-- | Athena.Translation.Functions module.

{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE UnicodeSyntax                #-}
{-# OPTIONS -fno-warn-missing-signatures  #-}

module Athena.Translation.AgdaFile
   (
   AgdaFile
      ( AgdaFile
      , fileAxioms
      , fileConjecture
      , fileDict
      , fileName
      , filePremises
      , fileScriptMode
      , fileSubgoals
      , fileTags
      , fileTrees
      , fileVariables
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

import Athena.Translation.Rules.Strip ( inferSplit, unshunt, split )

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
  , hypenlineQED
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
import qualified Data.Set as Set

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

import System.FilePath ( takeBaseName )

------------------------------------------------------------------------------

-- | Agda file.
data AgdaFile = AgdaFile
  { fileAxioms     ∷ [F]
  , fileConjecture ∷ F
  , fileDict       ∷ ProofMap
  , fileName       ∷ FilePath
  , filePremises   ∷ [F]
  , fileScriptMode ∷ Bool
  , fileSubgoals   ∷ [F]
  , fileTags       ∷ Set.Set String
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
   <@> pretty "open import ATP.Metis" <+> int n <+> pretty "public"
   <> line
   <> pretty "open import Data.PropFormula" <+> int n <+> pretty "public"
   <> line
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
          pretty var <+> colon  <+> pretty "PropFormula" <> line
      <>  pretty var <+> equals <+> prettyVar var n <> line
      <@> docVars' vs (n+1)

------------------------------------------------------------------------------
-- Axioms.
------------------------------------------------------------------------------

definition ∷ F → Doc
definition fm =
     pretty fm <+> colon  <+> pretty "PropFormula" <> line
  <> pretty fm <+> equals <+> pretty formulaF <> line
  where
    formulaF ∷ Formula
    formulaF = formula fm

-- | Extract axioms from a list of formulas.
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

-- | Try to extract a conjecture from a list of formulas. Only one
-- conjecture is allowed.
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

-- | Extract subgoals from a list of formulas.
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

-- | Extract refuting steps from a list of formulas.
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
    rootRefutes = [ maximum (filter (\l -> (fst l == r)) refutesID)
                  | r ← numRefutes ]

    extractRefuteId ∷ String → (Int, Int)
    extractRefuteId ref =
          let nid = drop 7 ref
          in ( read (takeWhile (/= '-') nid) ∷ Int
             , read (tail (dropWhile (/= '-') nid)) ∷ Int)

type Tag = String

extractTagTree ∷ ProofTree → Tag
extractTagTree (Leaf _ tag)   = stdName tag
extractTagTree (Root _ tag _) = stdName tag

extractTagsTree ∷ ProofTree → [Tag]
extractTagsTree (Leaf _ tag)        = [tag]
extractTagsTree (Root _ tag strees) =
  [tag] ++ (concatMap extractTagsTree strees)

removeTagAgdaFile ∷ Tag → AgdaFile → AgdaFile
removeTagAgdaFile tag agdaFile =
  agdaFile {fileTags = Set.delete tag (fileTags agdaFile)}

removeTagsAgdaFile ∷ [Tag] → AgdaFile → AgdaFile
removeTagsAgdaFile [] agdaFile = agdaFile
removeTagsAgdaFile (tag:tags) agdaFile =
  removeTagsAgdaFile tags newAgdaFile
  where
    newAgdaFile ∷ AgdaFile
    newAgdaFile = removeTagAgdaFile tag agdaFile

------------------------------------------------------------------------------
-- Proof.
------------------------------------------------------------------------------

subgoalName ∷ Int → Doc
subgoalName n = pretty "subgoal" <> (pretty . stdName . show) n

docProof ∷ AgdaFile → Doc
docProof agdaFile =
  vsep
    [ docProofSubgoals agdaFile
    , docProofGoal agdaFile
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
     hypenline
  <> comment (pretty "Proof of subgoal"<> (pretty . stdName . show) n <> dot)
  <> hypenline
  <> line
  <> proof
  <> line
  -- <> hypenlineQED
  where
    pName ∷  Doc
    pName = pretty "proof" <> (pretty . stdName . show) n

    proof ∷ Doc
    proof =
      if not (fileScriptMode agdaFile)
        then
          (  pName <+> colon <+> pretty "Γ ⊢" <+> pretty (subgoalName n)
          <> line
          <> pName <+> equals
          <> line
          <> indent 2 (parens (pretty "RAA"
               <> line
               <> indent 2 (docSteps n tree agdaFile)))
          )
        else
          (  docScriptMode n tree agdaFile
          <> line
          <> pName <+> colon <+> pretty "Γ ⊢" <+> pretty (subgoalName n)
          <> line
          <> pName <+> equals <+> pretty "RAA" <+> pretty (extractTagTree tree)
          )

docProofGoal ∷ AgdaFile → Doc
docProofGoal agdaFile =
     hypenline
  <> comment (pretty "Proof of the goal.")
  <> hypenline
  <> line
  <> pretty "proof" <+> colon
       <+> pretty "Γ ⊢ " <> (pretty (fileConjecture agdaFile)) <> line
  <> pretty "proof" <+> equals <> line
  <> indent 2 (pretty "⊃-elim" <> line)
  <> indent 2 (pretty Strip <> line <> nestedproofs) <> line
  -- <> hypenlineQED
    where
      nestedproofs ∷ Doc
      nestedproofs =
        inferSplit
          (formula (fileConjecture agdaFile))
          (map formula (fileSubgoals agdaFile))

------------------------------------------------------------------------------
-- ScriptMode
------------------------------------------------------------------------------

-- docTag ∷ Int → ProofTree → AgdaFile → Doc
-- docTag subgoalN t@(Leaf _ axiom) agdaFile = docSteps subgoalN t agdaFile
-- docTag subgoalN t@(Rule _ tag _) agdaFile = (pretty . stdName) tag
-- docTag _ _ _ = pretty "?"

-- | In script mode, we print step-by-step providing new terms
-- for each step in the TSTP proof.
docTypeStep ∷ Int     -- ^ The number of the subgoal.
            → Tag     -- ^ Tag step.
            → AgdaFile   -- ^ Agda file.
            → Doc        -- ^ Doc of the step.

docTypeStep subgoalN tag agdaFile =
  if fileScriptMode agdaFile
    then
     (   line
     <>  pretty (stdName tag) <+> colon
     <+> pretty "Γ , ¬" <+> (pretty . stdName) ("subgoal" ++ show subgoalN)
     <+> pretty "⊢"
     <+> (pretty (getFormulaByTag agdaFile tag))
     <>  line
     )
    else empty   -- do nothing.

docSubTrees ∷ Int → [ProofTree] → AgdaFile → Doc
docSubTrees subgoalN []          _        = empty
docSubTrees subgoalN (tree : ts) agdaFile =
     (docScriptMode subgoalN tree (removeTagAgdaFile tag agdaFile))
  <> (docSubTrees subgoalN ts (removeTagsAgdaFile rTags agdaFile))
  where
    tag ∷ Tag
    tag = extractTagTree tree

    rTags ∷ [Tag]
    rTags = extractTagsTree tree

-- | Printing the step by annotating the type and the proof-term.
docScriptMode ∷ Int        -- ^ The number of the subgoal.
              → ProofTree  -- ^
              → AgdaFile   -- ^ Agda file.
              → Doc

docScriptMode subgoalN (Root Negate tag _) agdaFile =
  if not (Set.member tag (fileTags agdaFile))
    then empty
    else
      (  docTypeStep subgoalN tag agdaFile
      <> pretty (stdName tag) <+> pretty "="
      <> line <> indent 2
         (   pretty "assume {Γ = Γ}"
         <+> parens (pretty "¬" <+> pretty (subgoalName subgoalN))
         )
      ) <> line

docScriptMode subgoalN (Root Simplify tag subtrees) agdaFile =
  if not (Set.member tag (fileTags agdaFile))
    then empty
    else
      (  (docSubTrees subgoalN subtrees agdaFile)
      <> (  docTypeStep subgoalN tag agdaFile
         <> pretty (stdName tag) <+> pretty "=" <> line
         <> indent 2 (simplification rTags)
         )
      <> line
      )
  where
    rTags ∷ [Tag]
    rTags = map extractTagTree subtrees

    expectedOut ∷ Doc
    expectedOut = pretty Simplify <+> getFormulaByTag agdaFile tag

    simplification ∷ [Tag] → Doc
    simplification tags =
      foldl
        (\x y ->
          parens $
            vsep
              [ expectedOut
              , indent 2
                  (vsep
                    [ pretty x
                    , pretty y
                    ]
                   )
               ]

        )
        (pretty (head rTags))
        (tail rTags)

docScriptMode subgoalN (Root Resolve tag [left, right]) agdaFile =
  if not (Set.member tag (fileTags agdaFile))
    then empty
    else (
      (  (docScriptMode subgoalN left (removeTagAgdaFile tag agdaFile))
      <> (docScriptMode subgoalN right (removeTagsAgdaFile rTags agdaFile))
      <> (  docTypeStep subgoalN tag agdaFile
         <> pretty (stdName tag) <+> pretty "=" <> line
         <> indent 2
            ( ( if fileScriptMode agdaFile then pretty "original-resolve-thm"
                else pretty Resolve) <+> getFormulaByTag agdaFile tag <+> pretty literal
            <+> pretty (extractTagTree left) <+> pretty (extractTagTree right)
            )
         <> line
         )
      )
    )
  where
    rTags ∷ [Tag]
    rTags = [tag] ++ extractTagsTree left

    dict ∷ ProofMap
    dict = fileDict agdaFile

    literal ∷ Formula
    literal = let sourceInfo ∷ Source
                  sourceInfo = source . fromJust $ Map.lookup tag dict
              in  getResolveLiteral sourceInfo

    getResolveLiteral ∷ Source → Formula
    getResolveLiteral
      (Inference Resolve (Function _ (GTerm (GWord lit):_) :_) _) =
        PredApp lit []
    getResolveLiteral _ = error "Athena expected a literal here."

docScriptMode subgoalN (Root rule tag [subtree]) agdaFile =
  if not (Set.member tag (fileTags agdaFile))
    then empty
    else
     ((docScriptMode subgoalN subtree (removeTagAgdaFile tag agdaFile))
      <> ( docTypeStep subgoalN tag agdaFile
           <> pretty (stdName tag) <+> pretty "="
           <> line
           <> indent 2
                (   pretty rule <+> getFormulaByTag agdaFile tag
                <+> pretty (extractTagTree subtree))
           )
      <> line
      )


docScriptMode subgoalN _ agdaFile = empty

------------------------------------------------------------------------------
-- Natural Deduction Style proofs.
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

  parens (prettyWeaken <> line
    <> indent 2
      (parens prettyAssume))
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
        [] → pretty "weaken"
               <+> parens (pretty "¬" <+> subgoalName subgoalN)
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
          (pretty "{Γ = ∅ ," <+> toCtxt (map pretty axs) <> pretty "}"
            <+> pAxiom)

------------------------------------------------------------------------------
-- Canonicalize.
------------------------------------------------------------------------------

docSteps subgoalN (Root Canonicalize tag [subtree]) agdaFile =
  parens (pretty Canonicalize <+> getFormulaByTag agdaFile tag <> line
    <> indent 2 (docSteps subgoalN subtree agdaFile))

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
  parens $
    -- pretty "-- ¬"
    -- <+> parens (getFormulaByTag agdaFile ("subgoal-" ++ show subgoalN))
    -- <> line <>
        pretty "assume {Γ = Γ}"
    <+> parens (pretty "¬" <+> docSteps subgoalN subtree agdaFile)

------------------------------------------------------------------------------
-- Resolve.
------------------------------------------------------------------------------

{-
     left         right
   ────────     ──────────
    ϕ₁ ∨ ℓ        ϕ₂ ∨ ¬ ℓ
   ────────────────────────  resolve ℓ
        φ: ϕ₁ ∨ ϕ₂

-}

docSteps subgoalN (Root Resolve tag [left, right]) agdaFile =
  parens $ pretty Resolve <+> getFormulaByTag agdaFile tag <+> pretty literal
  <> line <> indent 2
    (docSteps subgoalN left agdaFile <> line <>
      docSteps subgoalN right agdaFile)
  where
    dict ∷ ProofMap
    dict = fileDict agdaFile

    literal ∷ Formula
    literal = let sourceInfo ∷ Source
                  sourceInfo = source . fromJust $ Map.lookup tag dict
              in  getResolveLiteral sourceInfo

    getResolveLiteral ∷ Source → Formula
    getResolveLiteral
      (Inference Resolve (Function _ (GTerm (GWord lit):_) :_) _) =
        PredApp lit []
    getResolveLiteral _ = error "Athena expected a literal here."

------------------------------------------------------------------------------
-- Simplify.
------------------------------------------------------------------------------

docSteps subgoalN (Root Simplify tag nodes) agdaFile =
  foldl
    (\x node →
      parens $
        vsep
          [ pretty Simplify <+> getFormulaByTag agdaFile tag
          , indent 2
              (vsep
                [ x
                , docSteps subgoalN node agdaFile
                ]
               )
           ]
    )
    (docSteps subgoalN (head nodes) agdaFile)
    (tail nodes)

------------------------------------------------------------------------------
-- Strip.
------------------------------------------------------------------------------

docSteps subgoalN (Root Strip _ _) _ = subgoalName subgoalN
docSteps _ _ _                       = pretty "? -- not supported."
