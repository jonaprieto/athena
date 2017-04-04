-- | Athena.Translation.Functions module.

{-# OPTIONS -fno-warn-missing-signatures  #-}
{-# LANGUAGE UnicodeSyntax                #-}
{-# LANGUAGE ScopedTypeVariables          #-}

module Athena.Translation.Functions
   ( docAxioms
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
   , AgdaFile
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
  , (<@>)
  , (<>)
  -- , (</>)
  -- , hcat
  , colon
  , comma
  , comment
  , Doc
  , braces
  , dollar
  , dot
  , hypen
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
  , Pretty(pretty)
  , rbracket
  , space
  , vsep
  -- , softline
  )
import Athena.Utils.Version      ( progNameVersion )

import Data.Proof
  ( ProofMap
  , ProofTree
  , ProofTreeGen ( Root, Leaf )
  )

import Data.List                ( isPrefixOf, break )
import Data.Maybe               ( fromJust )
import qualified Data.Map as Map

import Data.TSTP
  ( F    ( name, role, formula, source )
  , Formula(..)
  , Info(Function), GData(..), GTerm(..), Source(..)
  , Role ( Axiom, Conjecture )
  , Rule ( Negate, Strip, Conjunct, Canonicalize, Simplify, Resolve )
  )
import Data.TSTP.V              ( V(..) )
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
     [
       docModule (fileName problem)
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
     <> comment (pretty "Tstp file:" <+> pretty (optInputFile opts) <> dot)
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

docProblemComment ∷ Doc
docProblemComment =
     hypenline
  <> comment (pretty "Problem" <> dot)
  <> hypenline

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
    docAxioms' []         = empty
    docAxioms' (fm : fms) =
          definition fm
      <@> docAxioms' fms

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
docSubgoals []  = empty
docSubgoals formulas =
     comment (pretty "Subgoal" <> s <> dot) <> line
  <> docSubgoals' formulas
  where
    s ∷ Doc
    s = if length formulas > 1 then pretty 's' else empty

    docSubgoals' ∷ [F] → Doc
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

------------------------------------------------------------------------------
-- Proof.
------------------------------------------------------------------------------

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
     pName <+> colon <+> pretty "Γ ⊢" <+> pretty sName <> line
  <> pName <+> equals <> line
  <> indent 2  (parens (pretty "RAA" <> line <>
              indent 2 (docSteps sName tree agdaFile))) <> line
  where
    pName ∷  Doc
    pName = pretty "proof" <> (pretty . stdName . show) n

    sName ∷ Doc
    sName = pretty "subgoal" <> (pretty . stdName . show) n

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
        foldr
          (\x y →
            parens $
              vsep
                [ pretty "∧-intro"
                , indent 2
                    (vsep
                      [ pretty "proof" <> (pretty . stdName . show) x
                      , y
                      ]
                     )
                 ]
          )
          (pretty "proof" <> (pretty . stdName . show) (length subgoals -1))
          [0..(length subgoals - 2)]

------------------------------------------------------------------------------

-- | docSteps generates the document with the proof making recursion
-- over the deduction tree.
docSteps ∷ Doc → ProofTree → AgdaFile → Doc

------------------------------------------------------------------------------
-- Axiom.
------------------------------------------------------------------------------

docSteps sName (Leaf Axiom axiom) agdaFile =
  parens $
      prettyWeaken <> line
    <> indent 2 (parens (prettyAssume <+> pAxiom))
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
        [a] → []
        ps  → tail ps

    prettyWeaken ∷ Doc
    prettyWeaken =
      case toWeak of
        [] → pretty "weaken"  <+> parens (pretty Negate <+> sName)
        ps → pretty "weaken-\916\8321" <+> parens
          (toCtxt
           ([pretty '\8709'] ++ map pretty ps ++ [pretty Negate <+> sName]))

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

docSteps sName (Root Canonicalize _ [subtree]) agdaFile =
  parens $
       pretty Canonicalize <> line
    <> indent 2 (docSteps sName subtree agdaFile)

------------------------------------------------------------------------------
-- Conjecture.
------------------------------------------------------------------------------

docSteps sName (Leaf Conjecture conjecture) _ =
  pretty conjecture

------------------------------------------------------------------------------
-- Conjunct.
------------------------------------------------------------------------------

docSteps sName (Root Conjunct tag [subtree]) agdaFile =
   parens $
        pretty Conjunct <+> parens (pretty ω) <> line
     <> indent 2 (docSteps sName subtree agdaFile)
   where
     ω ∷ Formula
     ω = formula . fromJust $ Map.lookup tag dict

     dict ∷ ProofMap
     dict = fileDict agdaFile

------------------------------------------------------------------------------
-- Negate.
------------------------------------------------------------------------------

docSteps sName (Root Negate tag [Root Strip _ _]) agdaFile =
  parens $
       pretty Strip <> line
    <> indent 2 (parens (pretty "assume {Γ = Γ}" <> line
    <> indent 2 (parens (pretty Negate <+> sName))))

------------------------------------------------------------------------------
-- Resolve.
------------------------------------------------------------------------------

{-
      left    right
     -----    -----
      (f)      (g)
      _|_      _|_
     /   \    /    \
     ϕ₁ ∨ l  ϕ₂ ∨ ¬ l
     ---------------- (resolve ℓ)
         ϕ₁ ∨ ϕ₂
         \____/
            |
            ϕ
-}

docSteps sName
         (Root Resolve tag
           [ left@(Root _ fTag _)
           , right@(Root _ gTag _)
           ])
         agdaFile =
  parens $
    pretty thm <+> parens (pretty l) <> line <>
  if swap then
    indent 2 (parens (docSteps sName left agdaFile)) <> line <>
      indent 2 (parens (docSteps sName right agdaFile))
    else
    indent 2 (parens (docSteps sName right agdaFile)) <> line <>
      indent 2 (parens (docSteps sName left agdaFile))

  where
    dict ∷ ProofMap
    dict = fileDict agdaFile

    ϕ  ∷ Formula
    ϕ = formula . fromJust $ Map.lookup tag dict

    f , g ∷ Formula
    f = formula . fromJust $ Map.lookup fTag dict
    g = formula . fromJust $ Map.lookup gTag dict

    thm ∷ String
    swap ∷ Bool
    (thm, swap) = atpResolve f g l

    l ∷ Formula
    l = let sourceInfo ∷ Source
            sourceInfo = source . fromJust $ Map.lookup tag dict
        in getResolveLiteral sourceInfo

    getResolveLiteral ∷ Source → Formula
    getResolveLiteral
      (Inference Resolve (Function _ (GTerm (GWord l):_) :_) _) =
        PredApp l []
    getResolveLiteral _ = error "I expected a literal, nothing more."

------------------------------------------------------------------------------
-- Simplify.
------------------------------------------------------------------------------

docSteps sName (Root Simplify tag nodes) agdaFile =
  simplification
  where
    rNodes :: [ProofTree]
    rNodes = reverse nodes

    simplification :: Doc
    simplification =
      foldr
        (\node y →
          parens $
            vsep
              [ pretty Simplify
              , indent 2
                  (vsep
                    [ docSteps sName node agdaFile
                    , y
                    ]
                   )
               ]
        )
        (docSteps sName (last rNodes) agdaFile)
        (init rNodes)


------------------------------------------------------------------------------
-- Strip.
------------------------------------------------------------------------------

docSteps sName (Root Strip _ [subtree]) agdaFile =
     pretty Strip <> line
  <> indent 2 (parens (docSteps sName subtree agdaFile))

docSteps n tree agdaFile = pretty "?" <> line
