
-- | Athena.TSTP.Base module.

{-# OPTIONS_HADDOCK hide   #-}
{-# LANGUAGE UnicodeSyntax #-}

module Athena.TSTP.Base where

------------------------------------------------------------------------------

import Data.Set ( toList )

import Data.TSTP
  ( AtomicWord(..)
  , BinOp
  , Formula(..)
  , freeVarsF
  , IntroType
      ( Assumption_
      , AxiomOfChoice
      , Definition_
      , Tautology
      , UnknownType
      )
  , Quant (All)
  , Role
      ( Assumption
      , Axiom
      , Conjecture
      , Definition
      , FiDomain
      , FiFunctors
      , FiPredicates
      , Hypothesis
      , Lemma
      , NegatedConjecture
      , Plain
      , Theorem
      , Type
      , Unknown
      )
  , Rule
    ( Canonicalize
    , Conjunct
    , Negate
    , NewRule
    , Resolve
    , Simplify
    , Skolemize
    , Specialize
    , Strip
    )
  , Status(..)
  , V
  )

------------------------------------------------------------------------------

univquantFreeVars ∷ Formula → Formula
univquantFreeVars cnf = Quant All freeVars cnf
    where
      freeVars ∷ [V]
      freeVars = toList $ freeVarsF cnf

readRole ∷ String → Role
readRole "assumption"         = Assumption
readRole "axiom"              = Axiom
readRole "conjecture"         = Conjecture
readRole "definition"         = Definition
readRole "fi_domain"          = FiDomain
readRole "fi_functors"        = FiFunctors
readRole "fi_predicates"      = FiPredicates
readRole "hypothesis"         = Hypothesis
readRole "lemma"              = Lemma
readRole "negated_conjecture" = NegatedConjecture
readRole "plain"              = Plain
readRole "theorem"            = Theorem
readRole "type"               = Type
readRole _                    = Unknown

binOp ∷ BinOp → Formula → Formula → Formula
binOp op f1 = BinOp f1 op

readRule ∷ String → Rule
readRule "canonicalize" = Canonicalize
readRule "conjunct"     = Conjunct
readRule "negate"       = Negate
readRule "resolve"      = Resolve
readRule "simplify"     = Simplify
readRule "skolemize"    = Skolemize
readRule "specialize"   = Specialize
readRule "strip"        = Strip
readRule str            = NewRule str

readType ∷ String → IntroType
readType "assumption"      = Assumption_
readType "axiom_of_choice" = AxiomOfChoice
readType "definition"      = Definition_
readType "tautology"       = Tautology
readType _                 = UnknownType

readWord ∷ AtomicWord → String
readWord (AtomicWord a) = func a
  where
    func ∷ String → String
    func = map repl
        where
          repl ∷ Char → Char
          repl '_' = '-'
          repl b   = b

readStatus ∷ String → Status
readStatus "cax" = Cax
readStatus "ceq" = Ceq
readStatus "csa" = Csa
readStatus "csp" = Csp
readStatus "cth" = Cth
readStatus "cup" = Cup
readStatus "ecs" = Ecs
readStatus "ect" = Ect
readStatus "eqv" = Eqv
readStatus "esa" = Esa
readStatus "eth" = Eth
readStatus "fsa" = Fsa
readStatus "fun" = Fun
readStatus "noc" = Noc
readStatus "sap" = Sap
readStatus "sat" = Sat
readStatus "sca" = Sca
readStatus "scc" = Scc
readStatus "suc" = Suc
readStatus "tac" = Tac
readStatus "tau" = Tau
readStatus "tca" = Tca
readStatus "thm" = Thm
readStatus "uca" = Uca
readStatus "unc" = Unc
readStatus "unp" = Unp
readStatus "uns" = Uns
readStatus "wca" = Wca
readStatus "wcc" = Wcc
readStatus "wct" = Wct
readStatus "wec" = Wec
readStatus "wtc" = Wtc
readStatus "wth" = Wth
readStatus "wuc" = Wuc
readStatus _     = Unk
