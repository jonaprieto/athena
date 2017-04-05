
-- | Data.TSTP.Role module.
-- Adapted from https://github.com/agomezl/tstp2agda.

module Data.TSTP.Role where

------------------------------------------------------------------------------

-- | Formula roles.
data Role = Assumption
          | Axiom
          | Conjecture
          | Definition
          | FiDomain
          | FiFunctors
          | FiPredicates
          | Hypothesis
          | Lemma
          | NegatedConjecture
          | Plain
          | Theorem
          | Type
          | Unknown
          deriving (Eq, Ord, Read, Show)
