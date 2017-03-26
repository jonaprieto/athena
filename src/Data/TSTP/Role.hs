
-- | Data.TSTP.Role module.

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
          deriving (Eq, Ord, Show, Read)
