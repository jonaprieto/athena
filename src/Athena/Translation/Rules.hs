
-- | Athena.Translation.Rules module.

module Athena.Translation.Rules
  ( atpCanonicalize
  , atpClausify
  , atpConjunct
  , atpNegate
  , atpResolve
  , atpSimplify
  , atpSkolemize
  , atpSpecialize
  , atpStrip
  )
  where

------------------------------------------------------------------------------

import Athena.Translation.Rules.Canonicalize ( atpCanonicalize )
import Athena.Translation.Rules.Clausify     ( atpClausify )
import Athena.Translation.Rules.Conjunct     ( atpConjunct )
import Athena.Translation.Rules.Negate       ( atpNegate )
import Athena.Translation.Rules.Resolve      ( atpResolve )
import Athena.Translation.Rules.Simplify     ( atpSimplify )
import Athena.Translation.Rules.Skolemize    ( atpSkolemize )
import Athena.Translation.Rules.Specialize   ( atpSpecialize )
import Athena.Translation.Rules.Strip        ( atpStrip )

------------------------------------------------------------------------------
