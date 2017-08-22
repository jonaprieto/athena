-- | Athena.Translation.Rules module.

module Athena.Translation.Rules
  ( atpCanonicalize
  , atpClausify
  , atpSimplify
  , atpSplit
  ) where

------------------------------------------------------------------------------

import Athena.Translation.Rules.Canonicalize ( atpCanonicalize )
import Athena.Translation.Rules.Clausify     ( atpClausify )
import Athena.Translation.Rules.Simplify     ( atpSimplify )
import Athena.Translation.Rules.Strip        ( atpSplit )

------------------------------------------------------------------------------
