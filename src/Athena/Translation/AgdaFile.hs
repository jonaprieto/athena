
-- | Athena.Translation.AgdaFile module.

module Athena.Translation.AgdaFile
  ( AgdaFile
     ( AgdaFile
     , fileVariables
     , fileAxioms
     , fileConjecture
     , fileInfo
     , filePremises
     , fileSubgoals
     , fileTree
     )
  )
  where

------------------------------------------------------------------------------

import Athena.Translation.Functions
  ( docAxioms
  , docConjecture
  , docImports
  , docPremises
  , docSubgoals
  , docVars
  )
import Athena.Utils.PrettyPrint
  ( Pretty(pretty)
  , vsep
  )

import Data.Proof
  ( ProofMap
  , ProofTree
  )
import Data.TSTP
  ( F
  , V
  )

------------------------------------------------------------------------------


-- | Agda file.
data AgdaFile = AgdaFile
  { fileAxioms     :: [F]
  , fileConjecture :: F
  , fileInfo       :: ProofMap
  , filePremises   :: [F]
  , fileSubgoals   :: [F]
  , fileTree       :: [ProofTree]
  , fileVariables  :: [V]
  }

instance Pretty AgdaFile where
  pretty problem =
   vsep
     [ docImports (length (fileVariables problem))
     , docVars (fileVariables problem)
     , docAxioms (fileAxioms problem)
     , docPremises (filePremises problem)
     , docConjecture (fileConjecture problem)
     , docSubgoals (fileSubgoals problem)
     ]
