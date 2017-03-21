
-- | Data.Proof module

{-# LANGUAGE UnicodeSyntax #-}

module Data.Proof
  ( -- * Types
    ProofTreeGen(..)
  , ProofMap
  , ProofTree
  , IdSet
  -- * Constructors
  , buildProofMap
  , buildProofTree
  -- * Internals
  , getParents
  , getParentsTree
  , unknownTree
  ) where

------------------------------------------------------------------------------

import Data.Map      ( empty, insert )
import Data.Map as M ( lookup )
import Data.Maybe    ( mapMaybe )


import Data.Proof.ProofTreeGen
  ( ProofTreeGen(..)
  , ProofMap
  , ProofTree
  , IdSet
  )

import Data.TSTP
  ( F (..)
  , Parent (..)
  , Role (..)
  , Source (..)
  )

------------------------------------------------------------------------------

-- | 'buildProofTree' 'm' 'f', build a 'ProofTree' with 'f' as root,
-- and using 'm' for dependencies resolution. Depending on the root,
-- not all values in 'm' are used.
buildProofTree ∷ ProofMap     -- ^ 'Map' for resolving dependencies
               → F            -- ^ Root formula
               → ProofTree    -- ^ Tree of formulas with the given
                              -- formula as root
buildProofTree m formulaF =
  let namef ∷ String
      namef = name formulaF
  in case role formulaF of
    Axiom       → Leaf Axiom namef
    Conjecture  → Leaf Conjecture namef
    Plain       → case source formulaF of
      (Inference r _ p) → Root r namef (getParentsTree m p)
      sname             → unknownTree "Source" sname namef
    rname       → unknownTree "Role" rname namef

-- | 'buildProofMap' 'lf', given a list of functions 'lf' builds a 'ProofMap'
buildProofMap ∷ [F]      -- ^ List of functions
              → ProofMap -- ^ Map of the given functions indexed by its names
buildProofMap = foldl buildMap empty
    where
      -- buildMap ∷ Map String F → F → Map String F
      buildMap m f' = insert (name f') f' m

-- | 'getParentsTree' 'm' 'p', from a 'Map' 'm' and a list of parents 'p'
-- return a list of corresponding parent subtrees.
getParentsTree ∷ ProofMap    -- ^ 'Map'
               → [Parent]    -- ^ List of parents
               → [ProofTree] -- ^ List of parents subtrees
getParentsTree m p = map (buildProofTree m) $ getParents m p

-- | 'getParents' 'm' 'p', from a 'Map' 'm' and a list of parents 'p'
-- returns a list of corresponding parent formulas.
getParents ∷ ProofMap -- ^ 'Map'
           → [Parent] -- ^ List of 'Parents
           → [F]      -- ^ List of parent formulas
getParents m p = mapMaybe (`M.lookup` m) parents
    where
      parents ∷ [String]
      parents = map (\(Parent s _) → s) p

-- | When an unknown 'Rule', 'Source', or other unexpected data type
-- is found a 'Leaf' With an 'Unknown' 'Role' and error message is
-- created.
unknownTree ∷ (Show a) ⇒
              String     -- ^ Description of the unexpected data type
            → a          -- ^ Unexpected data
            → String     -- ^ Formula name
            → ProofTree  -- ^ 'Unknown' node
unknownTree m r n = Leaf Unknown $ m ++  ' ':show r  ++ " in " ++ n
