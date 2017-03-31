
-- | Data.Proof.ProofTreeGen module.
-- Adapted from https://github.com/agomezl/tstp2agda.

-- {-# LANGUAGE FlexibleInstances #-}

module Data.Proof.ProofTreeGen where

------------------------------------------------------------------------------

import Data.Map  ( Map )
import Data.Set  ( Set )
import Data.TSTP ( Role, Rule, F )

------------------------------------------------------------------------------

-- | Generic tree structure for representing the structure of a proof.
data ProofTreeGen a =
    -- | 'Leaf' 'r' 'a' is a node with 'Role' 'r' and content 'a' (usually
    -- 'String', 'F' or 'Formula') and with no dependencies in other nodes.
    Leaf Role a
    -- | 'Root' 'r' 'a' 'd' is a node with deduction 'Rule' 'r', content 'a'
    -- (usually 'String', 'F' or 'Formula'),  and dependencies 'd'.
  | Root Rule a [ProofTreeGen a]
  deriving (Eq, Ord, Read, Show)



-- | Simple type for sets of identifiers whit associated scopes
type IdSet = Set (Int,String)

-- | Concrete instance of 'ProofTreeGen' with 'String' as
-- contents. Each node contains the name of a corresponding formula,
-- along with its dependencies.
type ProofTree = ProofTreeGen String

instance Functor ProofTreeGen where
    fmap f (Leaf r a)   = Leaf r (f a)
    fmap f (Root r a t) = Root r (f a) (fmap (fmap f) t)

instance Foldable ProofTreeGen where
    foldr f b (Leaf _ a)   = f a b
    foldr f b (Root _ a t) = f a $ foldr (flip $ foldr f) b t

instance Traversable ProofTreeGen where
    traverse f (Leaf r a)   = Leaf r <$> f a
    traverse f (Root r a t) = Root r <$> f a <*> traverse (traverse f) t

-- | 'Map' from formula names to an 'F' formula type, useful to get more
-- information from a node in a 'ProofTree'.
type ProofMap = Map String F
