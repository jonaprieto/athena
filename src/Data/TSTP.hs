-- | Data.TSTP module.
-- Adapted from https://github.com/agomezl/tstp2agda.

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UnicodeSyntax        #-}

module Data.TSTP
  ( F(..)
  , Role(..)
  -- * Formulas and terms
  , Formula(..)
  , Term(..)
  , AtomicWord(..)
  , BinOp(..)
  , InfixPred(..)
  , Quant(..)
  , V(..)
  -- * Source information
  , Parent(..)
  , Rule(..)
  , Source(..)
  -- * Functions
  , bottom
  , freeVarsF
  , freeVarsT
  , getFreeVars
  , isBottom
  -- * Unused types
  -- | The following types are required to have full
  -- support of the TSTP syntax but haven't been used yet
  -- in 'tstp2agda' aside from the parser.
  , GData(..)
  , GTerm(..)
  , Info(..)
  , IntroType(..)
  , Status(..)
  , Theory(..)
  ) where

------------------------------------------------------------------------------

import Data.TSTP.AtomicWord ( AtomicWord(..) )
import Data.TSTP.BinOp      ( BinOp(..) )
import Data.TSTP.F          ( F(..) )
import Data.TSTP.Formula
  ( Formula(..)
  , freeVarsF
  , freeVarsT
  , getFreeVars
  )
import Data.TSTP.GData      ( GData(..), GTerm(..) )
import Data.TSTP.InfixPred  ( InfixPred(..) )
import Data.TSTP.IntroType  ( IntroType(..) )
import Data.TSTP.Parent     ( Parent(..) )
import Data.TSTP.Quant      ( Quant(..) )
import Data.TSTP.Role       ( Role(..) )
import Data.TSTP.Rule       ( Rule(..) )
import Data.TSTP.Source     ( Info(..), Source(..) )
import Data.TSTP.Status     ( Status(..) )
import Data.TSTP.Term       ( Term(..) )
import Data.TSTP.Theory     ( Theory(..) )
import Data.TSTP.V          ( V(..) )

------------------------------------------------------------------------------

-- | 'bottom' = ⊥.
bottom ∷ Formula
bottom = PredApp (AtomicWord "$false") []

-- | 'isBottom' 'f', test whether 'f' = ⊥.
isBottom ∷ F → Bool
isBottom = (==) (PredApp (AtomicWord "$false") []) . formula
