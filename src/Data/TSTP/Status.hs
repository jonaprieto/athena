-- | Data.TSTP.Status module.
-- Adapted from https://github.com/agomezl/tstp2agda.

module Data.TSTP.Status
  ( module Data.TSTP.Status
  ) where

------------------------------------------------------------------------------

-- NOT BEING USED YET
data Status = Cax
            | Ceq
            | Csa
            | Csp
            | Cth
            | Cup
            | Ecs
            | Ect
            | Eqv
            | Esa
            | Eth
            | Fsa
            | Fun
            | Noc
            | Sap
            | Sat
            | Sca
            | Scc
            | Suc
            | Tac
            | Tau
            | Tca
            | Thm
            | Uca
            | Unc
            | Unk
            | Unp
            | Uns
            | Wca
            | Wcc
            | Wct
            | Wec
            | Wtc
            | Wth
            | Wuc
            deriving (Eq, Ord, Read, Show)
