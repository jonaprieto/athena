
-- | Data.TSTP.IntroType module.
-- Adapted from https://github.com/agomezl/tstp2agda.

module Data.TSTP.IntroType where

------------------------------------------------------------------------------

-- NOT BEING USED YET
data IntroType = Assumption_
               | AxiomOfChoice
               | Definition_
               | Tautology
               | UnknownType
               deriving (Eq, Ord, Show, Read)
