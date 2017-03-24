
-- | Data.TSTP.IntroType module

module Data.TSTP.IntroType where

------------------------------------------------------------------------------

-- NOT BEING USED YET
data IntroType = Assumption_
               | AxiomOfChoice
               | Definition_
               | Tautology
               | UnknownType
               deriving (Eq, Ord, Show, Read)
