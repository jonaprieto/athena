-- | Data.TSTP.Source module.
-- Adapted from https://github.com/agomezl/tstp2agda.

module Data.TSTP.Source
  ( Source
    ( Creator
    , File
    , Inference
    , Introduced
    , Name
    , NoSource
    , Source
    , Theory
    )
  , Info
    ( AssumptionR
    , Description
    , Function
    , InferenceInfo
    , IQuote
    , Refutation
    , Status
    )
  )
  where

------------------------------------------------------------------------------

import Data.TSTP.GData     ( GTerm(..) )
import Data.TSTP.IntroType ( IntroType(..) )
import Data.TSTP.Parent    ( Parent(..) )
import Data.TSTP.Rule      ( Rule(..) )
import Data.TSTP.Status    ( Status(..) )
import Data.TSTP.Theory    ( Theory(..) )

------------------------------------------------------------------------------

-- | 'Source' main purpose is to provide all the information regarding
-- the deductive process that lead to a given formula. Information
-- about the rules applied along with parent formulas and
-- status are among the information you might expect from this field.
data Source = Creator String [Info]
            | File String (Maybe String)
            | Inference Rule [Info] [Parent]
            | Introduced IntroType [Info]
            | Name String
            | NoSource
            | Source String
            | Theory Theory [Info]
            deriving (Eq, Ord, Read, Show)

data Info   = AssumptionR [String]
            | Description String
            | Function String [GTerm]
            | InferenceInfo Rule String [GTerm]
            | IQuote String
            | Refutation Source
            | Status Status
            deriving (Eq, Ord, Read, Show)
