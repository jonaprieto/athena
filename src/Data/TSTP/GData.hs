
-- | Data.TSTP.GData module.
-- Adapted from https://github.com/agomezl/tstp2agda.

module Data.TSTP.GData where

------------------------------------------------------------------------------

import Data.TSTP.AtomicWord ( AtomicWord(..) )
import Data.TSTP.Formula    ( Formula(..) )
import Data.TSTP.Term       ( Term(..) )
import Data.TSTP.V          ( V(..) )

------------------------------------------------------------------------------
-- | Metadata (the /general_data/ rule in
--   <http://www.cs.miami.edu/~tptp/ TPTP>'s grammar)
data GData = GApp AtomicWord [GTerm]
           | GDistinctObject String
           | GFormulaData String Formula
           | GFormulaTerm String Term
           | GNumber Rational
           | GVar V
           | GWord AtomicWord
           deriving (Eq, Ord, Show, Read)

-- | Metadata (the /general_term/ rule in
--   <http://www.cs.miami.edu/~tptp/ TPTP>'s grammar)
data GTerm = ColonSep GData GTerm
           | GList [GTerm]
           | GTerm GData
           deriving (Eq, Ord, Show, Read)
