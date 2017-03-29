
-- | Athena.TSTP module.
-- Adapted from https://github.com/agomezl/tstp2agda.

{-# LANGUAGE UnicodeSyntax #-}

module Athena.TSTP
  ( parse
  , parseFile
  ) where

------------------------------------------------------------------------------

import  Athena.TSTP.Lexer  ( alexScanTokens )
import  Athena.TSTP.Parser ( parseTSTP )

import  Data.TSTP.F

------------------------------------------------------------------------------

-- | Parse a TSTP file and return a list of `F` formulas in no
-- particular order, for example:
--
-- @
--   $ cat examples\/proof\/Basic-1.tstp
--   fof(a1, axiom, (a)).
--   fof(a2, axiom, (b)).
--   fof(a3, axiom, ((a & b) => z)).
--   ...
-- @
--
-- would be:
--
-- @
--   [
--     F {name = "a1", role = Axiom, formula = a, source = NoSource},
--     F {name = "a2", role = Axiom, formula = b, source = NoSource},
--     F {name = "a3", role = Axiom, formula = ( a ∧ b → z ), source = NoSource},
--     ...
--   ]
-- @

parse ∷ String → [F]
parse = parseTSTP . fmap snd . alexScanTokens

parseFile ∷ FilePath → IO [F]
parseFile path = parse <$> readFile path
