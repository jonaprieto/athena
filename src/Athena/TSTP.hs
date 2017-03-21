-- | TSTP module.

{-# LANGUAGE UnicodeSyntax #-}

module TSTP
  ( parse
  , parseFile
  ) where

------------------------------------------------------------------------------

import  Data.TSTP.F

import  TSTP.Lexer  ( alexScanTokens )
import  TSTP.Parser ( parseTSTP )
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
parse = parseTSTP . map snd . alexScanTokens

parseFile ∷ FilePath → IO [F]
parseFile path = parse <$> readFile path
