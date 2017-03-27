
-- | Athena.Translation.Utils module.

{-# LANGUAGE UnicodeSyntax #-}

module Athena.Translation.Utils
  ( Ident
  , getIdent
  , stdName
  , subIndex
  )
  where

------------------------------------------------------------------------------

import Data.List.Split ( splitOn )

------------------------------------------------------------------------------

type Ident = Int

getIdent ∷ Ident → String
getIdent n = concat $ replicate (2 * n) " "

stdName ∷ String → String
stdName name = map subIndex . concat $ splitOn "-" name

subIndex ∷ Char → Char
subIndex '0' = '₀'
subIndex '1' = '₁'
subIndex '2' = '₂'
subIndex '3' = '₃'
subIndex '4' = '₄'
subIndex '5' = '₅'
subIndex '6' = '₆'
subIndex '7' = '₇'
subIndex '8' = '₈'
subIndex '9' = '₉'
subIndex s   = s
