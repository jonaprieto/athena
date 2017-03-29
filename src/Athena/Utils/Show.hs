
-- | Athena.Utils.Show module.
-- Adapted from https://github.com/asr/apia.

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Athena.Utils.Show
  ( showListLn
  , showLn
  ) where

------------------------------------------------------------------------------

-- | Version of 'show' adding a newline character.
showLn ∷ Show a ⇒ a → String
showLn = (++ "\n") . show

-- | Version of 'show' on lists where the elements are separated by
-- newline characters.
showListLn ∷ Show a ⇒ [a] → String
showListLn [] = "[]"
showListLn xs = concatMap showLn xs
