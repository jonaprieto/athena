
-- | Translation.Rule.Resolve module.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Athena.Translation.Rules.Resolve ( atpResolve ) where

------------------------------------------------------------------------------

import Data.TSTP            ( Formula(..) )
import Data.TSTP.AtomicWord ( AtomicWord(..) )
import Data.TSTP.BinOp      ( BinOp(..) )

------------------------------------------------------------------------------

atpResolve ∷ Formula → Formula → Formula → (String, Bool)
atpResolve f g  (PredApp (AtomicWord "$false") [])
  | f == (:~:) g = ("atp-resolve₈", False)
  | (:~:) f == g = ("atp-resolve₈", True)
-- I guess l literal is always positive.
atpResolve (BinOp f₁ (:|:) f₂) (BinOp g₁ (:|:) g₂) l
  | f₁ == l && g₁ == (:~:) l = ("atp-resolve₀", False)
  | f₂ == l && g₂ == (:~:) l = ("atp-resolve₁", False)
  | f₁ == l && g₂ == (:~:) l = ("atp-resolve₂", False)
  | f₂ == l && g₁ == (:~:) l = ("atp-resolve₃", False)
  | otherwise = ("id -- resolve 1.", False)
atpResolve (BinOp f₁ (:|:) f₂) g l
  | f₁ == (:~:) l && g == l = ("atp-resolve₄", False)
  | f₂ == (:~:) l && g == l = ("atp-resolve₅", False)
  | f₂ == l && g == (:~:) l = ("atp-resolve₆", False)
  | f₁ == l && g == (:~:) l = ("atp-resolve₇", False)
  | otherwise = ("id -- resolve 2.", False)
atpResolve f (BinOp g₁ (:|:) g₂) l
  | f == l && g₁ == (:~:) l = ("atp-resolve₄", True)
  | f == l && g₂ == (:~:) l = ("atp-resolve₅", True)
  | f == (:~:) l && g₂ == l = ("atp-resolve₆", True)
  | f == (:~:) l && g₁ == l = ("atp-resolve₇", True)
  | otherwise = ("id -- resolve 3.", False)
atpResolve _ _ _ = ("id -- resolve 4.", False)
