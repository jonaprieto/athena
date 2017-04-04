
-- | Athena.Translation.Rule.Resolve module.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Athena.Translation.Rules.Resolve ( atpResolve ) where

------------------------------------------------------------------------------

import Data.TSTP            ( Formula(..), Quant(..) )
import Data.TSTP.AtomicWord ( AtomicWord(..) )
import Data.TSTP.BinOp      ( BinOp(..) )

------------------------------------------------------------------------------

fromQuant :: Formula -> Formula
fromQuant (Quant _ _ f) = f
fromQuant f = f

atpResolve ∷ Formula → Formula → Formula → (String, Bool)
atpResolve f g l = atpResolve' (fromQuant f) (fromQuant g) (fromQuant l)

--
atpResolve' f g  (PredApp (AtomicWord "$false") [])
  | f == (:~:) g             = ("atp-resolve₈", False)
  | (:~:) f == g             = ("atp-resolve₈", True)
atpResolve' f@(BinOp f₁ (:|:) f₂) g@(BinOp g₁ (:|:) g₂) l
  | f₁ == l && g₁ == (:~:) l = ("atp-resolve₀", False)
  | f₂ == l && g₂ == (:~:) l = ("atp-resolve₁", False)
  | f₁ == l && g₂ == (:~:) l = ("atp-resolve₂", False)
  | f₂ == l && g₁ == (:~:) l = ("atp-resolve₃", False)
  | otherwise                = errorResolve 1 f g l
atpResolve' f@(BinOp f₁ (:|:) f₂) g l
  | f₁ == (:~:) l && g == l = ("atp-resolve₄", False)
  | f₂ == (:~:) l && g == l = ("atp-resolve₅", False)
  | f₂ == l && g == (:~:) l = ("atp-resolve₆", False)
  | f₁ == l && g == (:~:) l = ("atp-resolve₇", False)
  | otherwise               = errorResolve 2 f g l
atpResolve' f g@(BinOp g₁ (:|:) g₂) l
  | f == l && g₁ == (:~:) l = ("atp-resolve₄", True)
  | f == l && g₂ == (:~:) l = ("atp-resolve₅", True)
  | f == (:~:) l && g₂ == l = ("atp-resolve₆", True)
  | f == (:~:) l && g₁ == l = ("atp-resolve₇", True)
  | otherwise               = errorResolve 3 f g l
atpResolve' f g l           = errorResolve 4 f g l


errorResolve :: Int -> Formula -> Formula -> Formula -> (String, Bool)
errorResolve i f g l =
  ("id "++show i ++"--\nf="++show f ++"\ng= "++show  g ++ "\nl="++show l ++"\n", False)
