module Or where

data _∨_ (A B : Set) : Set where
  inj1 : A → A ∨ B
  inj2 : B → A ∨ B

postulate
  A B    : Set
  ∨-comm : A ∨ B → B ∨ A
{-# ATP prove ∨-comm #-}
