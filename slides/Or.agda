module Or where

data _or_ (A B : Set) : Set where
  inj1 : A -> A or B
  inj2 : B -> A or B

postulate
  A B    : Set
  or-comm : A or B -> B or A
{-# ATP prove or-comm #-}