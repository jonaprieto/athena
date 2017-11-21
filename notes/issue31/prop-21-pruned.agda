------------------------------------------------------------------------------
-- Athena version 0.1-55428b3.
-- TSTP file: test/prop-pack/problems/prop-metis/prop-21.tstp.
------------------------------------------------------------------------------

-- To test this file:
-- $ export AGDA_DIR=ATHENA_PATH/lib/.agda
-- $ pwd
-- ATHENA_PATH
-- $ agda --library=notes notes/issue31/prop-21-pruned.agda -v20

module prop-21-pruned where

------------------------------------------------------------------------------

open import ATP.Metis 3 public
open import Data.PropFormula 3 public

------------------------------------------------------------------------------

-- Variables.

p : PropFormula
p = Var (# 0)

q : PropFormula
q = Var (# 1)

r : PropFormula
r = Var (# 2)

-- Premise.

Γ : Ctxt
Γ = ∅

-- Conjecture.

all₃clauses : PropFormula
all₃clauses = (((p ∨ (q ∨ r)) ∧ ((p ∨ (q ∨ (¬ r))) ∧ ((p ∨ ((¬ q) ∨ r)) ∧ ((p ∨ ((¬ q) ∨ (¬ r))) ∧ (((¬ p) ∨ (q ∨ r)) ∧ (((¬ p) ∨ (q ∨ (¬ r))) ∧ (((¬ p) ∨ ((¬ q) ∨ r)) ∧ ((¬ p) ∨ ((¬ q) ∨ (¬ r)))))))))) ⊃ ⊥)

-- Subgoal.

subgoal₀ : PropFormula
subgoal₀ = (((p ∨ (q ∨ r)) ∧ ((p ∨ (q ∨ (¬ r))) ∧ ((p ∨ ((¬ q) ∨ r)) ∧ ((p ∨ ((¬ q) ∨ (¬ r))) ∧ (((¬ p) ∨ (q ∨ r)) ∧ (((¬ p) ∨ (q ∨ (¬ r))) ∧ (((¬ p) ∨ ((¬ q) ∨ r)) ∧ ((¬ p) ∨ ((¬ q) ∨ (¬ r)))))))))) ⊃ ⊥)

------------------------------------------------------------------------------
-- Proof.
------------------------------------------------------------------------------

proof₀ : Γ ⊢ subgoal₀
proof₀ =
  (RAA
    (atp-resolve ⊥ q
      (atp-resolve q p
        (atp-resolve p q
          (atp-resolve (p ∨ q) r
            (atp-conjunct (p ∨ (q ∨ r))
              (canonicalized))
            (atp-conjunct ((¬ r) ∨ (p ∨ q))
              (canonicalized)))
          (atp-resolve ((¬ q) ∨ p) r
            (atp-resolve (p ∨ r) q
              (atp-resolve (p ∨ q) r
                (atp-conjunct (p ∨ (q ∨ r))
                  (canonicalized))
                (atp-conjunct ((¬ r) ∨ (p ∨ q))
                  (canonicalized)))
              (atp-conjunct ((¬ q) ∨ (p ∨ r))
                (canonicalized)))
            (atp-conjunct ((¬ q) ∨ ((¬ r) ∨ p))
              (canonicalized))))
        (atp-resolve ((¬ p) ∨ q) r
          (atp-resolve (q ∨ r) p
            (atp-resolve p q
              (atp-resolve (p ∨ q) r
                (atp-conjunct (p ∨ (q ∨ r))
                  (canonicalized))
                (atp-conjunct ((¬ r) ∨ (p ∨ q))
                  (canonicalized)))
              (atp-resolve ((¬ q) ∨ p) r
                (atp-resolve (p ∨ r) q
                  (atp-resolve (p ∨ q) r
                    (atp-conjunct (p ∨ (q ∨ r))
                      (canonicalized))
                    (atp-conjunct ((¬ r) ∨ (p ∨ q))
                      (canonicalized)))
                  (atp-conjunct ((¬ q) ∨ (p ∨ r))
                    (canonicalized)))
                (atp-conjunct ((¬ q) ∨ ((¬ r) ∨ p))
                  (canonicalized))))
            (atp-conjunct ((¬ p) ∨ (q ∨ r))
              (canonicalized)))
          (atp-conjunct ((¬ p) ∨ ((¬ r) ∨ q))
            (canonicalized))))
      (atp-resolve (¬ q) p
        (atp-resolve p q
          (atp-resolve (p ∨ q) r
            (atp-conjunct (p ∨ (q ∨ r))
              (canonicalized))
            (atp-conjunct ((¬ r) ∨ (p ∨ q))
              (canonicalized)))
          (atp-resolve ((¬ q) ∨ p) r
            (atp-resolve (p ∨ r) q
              (atp-resolve (p ∨ q) r
                (atp-conjunct (p ∨ (q ∨ r))
                  (canonicalized))
                (atp-conjunct ((¬ r) ∨ (p ∨ q))
                  (canonicalized)))
              (atp-conjunct ((¬ q) ∨ (p ∨ r))
                (canonicalized)))
            (atp-conjunct ((¬ q) ∨ ((¬ r) ∨ p))
              (canonicalized))))
        (atp-resolve ((¬ p) ∨ (¬ q)) r
          (atp-resolve r p
            (atp-resolve p q
              (atp-resolve (p ∨ q) r
                (atp-conjunct (p ∨ (q ∨ r))
                  (canonicalized))
                (atp-conjunct ((¬ r) ∨ (p ∨ q))
                  (canonicalized)))
              (atp-resolve ((¬ q) ∨ p) r
                (atp-resolve (p ∨ r) q
                  (atp-resolve (p ∨ q) r
                    (atp-conjunct (p ∨ (q ∨ r))
                      (canonicalized))
                    (atp-conjunct ((¬ r) ∨ (p ∨ q))
                      (canonicalized)))
                  (atp-conjunct ((¬ q) ∨ (p ∨ r))
                    (canonicalized)))
                (atp-conjunct ((¬ q) ∨ ((¬ r) ∨ p))
                  (canonicalized))))
            (atp-resolve ((¬ p) ∨ r) q
              (atp-resolve q p
                (atp-resolve p q
                  (atp-resolve (p ∨ q) r
                    (atp-conjunct (p ∨ (q ∨ r))
                      (canonicalized))
                    (atp-conjunct ((¬ r) ∨ (p ∨ q))
                      (canonicalized)))
                  (atp-resolve ((¬ q) ∨ p) r
                    (atp-resolve (p ∨ r) q
                      (atp-resolve (p ∨ q) r
                        (atp-conjunct (p ∨ (q ∨ r))
                          (canonicalized))
                        (atp-conjunct ((¬ r) ∨ (p ∨ q))
                          (canonicalized)))
                      (atp-conjunct ((¬ q) ∨ (p ∨ r))
                        (canonicalized)))
                    (atp-conjunct ((¬ q) ∨ ((¬ r) ∨ p))
                      (canonicalized))))
                (atp-resolve ((¬ p) ∨ q) r
                  (atp-resolve (q ∨ r) p
                    (atp-resolve p q
                      (atp-resolve (p ∨ q) r
                        (atp-conjunct (p ∨ (q ∨ r))
                          (canonicalized))
                        (atp-conjunct ((¬ r) ∨ (p ∨ q))
                          (canonicalized)))
                      (atp-resolve ((¬ q) ∨ p) r
                        (atp-resolve (p ∨ r) q
                          (atp-resolve (p ∨ q) r
                            (atp-conjunct (p ∨ (q ∨ r))
                              (canonicalized))
                            (atp-conjunct ((¬ r) ∨ (p ∨ q))
                              (canonicalized)))
                          (atp-conjunct ((¬ q) ∨ (p ∨ r))
                            (canonicalized)))
                        (atp-conjunct ((¬ q) ∨ ((¬ r) ∨ p))
                          (canonicalized))))
                    (atp-conjunct ((¬ p) ∨ (q ∨ r))
                      (canonicalized)))
                  (atp-conjunct ((¬ p) ∨ ((¬ r) ∨ q))
                    (canonicalized))))
              (atp-conjunct ((¬ p) ∨ ((¬ q) ∨ r))
                (canonicalized))))
          (atp-conjunct ((¬ p) ∨ ((¬ q) ∨ (¬ r)))
            (canonicalized))))))
  where
    canonicalized =
      atp-canonicalize
        (((¬ p) ∨ ((¬ q) ∨ (¬ r))) ∧ (((¬ p) ∨ ((¬ q) ∨ r)) ∧ (((¬ p) ∨ ((¬ r) ∨ q)) ∧ (((¬ p) ∨ (q ∨ r)) ∧ (((¬ q) ∨ ((¬ r) ∨ p)) ∧ (((¬ q) ∨ (p ∨ r)) ∧ (((¬ r) ∨ (p ∨ q)) ∧ (p ∨ (q ∨ r)))))))))
          (assume {Γ = Γ}
            (¬ subgoal₀))

proof : Γ ⊢ all₃clauses
proof =
  ⊃-elim
    atp-split
    proof₀
