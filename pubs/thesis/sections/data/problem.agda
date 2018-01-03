------------------------------------------------------------------------------
-- Athena version 0.1-1425bd2.
-- TSTP file: problem.tstp.
------------------------------------------------------------------------------

module problem where

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

-- Axiom.

a₁ : PropFormula
a₁ = ((p ∨ q) ∧ (p ∨ r))

-- Premise.

Γ : Ctxt
Γ = [ a₁ ]

-- Conjecture.

goal : PropFormula
goal = (p ∨ (q ∧ r))

-- Subgoals.

subgoal₀ : PropFormula
subgoal₀ = ((¬ p) ⊃ q)

subgoal₁ : PropFormula
subgoal₁ = (((¬ p) ∧ q) ⊃ r)

------------------------------------------------------------------------------
-- Proof.
------------------------------------------------------------------------------

proof₀ : Γ ⊢ subgoal₀
proof₀ =
  (RAA
    (thm-simplify ⊥
      (thm-canonicalize ((¬ p) ∧ (¬ q))
        (assume {Γ = Γ}
          (¬ subgoal₀)))
      (thm-conjunct (p ∨ q)
        (thm-canonicalize ((p ∨ q) ∧ (p ∨ r))
          (weaken (¬ subgoal₀)
            (assume {Γ = ∅} a₁))))))

proof₁ : Γ ⊢ subgoal₁
proof₁ =
  (RAA
    (thm-simplify ⊥
      (thm-canonicalize ((¬ p) ∧ ((¬ r) ∧ q))
        (assume {Γ = Γ}
          (¬ subgoal₁)))
      (thm-conjunct (p ∨ r)
        (thm-canonicalize ((p ∨ q) ∧ (p ∨ r))
          (weaken (¬ subgoal₁)
            (assume {Γ = ∅} a₁))))))

proof : Γ ⊢ goal
proof =
  ⊃-elim
    thm-strip
    (∧-intro proof₀ proof₁)
