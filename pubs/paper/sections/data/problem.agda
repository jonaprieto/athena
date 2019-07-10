------------------------------------------------------------------------------
-- Athena version 0.1-172931c.
-- TSTP file: problem.tstp.
------------------------------------------------------------------------------

module problem where

------------------------------------------------------------------------------

open import ATP.Metis 2 public
open import Data.PropFormula 2 public

------------------------------------------------------------------------------

-- Variables.

p : PropFormula
p = Var (# 0)

q : PropFormula
q = Var (# 1)

-- Axiom.

premise : PropFormula
premise = ((p ⊃ q) ∧ (q ⊃ p))

-- Premise.

Γ : Ctxt
Γ = [ premise ]

-- Conjecture.

goal : PropFormula
goal = ((p ∨ q) ⊃ (p ∧ q))

-- Subgoals.

subgoal₀ : PropFormula
subgoal₀ = ((p ∨ q) ⊃ p)

subgoal₁ : PropFormula
subgoal₁ = (((p ∨ q) ∧ p) ⊃ q)

------------------------------------------------------------------------------
-- Proof of subgoal₀.
------------------------------------------------------------------------------

proof₀ : Γ ⊢ subgoal₀
proof₀ =
  (RAA
    (resolve-thm ⊥ p
      (resolve-thm p q
        (simplify-thm q
          (conjunct-thm (p ∨ q)
            (canonicalize-thm ((¬ p) ∧ (p ∨ q))
              (assume {Γ = Γ} (¬ subgoal₀))))
          (conjunct-thm (¬ p)
            (canonicalize-thm ((¬ p) ∧ (p ∨ q))
              (assume {Γ = Γ} (¬ subgoal₀)))))
        (conjunct-thm ((¬ q) ∨ p)
          (canonicalize-thm (((¬ p) ∨ q) ∧ ((¬ q) ∨ p))
            (weaken (¬ subgoal₀)
              (assume {Γ = ∅} premise)))))
      (conjunct-thm (¬ p)
        (canonicalize-thm ((¬ p) ∧ (p ∨ q))
          (assume {Γ = Γ} (¬ subgoal₀))))))

------------------------------------------------------------------------------
-- Proof of subgoal₁.
------------------------------------------------------------------------------

proof₁ : Γ ⊢ subgoal₁
proof₁ =
  (RAA
    (simplify-thm ⊥
      (canonicalize-thm ((¬ q) ∧ (p ∧ (p ∨ q)))
        (assume {Γ = Γ} (¬ subgoal₁)))
      (conjunct-thm ((¬ p) ∨ q)
        (canonicalize-thm (((¬ p) ∨ q) ∧ ((¬ q) ∨ p))
          (weaken (¬ subgoal₁)
            (assume {Γ = ∅} premise))))))

------------------------------------------------------------------------------
-- Proof of the goal.
------------------------------------------------------------------------------

proof : Γ ⊢ goal
proof =
  ⊃-elim
    strip-thm
    (∧-intro proof₀ proof₁)
