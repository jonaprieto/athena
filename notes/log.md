Tue Apr  4 16:36:01 COT 2017
---

* Contexts:

I found a problem related with the contexts. When in a deduction tree,
the leaves assume some axiom, the context is not trivial and some
weaken steps are necessary.

hint:
Γ = {a₁, a₂, ⋯ , aᵢ , ⋯ , aₙ }

The axiom in the leaf is aᵢ:

0. Γ₂ = {aᵢ₊₁, ⋯, aₙ}  (in Haskell) `tail . dropWhile (/= aᵢ)`, and
1. use this context Γ₁ = {a₁, ⋯, aᵢ₋₁}, (in Haskell `takeWhile (/= a₁)` and
2. The result should be the step:
     `weaken₁ (Γ₂ ∪ (atp-negate subgoal̰) (assume {Γ = Γ₁} aᵢ)`

* Resolve:

I removed temporally the use of quantifiers because I am
working without them at this moment.

```Haskell
-- Athena.TSTP.Base.hs
univquantFreeVars ∷ Formula → Formula
univquantFreeVars cnf = Quant All freeVars cnf
    where
      freeVars ∷ [V]
      freeVars = toList $ freeVarsF cnf

-- Athena.TSTP.Parser.hs
cnf_annotated :: {F}
cnf_annotated : cnf lp name  comma formula_role  comma cnf_formula  annotations  rp dot
              { F { formula = univquantFreeVars $7
                  , name    = readWord $3
                  , role    = $5
                  , source  = $8
                  }
              }
```

Wed Apr  5 11:07:41 COT 2017
---

I committed a mistake reasoning about the right associative in the splitGoal,
when I was doing the following with `splitGoal` but it didn't type-checked.

```agda
proof : Γ ⊢ goal
proof =
  ⇒-elim
    atp-splitGoal
    (∧-intro
      proof₀
      (∧-intro
        proof₁
        proof₂))
```

I've type-checked:

```agda
proof : Γ ⊢ goal
proof =
  ⇒-elim
    atp-splitGoal
    (∧-intro
      (∧-intro
        proof₀
        proof₁)
      proof₂
      )
```
