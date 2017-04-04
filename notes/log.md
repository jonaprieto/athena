Tue Apr  4 16:36:01 COT 2017
====

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
