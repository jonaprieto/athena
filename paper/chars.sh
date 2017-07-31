#!/bin/sh

sed -i 's/phat/$\\hat p$/g' main.tex
sed -i 's/ᵢ/$_i$/g' main.tex
sed -i 's/≤/$\\leq$/g' main.tex
sed -i 's/ₙ/$_n$/g' main.tex
sed -i 's/ρ/$\\rho$/g' main.tex
sed -i 's/ψ/$\\psi$/g' main.tex
sed -i 's/σ/$\\sigma$/g' main.tex
sed -i 's/Γ/$\\Gamma$/g' main.tex
sed -i 's/η/$\\eta$/g' main.tex
sed -i 's/ᶜ/$\^c$/g' main.tex
sed -i 's/φ/$\\phi$/g' main.tex
sed -i 's/⊨/$\\vDash$/g' main.tex
sed -i 's/◾/$\\scriptscriptstyle\\blacksquare$/g' main.tex
sed -i 's/⇛/$\\Rrightarrow$/g' main.tex
# sed -i 's/\\ensuremath{\\neg }/\\ensuremath{\\Varid{neg}}/g' main.tex
# sed -i 's/\\ensuremath{\\neg}/\\ensuremath{\\Varid{neg}}/g' main.tex
# sed -i 's/\\neg/\\Varid{neg}/g' main.tex
