.PHONY : agda-meeting
thesis-defense :
	- make clean
	- rm -f Reconstructing-Propositional-Proofs-in-Type-Theory.pdf
	- latexmk -pdf -xelatex -shell-escape Reconstructing-Propositional-Proofs-in-Type-Theory.tex

.PHONY : agda-meeting
agda-meeting :
	- make clean
	- rm -f Jonathan-Proof-Reconstruction.pdf
	- latexmk -pdf -xelatex -shell-escape Jonathan-Proof-Reconstruction-AIM.tex

.PHONY: clean
clean :
	@latexmk -c
	@rm -f *.vrb
	@rm -f *.nav
	@rm -f *.aux
	@rm -f *.bbl
	@rm -f *.run.xml
	@rm -f *.log
	@rm -f *.snm
	@rm -f *synctex*
	@rm -f *toc
	@rm -f *out
	@rm -f *bcf
	@rm -f *blg
	@rm -f *.xdv
	@rm -Rf _*
	@rm -Rf auto

default: thesis-defense
