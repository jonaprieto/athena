NAME=Reconstructing-Propositional-Proofs-in-Type-Theory

.PHONY : pdf
pdf :
	- make clean
	- rm -f $(NAME).pdf
	- latexmk -pdf -xelatex -shell-escape $(NAME).tex

.PHONY : old-pdf
old-pdf :
	- make clean
	- rm -f Jonathan-Proof-Reconstruction.pdf
	- latexmk -pdf -xelatex -shell-escape Jonathan-Proof-Reconstruction.tex

.PHONY : dependencies
dependencies :
	- cd fonts && git submodule update --init --recursive
	- git submodule update --init --recursive pygments-cagda/
	- cd pygments-cagda && pip install .


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

default: pdf
