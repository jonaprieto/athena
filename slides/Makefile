.PHONY : tex
tex :
	make clean
	latexmk -pvc -xelatex -shell-escape Jonathan-Proof-Reconstruction.tex


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

default: tex
