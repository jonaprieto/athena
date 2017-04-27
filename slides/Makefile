

.PHONY : tex
tex :
	xelatex slides.tex
	biber slides.bcf
	xelatex slides.tex
	make clean


.PHONY: clean
clean :
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

default: tex