

.PHONY : tex
tex :
	make clean
	xelatex -shell-escape slides.tex
	biber slides.bcf
	xelatex -shell-escape slides.tex
	make clean
	open slides.pdf


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
	@rm -Rf _*

default: tex