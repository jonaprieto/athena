.PHONY : pdf
pdf :
	- git submodule update --init --recursive fonts
	- make clean
	- lhs2TeX --agda main.lagda > main.tex
	- sh chars.sh
	- sh fix-references.sh
	- latexmk -pdf -f -synctex=1 -use-make main.tex

.PHONY : clean
clean :
	@latexmk -c
	@rm -f main.tex
	@rm -f *.aux
	@rm -f *.bbl
	@rm -f *.dvi
	@rm -f *.fdb_latexmk
	@rm -f *.log
	@rm -f *.nav
	@rm -f *.fls
	@rm -f *.pdf
	@rm -f *.run.xml
	@rm -f *.snm
	@rm -f *.vrb
	@rm -f *.xdv
	@rm -f *bcf
	@rm -f *blg
	@rm -f *out
	@rm -f *ptb
	@rm -f *synctex*
	@rm -f *toc
	@rm -f *~
	@rm -Rf _*
	@rm -Rf auto

default: pdf
