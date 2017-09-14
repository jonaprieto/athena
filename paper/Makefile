.PHONY : pdf
pdf :
	- make clean
	- sh fix-references.sh
	- biber --tool --output_align --output_indent=2 --output_fieldcase=lower -w -O=ref.bib ref-mendeley.bib
	- latexmk -pdf -f -synctex=1 -use-make main.tex

.PHONY : clean
clean:
	cat .gitignore | xargs echo rm -f | sh

default: pdf
