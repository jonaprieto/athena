.PHONY : pdf
pdf :
	- make clean
	- latexmk -pdf -f -synctex=1 -use-make main.tex

.PHONY : references
references : ref-mendeley.bib
	- rm -f ref.bib
	- biber --tool --output_align --output_indent=2 \
		--output_fieldcase=lower -w -O=ref.bib ref-mendeley.bib
	- sh fix-references.sh

.PHONY : clean
clean:
	cat .gitignore | xargs echo rm -f | sh

default: pdf
