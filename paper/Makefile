.PHONY : pdf
pdf :
	- make clean
	- latexmk -pdf -f -synctex=1 -use-make main.tex



# references-mendeley:
#
# From now on, we maintain both references (ref.bib and
# ref-mendely.bib), but they could diverge.
# ref-mendeley.bib will stand for the backup (automatically
# generated).

.PHONY : ref-mendeley
ref-mendeley : ref-mendeley.bib
	- biber --tool --output_align --output_indent=2 \
		--output_fieldcase=lower -w \
		-O=ref-mendeley.bib ref-mendeley.bib
	- sh fix-references.sh

.PHONY : clean
clean:
	cat .gitignore | xargs echo rm -f | sh

default: pdf
