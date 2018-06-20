.PHONY : pdf
pdf :
	- latexmk -pdf itp-2018.tex

.PHONY : watch-pdf
watch-pdf :
<<<<<<< HEAD
	- pkill pdflatex
=======
>>>>>>> a9fedb680d16b5a15e7397f9111524be5f53f071
	- latexmk -pdf -pvc itp-2018.tex
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
		-O=ref-mendeley.bib.new ref-mendeley.bib
	- mv ref-mendeley.bib.new ref-mendeley.bib
	- rm -f ref-mendeley.bib.blg
	- sh fix-references.sh
	- cp ref-mendeley.bib ref.bib

.PHONY : clean
clean:
	cat .gitignore | xargs echo rm -r -f | sh

default: pdf
