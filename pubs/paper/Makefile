.PHONY : pdf
pdf :
	- latexmk -pdf paper.tex

.PHONY : watch-pdf
watch-pdf :
	- pkill pdflatex
<<<<<<< HEAD:pubs/paper/Makefile
	- latexmk -pdf -pvc paper.tex
=======
	- latexmk -pdf -pvc itp-2018.tex
>>>>>>> 078fe67b5636e860b63c9f7ccb7fedded0cdfd44:pubs/itp-2018/Makefile
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
