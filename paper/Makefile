.PHONY : pdf
pdf :
	- make clean
	- sh chars.sh
	- sh fix-references.sh
	- latexmk -pdf -f -synctex=1 -use-make main.tex

.PHONY : clean
clean:
	cat .gitignore | xargs echo rm -f | sh

default: pdf
