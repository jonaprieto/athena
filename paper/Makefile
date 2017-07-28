.PHONY : tex
tex :
				make clean
				make references
				latexmk -pvc -pdf -pdflatex="pdflatex -interactive=nonstopmode" -use-make main.tex


.PHONY : clean
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

.PHONY : references
references :
	@sed -i.backup "/^(url|doi)/ s/{.\?\(.\)}/\1/g" ref.bib
default: tex
