(TeX-add-style-hook
 "main"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("llncs" "runningheads" "a4paper")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "llncs"
    "llncs10"
    "bussproofs"
    "amssymb"
    "graphicx"
    "multicol"
    "url"
    "tikz"
    "rotating")
   (TeX-add-symbols
    '("keywords" 1))
   (LaTeX-add-labels
    "Related Work"
    "sec2"
    "secproofrecon"
    "secconclusion")
   (LaTeX-add-environments
    "bprooftree")
   (LaTeX-add-bibliographies
    "ref"))
 :latex)

