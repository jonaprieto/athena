(TeX-add-style-hook
 "slides"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("beamer" "xetex" "hyperref={pdfpagelabels=false}")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("babel" "english") ("csquotes" "autostyle") ("biblatex" "backend=biber" "style=authoryear-icomp" "sortlocale=en_US" "natbib=true" "url=false" "doi=true" "eprint=false")))
   (TeX-run-style-hooks
    "latex2e"
    "beamer"
    "beamer10"
    "xcolor"
    "babel"
    "amsmath"
    "amsthm"
    "amssymb"
    "bussproofs"
    "fancyvrb"
    "geometry"
    "graphicx"
    "lmodern"
    "lstagda"
    "tikz"
    "url"
    "fontspec"
    "mathtools"
    "unicode-math"
    "csquotes"
    "biblatex"
    "silence")
   (TeX-add-symbols
    '("solutiontstp" ["argument"] 1)
    '("problemtptp" ["argument"] 2))
   (LaTeX-add-labels
    "tptp-syntax"
    "tptp-examples"
    "tstp-syntax"
    "tstp-example"
    "tstp-dag"
    "references")
   (LaTeX-add-bibliographies
    "ref")
   (LaTeX-add-saveboxes
    "agdapragma"))
 :latex)

