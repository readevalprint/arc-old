; mzscheme -m -f as.scm
; (tl)
; (asv)
; http://localhost:8080

(require mzscheme) ; promise we won't redefine mzscheme bindings
;(port-count-lines-enabled #t)
(require "ac.scm") 
(require "brackets.scm")
(use-bracket-readtable)

(aload "arc.arc")
(aload "libs.arc") 

(tl)

