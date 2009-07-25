;Super Simple Document Templates - For Arc Lisp
;rev 1 added render and li
;Public Domain 2009 Timothy Watts coconutrandom@gmail.com

;opens and evals file everytime
;need to cache string for production?

;usage
;== html/test.html ==
;<html>
;  <body>@(header)</body>
;</html>
;== html/top.html ==
;<h1>@( companyname )</h1>

;(def companyname () "Acme Co")

;(def header () 
;  (render "html/top.html"))

;arc> (render "html/test.html")
;"<html>\n  <body><h1>Acme Co</h1>\n\n</html>\n"
(def file-ero (file char) (prn (cut (filechars file) (- char 100) char) ">>>" (cut (filechars file) char (+ char 100)) ) nil)

(def render (file . vars)
  (if vars
    (multisubst vars (filechars file))
    (filechars file)))

;<li> for use in <ul> or <ol> 
;gentag prs all over the place, need to build strings, eval, THEN print

(attribute li class opstring)
(attribute li id    opstring)

(def li items
  (tostring
    (each i items
      (if (isa i 'table)
          (tag (li class i!class id i!id) (pr i!body))
          (tag li (pr i))))))


;(def li (l)
;  (string 
;    (mappend [if (is (type _) 'table)
;                (list 
;                    "<li " 
;                    (aif (_ 'class) (string "class=\"" it "\""))
;                    (aif (_ 'id) (string "id=\"" it "\""))
;                    ">" 
;                    (_ 'body)
;                     "</li>")
;                (list 
;                    "<li>"
;                    (string _)
;                     "</li>")
;             ] l )))

;usage
;arc> (li `(,(obj body "hi" id "brown") ,(obj body "world" class "green") "foo"))
;"<li id=\"brown\">hi</li><li class=\"green\">world</li><li>foo</li>"
