;arc document templates - Public Domain 2009 Timothy Watts coconutrandom@gmail.com

;opens and evals file everytime, ok for now because developing
;need to cache string for production, but still eval atstrings

(declare 'atstrings t)

(def render (file)
  (eval (filechars file)))
  
  
(def companyname () "Acme Co")

(def header () 
  (render "html/header.html"))

;(render "html/test.html")
