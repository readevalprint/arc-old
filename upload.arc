(declare 'atstrings t)

(def render (file)
  (eval (filechars file)))


(defop upload req
  (aform (fn (req) (prn req)) (pr (tag (input name "uploadedfile" type "file"))) (submit)))
