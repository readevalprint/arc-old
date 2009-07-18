(load "ssdt.arc")

(= bubblefreshdir* "arc/bubblefresh/" )
  
(def init ()
  (= posts* (table) )
  (ensure-dir bubblefreshdir*)
  (load-posts))

(deftem 
  post 
  title "" 
  body "" 
  by "")

(def load-posts ()
  (each id (map string (dir bubblefreshdir*))
      (= (posts* id) (temload 'post (string bubblefreshdir* id)))))

(def save-post (post)
  (let id (string (datestring) "_" post!title)
    (save-table post (string bubblefreshdir* id))
    (= (posts* id) (list post))
    id))
  
(def post-list () 
  (accum accfn  
    (each x posts* 
      (accfn (tostring (link ((cadr x) 'title) (string "news?id=" (car x))))))))

(defop || req
  (pr (render "html/index.html" '("<!--bodyclass-->" "home"))))

(defop m req
  (pr (render "html/mobile.html" )))

(defop apparel req
  (pr (render "html/index.html" '("<!--bodyclass-->" "apparel"))))
  
  
(attribute ul class opstring)
(attribute ul id    opstring)

(defop news req
  (let content 
    (aif (posts* (alref (req 'args) "id"))
        (it 'body)
        (tostring (tag (ul class "news") (pr (apply li (post-list))))))
    (pr (render "html/index.html" 
          (list "<!--content-->" content)
          (list "<!--bodyclass-->" "news")))))

(defop magazine req
  (pr (render "html/index.html" '("<!--bodyclass-->" "magazine"))))

(defop bonus req
  (pr (render "html/index.html" 
        (list"<!--bodyclass-->" "bonus")
        (list "<!--content-->"
          (if (get-user req)
            "BONUS"
            (tostring (login-page 'login
                        "You need to be logged in to do that."
                        (list (fn (u ip))
                              (string 'bonus (reassemble-args req))))))))))
                              
                              
                              
; overwritten arc defs
(def failed-login (switch msg afterward)
  (if (acons afterward)
      (flink (fn ignore (pr (render "html/index.html" 
        (list "<!--content-->"
            (tostring (login-page switch msg afterward)))))))
      (do (prn)
          (login-page switch msg afterward))))
