(load "ssdt.arc")
(load "erp.arc")
(= bubblefreshdir* "arc/bubblefresh/" )
  
(def init ()
  (= posts* (table) )
  (ensure-dir bubblefreshdir*)
  (load-posts))

(deftem
  message
  user ""
  class "message"
  body "")

(deftem 
  post 
  title "" 
  body "" 
  by ""
  up '(nil)
  down '(nil)
  view '(nil)
  )

(attribute ul class opstring)
(attribute ul id    opstring)

(def load-posts ()
  (each id (map string (dir bubblefreshdir*))
      (= (posts* id) (temload 'post (string bubblefreshdir* id)))))

(def save-post (id post)
    (save-table post (string bubblefreshdir* id))
    (= (posts* id) (list post))
    id)

(def new-post (post)
  (let id (string (datestring) "_" post!title)
    (save-post (id post))))

(def add-msg args
  (ero args)) ;TODO


(def up-vote (user item)
  (if (some user (item 'down))
    (do 
      (pull user (item 'down))
      (add-msg user "your downvote was removed"))
    (if (no (some user (item 'up)))
      (do 
        (pushnew user (item 'up))
        (add-msg user "your upvote was added")))))

(def down-vote (user item)
    (if (some user (item 'up))
      (do 
        (pull user (item 'up))
        (add-msg user "your upvote was removed"))
      (if (no (some user (item 'down)))
        (do 
          (pushnew user (item 'down))
          (add-msg user "your downvote was added")))))

(def vote-links (req href item)
  (with (user (get-user req))
    (if user
      (string
        (rlinkf " &and; " (req) (up-vote user item) href)
        (rlinkf " &or; " (req) (down-vote user item) href)))))

(def post-list (req)
  (accum accfn  
    (each x posts* 
      (let href (string "news?id=" (car x))
        (accfn (tostring (vote-links req href (x 1)) (link ((cadr x) 'title) href) ))))))

(def is-ajax (req)
  (errsafe (or (alref (req 'args) "ajax") (is (string (alref (req 'cooks) "ajax")) "1"))))

(def render-content (content (o class "home") (o title) (o req))
  (pr (render (if (is-ajax req) "html/ajax.html" "html/index.html")
        (list "<!--content-->" content)
        (and title (list "<!--title-->"  title))
        (list "<!--class-->" class))))
          
          
(defop-raw || (str req) (w/stdout str
  (prn "Set-Cookie: ajax=0")
  (prn)
  (with (content "" class "home")
    (prn (render "html/index.html" 
          (list "<!--content-->" content)
          (list "<!--title-->" "")
          (list "<!--class-->" class))))))

(defop-raw m (str req) (w/stdout str
  (prn "Set-Cookie: ajax=1")
  (prn)
  (prn (render "html/mobile.html" 
          (list "<!--news-->" (string "<ul title=\"News\" id=\"news\">" (apply li (post-list)) "</ul>"))
          (list "<!--apparel-->" (string "<ul title=\"Apparel\" id=\"apparel\">" "<li><a href=\"\">123</a></li>" "</ul>"))))))


(defop apparel req
  (let f (file-exists (+ "html/product/" (caar (req 'args))))
    (if f
      (render-content (render f) "apparel" (+ " Apparel: " (caar (req 'args))) req)
      (render-content (render "html/apparel.html") "apparel" " Apparel" req))))
    
  
(defop news req
  (aif (posts* (alref (req 'args) "id"))
        (render-content (it 'body) "news" (it 'title) req) ;if id is specified use that
        (render-content (string "<ul>" (apply li (post-list req)) "</ul>") "news" " News" req)))

(defop magazine req
  (render-content "<ul><li><a href=\"\">123</a></li></ul>" "magazine" " Magazine" req))

(defop bonus req
  (if (get-user req)
      (render-content "<div class=\"panel\">BONUS BODY<div>" "bonus" " Bonus" req)
      (login-page req 'login
          "You need to be logged in to do that."
          (list (fn (u ip))
                (string 'bonus (reassemble-args req))) )))
;TODO make fix form redirect error in ajax                              
;FIXED                              
                              
;==overwritten arc defs==
;html.arc:241
;-
;(mac whitepage body
;  `(tag html 
;     (tag (body bgcolor white alink linkblue) ,@body)))
;+
;(mac whitepage content
;  `(render-content "" (tostring ,@content) "home" "Home" req))
