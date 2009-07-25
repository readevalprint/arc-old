(load "ssdt.arc")
(load "erp.arc")

(= bubblefreshdir* "arc/bubblefresh/" )
  
(def init ()
  (= posts* (table) 
    messages* (list))
  (ensure-dir bubblefreshdir*)
  (load-posts))

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

(def save-post (item)
    (save-table (item 1) (string bubblefreshdir* (item 0)))
    (= (posts* (item 0)) (item 1)))

(def new-post (post)
  (let id (string (datestring) "_" post!title)
    (save-post (list id post))))

(def add-msg args
  (ero args)
  (pushnew args messages*)) ;TODO

(def item-viewed (req item)
  (with (user (get-user req))
    (pushnew user ((item 1) 'view)))
  (save-post item))
  
(def up-vote (req item)
  (with (user (get-user req))
    (if (some user ((item 1) 'down))
      (do 
        (pull user ((item 1) 'down))
        (add-msg user "your downvote was removed"))
      (if (no (some user ((item 1) 'up)))
        (do 
          (pushnew user ((item 1) 'up))
          (add-msg user "your upvote was added"))))
    (save-post item)))

(def down-vote (req item)
  (with (user (get-user req))
    (if (some user ((item 1) 'up))
      (do 
        (pull user ((item 1) 'up))
        (add-msg user "your upvote was removed"))
      (if (no (some user ((item 1) 'down)))
        (do 
          (pushnew user ((item 1) 'down))
          (add-msg user "your downvote was added"))))
  (save-post item)))

(def vote-link (req href item)
  (with (user (get-user req))
    (if user
      (tostring 
        (spanclass "vote" 
          (if (some user ((item 1) 'up))
            (pr "<span class=\"up\">&and;</span>")
            (rlinkf "<span class=\"up\">&and;</span>" (req) (up-vote req item) href))
          (if (some user ((item 1) 'down))
            (pr "<span class=\"down\">&or;</span>")
           (rlinkf "<span class=\"down\">&or;</span>" (req) (down-vote req item) href)))))))

(def post-list (req)
  (accum accfn  
    (each x posts* 
      (let href (string "/news?id=" (car x))
        (accfn (tostring (string (pr (vote-link req "/news" x))) (link ((cadr x) 'title) href) ))))))

(def is-ajax (req)
  (errsafe (or (alref (req 'args) "ajax") (is (string (alref (req 'cooks) "ajax")) "1"))))

(def render-content (content (o class "home") (o title "") (o req))
  (pr (render (if (is-ajax req) "html/ajax.html" "html/index.html")
        (list "<!--content-->" content)
        (list "<!--title-->" title)
        (list "<!--class-->" class)
        (list "<!--message-->" messages*))))
          
          
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
  (aif (posts* (arg req "id"))
    (with (item (list (arg req "id") it))
    
     (iflet user (get-user req)
        (item-viewed req item))
      (render-content 
        (render "html/news.html" 
          (list "<!--vote-->" (vote-link req (string "/news?id=" (item 0)) item))
          (list "<!--title-->" ((item 1) 'title))
          (list "<!--body-->" ((item 1) 'body)))
          "news" (string " News: " ((item 1) 'title)) req))
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
                              
;==overwritten arc defs==
;html.arc:241
;-
;(mac whitepage body
;  `(tag html 
;     (tag (body bgcolor white alink linkblue) ,@body)))
;+
;(mac whitepage content
;  `(render-content "" (tostring ,@content) "home" "Home" req))
