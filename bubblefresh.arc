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
  id ""
  title "" 
  img ""
  cached ""
  link "" 
  by ""
  up '(nil)
  down '(nil)
  view '(nil)
  flag '(nil)
  dead '(nil)
  )

(attribute ul class opstring)
(attribute ul id    opstring)

(def load-posts ()
  (each id (map string (dir bubblefreshdir*))
      (= (posts* id) (temload 'post (string bubblefreshdir* id)))))

(def save-post (item)
    (save-table (item 1) (string bubblefreshdir* (item 0)))
    (= (posts* (item 0)) (item 1)))

(def cache-img (src)
  (with (to (string (rand-string 20) ".png")
         from (cut src 0 (urlend src 0)))
    (system (erp:string "convert " from " ./static/" to))
    to))
    
(def clean-url (url)
  (mz:regexp-replace* "[^A-Za-z0-9/_:?#&.]+" url "")) ;is this safe?
  
(def clean-title (title)
  (mz:regexp-replace* "[^A-Za-z0-9]+" title ""))
  
(def new-post (img link title);TODO add xss safty
    (save-post 
      (let id (string title "-" (len posts*))
        (list id (inst 'post 
                  'id id
                  'title (clean-title title)
                  'link (clean-url link)
                  'img (clean-url img)
                  'cached (cache-img (clean-url img)))))))

(def add-msg args
  (ero args)
  (pushnew args messages*)) ;TODO ... or not

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
            (pr "<span class=\"up\">&and;" (- (len ((item 1) 'up)) 1) "</span>")
            (rlinkf (string "<span class=\"up\">&and;" (- (len ((item 1) 'up)) 1) "</span>") (req) (up-vote req item) href))
          (if (some user ((item 1) 'down))
            (pr "<span class=\"down\">&or;"  (- (len ((item 1) 'down)) 1)"</span>")
           (rlinkf 
            (string "<span class=\"down\">&or;" (- (len ((item 1) 'down)) 1) "</span>") 
            (req) 
            (down-vote req item) 
            href)))))))

(def link-class (req item)
  (with (user (get-user req))
    (if user
      (string
        (if (some (get-user req) ((item 1) 'up)) "up")
        (if (some (get-user req) ((item 1) 'down)) "down")))))
        
(attribute img class opstring)

(def post-list (req)
  (accum accfn  
    (each x posts* 
      (let href (string "/news?id=" (car x))
        (accfn (string (tostring (link (tostring (gentag img src (string "/" ((cadr x) 'cached)) class (link-class req x))) href) )))))))

(def is-ajax (req)
  (errsafe (or (alref (req 'args) "ajax") (is (string (alref (req 'cooks) "ajax")) "1"))))

(def render-content (content (o class "home") (o title "") (o req))
  (pr (render (if (is-ajax req) "html/mobile.html" "html/index.html")
        (list "<!--content-->" content)
        (list "<!--title-->" title)
        (list "<!--class-->" class)
        (list "<!--message-->" messages*))))
          
;==== pages ====          
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
          (list "<!--news-->" (string "<ul title=\"News\" id=\"news\">" (apply li (post-list req)) "</ul>"))
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
          (list "<!--vote-->" (string (vote-link req (string "/news?id=" (item 0)) item)))
          (list "<!--title-->" ((item 1) 'title))
          (list "<!--body-->" (erp:string "<a href=\"" ((item 1) 'link) "\">" 
                                "<img src=\"" (string "/" ((item 1) 'cached)) "\"/>"
                              "</a>")))
          "news" (string " News: " ((item 1) 'title)) req))
    (render-content 
      (render "html/news.html" (list "<!--body-->" (string "<ul>" (apply li (post-list req)) "</ul>")))
        "news" " News" req)))

(defopl submit req
  (let user (get-user req)
    (urform user req
      (do
        (new-post (alref (req 'args) "img") (alref (req 'args) "link") (alref (req 'args) "title"))
        "news")
      (render-content (tostring 
        (inputs 
          title "title" 30 ""
          img "image url" 30 ""
          link "source link" 30 "")
        (submit))))))


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
