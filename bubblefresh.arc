(load "ssdt.arc")
(load "erp.arc")
;(load "bubblefresh.arc")(thread (asv))(init 'dev)
(= bubblefresh-posts-dir* "arc/bubblefresh/posts/" )
(= bubblefresh-comments-dir* "arc/bubblefresh/comments/" )

(def init ((o env 'live))
  (= posts* (table) 
    comments* (table) 
    messages* '())
  (ensure-dir bubblefresh-posts-dir*)
  (ensure-dir bubblefresh-comments-dir*)
  (if (is env 'live)
    (= base-img-url* "http://static.bubblefresh.com/img/posts/"))
  (if (is env 'dev)
    (= base-img-url* "http://127.0.0.1/img/posts/"))

  (= maxpost* 0)
  (= maxcomment* 0)
  (load-comments)  
  (load-posts))

(deftem 
  post 
  id 0
  parent 0
  title "" 
  img ""
  text ""
  folder ""
  link "" 
  by ""
  up '()
  down '()
  view '()
  flag '()
  dead '()
  children '()
  
  pending #t
  )


(deftem 
  comment 
  id 0
  parent 0
  text ""
  by ""
  up '()
  down '()
  flag '()
  children '()
  )  

(deftem
  profile
  user ""
  g 1
  m 1
  posts '()
  comments '()
  warns '()
  email ""
  body "")
  
  
(attribute ul class opstring)
(attribute ul id    opstring)

(def load-posts ()
  (each id (map string (dir bubblefresh-posts-dir*))
      (= maxpost* (max maxpost* (coerce id 'int)))
      (= (posts* id) (temload 'post (string bubblefresh-posts-dir* id)))))
      
(def load-comments ()
  (each id (map string (dir bubblefresh-comments-dir*))
      (= maxcomment* (max maxcomment* (coerce id 'int)))
      (= (comments* id) (temload 'comment (string bubblefresh-comments-dir* id)))))

(def save-post (item)
    (save-table (item 1) (string bubblefresh-posts-dir* (item 0)))
    (= (posts* (item 0)) (item 1)))

(def save-comment (item)
    (save-table (item 1) (string bubblefresh-comments-dir* (item 0)))
    (= (comments* (item 0)) (item 1)))

(def cache-img (src x y w h (o folder (string (rand-string 20))))
  (if (begins src "http://")
    (with (from (cut src 0 (urlend src 0))
            folder (clean-title folder))
      (ensure-dir (string "./static/img/posts/"folder))
      (system (erp:string "cd ./static/img/posts/"folder";\
        convert "from" \\( +clone -resize 500x500 -write orig.jpg +delete \\) -crop "w"x"h"+"x"+"y" -resize 150x150 thumb.jpg"))
      folder)))
    
    
(def clean-int (i)
  (coerce (mz:regexp-replace* "[^0-9]+" i "") 'int))
  
(def clean-url (url)
  (mz:regexp-replace* "[^A-Za-z0-9+/_:?#posts*&.]+" url "")) ;is this safe?
  
(def clean-title (title)
(let title (coerce title 'string)
  (mz:regexp-replace* "[^A-Za-z0-9+]+" title "_")))
  
(def new-post (parent title link body by img (o x 0) (o y 0) (o w 300) (o h 300))
    (save-post 
      (let id (string (++ maxpost*))
        (list id (inst 'post 
                  'id id
                  'parent 0
                  'by by
                  'title (eschtml (striptags title))
                  'body (eschtml (striptags body))
                  'link (clean-url link)
                  'img (clean-url img)
                  'folder (cache-img (clean-url img) x y w h id ))))))

(def new-comment (parent text by)
    (save-comment 
      (let id (string (++ maxcomment*))
        (push id ((parent 1) 'children))
        (if ((parent 1) 'title)
          (save-post parent)
          (save-comment parent))
        (list id (inst 'post 
                  'id id
                  'by by
                  'text (eschtml (striptags text)))))))

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
            (pr "<span class=\"up\">&and;" (len ((item 1) 'up)) "</span>")
            (rlinkf (string "<span class=\"up\">&and;" (len ((item 1) 'up)) "</span>") (req) (up-vote req item) href))
          (if (some user ((item 1) 'down))
            (pr "<span class=\"down\">&or;"  (len ((item 1) 'down))"</span>")
           (rlinkf 
            (string "<span class=\"down\">&or;" (len ((item 1) 'down)) "</span>") 
            (req) 
            (down-vote req item) 
            href)))))))

(def link-class (req item)
  (with (user (get-user req))
    (if user
      (string
        "link "
        (if (some (get-user req) ((item 1) 'up)) "up")
        (if (some (get-user req) ((item 1) 'down)) "down")))))
        
(attribute img class opstring)

(def post-thumb (x)
  (string base-img-url* ((x 1) 'folder) "/thumb.jpg"))
  
(def post-img (x)
  (string base-img-url* ((x 1) 'folder) "/orig.jpg"))

(def comment-list (req items)
  (ero items)
  (if (comments* (car items))
    (prn
     "<li>"
      ((comments* (car items)) 'text)
      (comment-list req ((comments* (car items)) 'children))
      "</li>"))
    (if (cdr items)
      (comment-list req (cdr items))))
  
(def post-list (req)
  (accum accfn  
    (each x posts* 
      (let href (string "/news?id=" (x 0))
        (accfn (string 
                  "<a href=\""href"#"((x 1) 'title)"\" class=\""(link-class req x)"\">" 
                    "<div>"
                      "<div class=\"up\">"(len ((x 1) 'up))"</div>"
                      "<div class=\"down\">"(len((x 1) 'down))"</div>"
                    "</div>"
                    "<img src=\""(post-thumb x)"\"/>"
                  "</a>"))))))


(def is-ajax (req)
  (errsafe (or (alref (req 'args) "ajax") (is (string (alref (req 'cooks) "ajax")) "1"))))

(def render-content (content (o class "home") (o title "") (o req))
  (pr (render (if (is-ajax req) "html/mobile.html" "html/index.html")
        (list "<!--content-->" content)
        (list "<!--title-->" title)
        (list "<!--class-->" class)
        (list "<!--message-->" messages*))))
(def submit-link (req)
  (iflet user (get-user req)
    "<li><a href=/submit>Add</a></li>"))
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
     ;TODO: not needed now
     ;(iflet user (get-user req)
     ;   (item-viewed req item))
      (render-content 
        (render "html/news.html" 
          (list "<!--breadcrumbs-->" (string ((item 1) 'title)" &lt; <a href=/news>news</a> &lt; <a href=/>home</a>"))
          (list "<!--vote-->" (string (vote-link req (string "/news?id=" (item 0) "#"((item 1) 'title)) item)))
          (list "<!--title-->" ((item 1) 'title))
          (list "<!--title-->" ((item 1) 'title))
          (list "<!--comment-link-->" (comment-link req item (item 0) ))
          (list "<!--comments-->" (tostring (comment-list req ((item 1) 'children))))
          (list "<!--body-->" (string "<a href=\"" ((item 1) 'link) "\">" 
                                "<img src=\"" (post-img item) "\"/>"
                              "</a>")))
          "news" (string " News: " ((item 1) 'title)) req))
    (render-content 
      (render "html/news.html" 
        (list "<!--breadcrumbs-->" "news &lt; <a href=/>home</a>")
        (list "<!--body-->" (string "<ul>"(submit-link req) (apply li (post-list req)) "</ul>")))
        "news" " News" req)))


(defop submit req
   (if (get-user req)
     (let user (get-user req)
       (urform user req
          (do
            (new-post 
              'news
              (alref (req 'args) "title") 
              (alref (req 'args) "link") 
              (alref (req 'args) "body") 
              user
              (alref (req 'args) "img") 
              (clean-int (alref (req 'args) "x"))
              (clean-int (alref (req 'args) "y"))
              (clean-int (alref (req 'args) "w"))
              (clean-int (alref (req 'args) "h")))
            "news")
          (render-content 
             (render "html/submit.html" 
              (list "<!--body-->" (tostring 
                (inputs
                  title "title" 30 ""
                  link "source link" 30 "" 
                  img "image url" 30 "")
                (pr "<input type=\"hidden\" name=x value=0>")
                (pr "<input type=\"hidden\" name=y value=0>")
                (pr "<input type=\"hidden\" name=w value=0>")
                (pr "<input type=\"hidden\" name=h value=0>")
                (submit)))))))
  (login-page req 'login
          "You need to be logged in to do that."
          (list (fn (u ip))
                (string 'submit (reassemble-args req))) )))


(def comment-link (req parent newsid)
   (if (get-user req)
     (let user (get-user req)
       (tostring 
        (urform user req
          (do
            (new-comment
              parent ;parent
              (alref (req 'args) "text") 
              user);by
            (string "/news?id="newsid))
          (inputs text "Comment:" 30 "")
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
