(load "ssdt.arc")
(load "erp.arc")
(load "score.arc")

;(load "bubblefresh.arc")(thread (asv))(init 'dev)

(= bubblefresh-posts-dir* "arc/bubblefresh/posts/" )
(= bubblefresh-comments-dir* "arc/bubblefresh/comments/" )
  
  
(def bsv ()
  (init)(thread (asv 42697)))
  
(def init ((o env 'live))
  (= dead-msg* (string "\n" (tostring (render-content "Unknown or expired link."))))
  (= posts* (table) 
    comments* (table) 
    messages* '())
  (ensure-dir bubblefresh-posts-dir*)
  (ensure-dir bubblefresh-comments-dir*)
  (if (is env 'live)
    (= base-img-url* "http://static.bubblefresh.com/img/posts/"))
  (if (is env 'dev)
    (= base-img-url* "http://127.0.0.1/img/posts/"))
  (=  g* 1
      m* .8
      maxpost* 0
      maxcomment* 0)
  (load-comments)  ;need to sort on save not on page view
  (load-posts)
  (update-sort-posts)) ;have to keep this last, since posts* is from 'load-posts


(deftem 
  post 
  id 0
  parent 0
  title nil
  img nil
  text nil
  folder nil
  link nil 
  by nil
  up '()
  down '()
  view '()
  flag '()
  dead '()
  children '()
  time (seconds)
  pending #t
  )


(deftem
  profile
  user ""
  g 1
  m .8
  posts '()
  comments '()
  warns '()
  email ""
  time (seconds)
  body "")
  
  
(attribute ul class opstring)
(attribute ul id    opstring)

(def load-posts ()
  (each id (map string (dir bubblefresh-posts-dir*))
      (= maxpost* (max maxpost* (coerce id 'int)))
      (= (posts* id) (temload 'post (string bubblefresh-posts-dir* id)))))

(def all-post-ids ()
  (sort (fn (a b)(> (coerce a 'int) (coerce b 'int))) 
    (let y (list) (each x posts* (push (x 0) y)) y)))

  
(def load-comments ()
  (each id (map string (dir bubblefresh-comments-dir*))
      (= maxcomment* (max maxcomment* (coerce id 'int)))
      (= (comments* id) (temload 'post (string bubblefresh-comments-dir* id)))))

(def update-sort-posts ()
  (= sortedposts* (sort-items (item-scores (all-post-ids) posts*) g* m*)))


(def save-post (item)
    (save-table (item 1) (string bubblefresh-posts-dir* (item 0)))
    (= (posts* (item 0)) (item 1))
    (update-sort-posts))

(def save-comment (item)
    (save-table (item 1) (string bubblefresh-comments-dir* (item 0)))
    (= (comments* (item 0)) (item 1)))

(def cache-img (src x y w h (o folder (string (rand-string 20))))
  (if (begins src "http://")
    (with (from (cut src 0 (urlend src 0))
            folder (clean-title folder))
      (ensure-dir (string "./static/img/posts/"folder))
      (system (string "cd ./static/img/posts/"folder";\
        convert "from" \\( +clone -resize 500x500 -write orig.jpg +delete \\) -crop "(clean-int w)"x"(clean-int h)"+"(clean-int x)"+"(clean-int y)" -resize 75x75 thumb.jpg"))
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
    (if ((item 1) 'title)
      (save-post item)
      (save-comment item)))
  
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
        (if ((item 1) 'title)
      (save-post item)
      (save-comment item))))

(def down-vote (req item)
  (with (user (get-user req))
    (if (some user ((item 1) 'up))
        (pull user ((item 1) 'up))
      (if (no (some user ((item 1) 'down)))
          (pushnew user ((item 1) 'down))))
      (if ((item 1) 'title)
      (save-post item)
      (save-comment item))))

(def score (item)
  (let s (- (len ((item 1) 'up))
  (len ((item 1) 'down)))
  (tostring   
    (spanclass "score" (pr (plural s " point"))))))

(def user-link (item)
  (string "<a href=\"/profile?id="((item 1) 'by)"\" >"((item 1) 'by)"</a>"))

(def vote-link (req href item)
  (with (user (get-user req))
    (if user
      (tostring 
        (spanclass "vote" 
          (if (some user ((item 1) 'up))
            (pr "<span class=\"up\">&and;</span>")
            (rlinkf (string "<span class=\"up\">&and;</span>") 
              (req) 
              (up-vote req item) 
              href))
          (if (some user ((item 1) 'down))
            (pr "<span class=\"down\">&or;</span>")
           (rlinkf 
            (string "<span class=\"down\">&or;</span>") 
            (req) 
            (down-vote req item) 
            href))))
          "")))

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

(def item-by (item)
  (tostring (spanclass "by"  (pr "by " (user-link item)))))


(def comment-link (req parent href)
   (if (get-user req)
     (let user (get-user req)
       (tostring 
        (urform user req
          (do
            (new-comment
              parent ;parent
              (alref (req 'args) "text") 
              user);by
            href)
          (pr "Comment:<br /><textarea value=\"\"  name=\"text\" rows=\"3\" cols=\"60\"></textarea><br />")
          (submit))))
          ""))
  
(def comment-list (req items (o href ""))
  (iflet comment (comments* ((car items) 2))
    (let item (list ((car items) 2) comment)
      (prn
       "<li id="(item 0)">"
        (vote-link req (string href"#"(item 0)) item) " "
        (score item) " "
        (item-by item) " | "
        "<a href=\"" href "#"(item 0)"\">link</a>"
        "<div>"
          (comment 'text) " "
        "</div>"
        (if (get-user req)
        (string "<div class=\"reply\">
          <a href=\"#\" class=\"over\">reply</a>"
          "<div class=\"form\">"(comment-link req item (string href"#"(item 0)) )"</div>"
        "</div>"
        )
        "")
        (if (comment 'children)
          (string
            "<ul>"
            (tostring (comment-list req (sort-items (item-scores (comment 'children) comments*) g* m*) href))
            "</ul>")
          "")
        "</li>")))
    (if (cdr items)
      (comment-list req (cdr items) href)))
  
(def post-list (req)
  (accum accfn  
    (each p sortedposts* ; ((post id) ..)
      (withs (item (list (p 2) (posts* (p 2)))
            href (string "/news?id=" (item 0))
            a (string "<a href=\""href"&"((item 1) 'title)"\" class=\""(link-class req item)"\">" ))
        (accfn (string 
                  a "<img src=\""(post-thumb item)"\"/> </a>"
                   "<span class=info>"
                      ((item 1) 'title) "<br/>"
                      (score item) "<br />"
                     (text-age:item-age (item 1)) "<br/>"
                      (item-by item)
                    "</span>"
                 ))))))


(def is-ajax (req)
  (errsafe (or (alref (req 'args) "ajax") (is (string (alref (req 'cooks) "ajax")) "1"))))

(def render-content (content (o class "home") (o title "") (o req ""))
  (pr (render  "html/index.html"
        (list "<!--content-->" content)
        (list "<!--title-->" title)
        (list "<!--class-->" class))))
        


(def hello-page (user ip)
  "") 

        
;==== pages ====          
(defop-raw || (str req) (w/stdout str
  (prn "Set-Cookie: ajax=0")
  (prn)
  (prn (render "html/index.html" 
        (list "<!--content-->" (render "html/expando/bubblefresh-loves-you.html"))
        (list "<!--title-->" " Home")
        (list "<!--class-->" "home")))))

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
    (withs (item (list (arg req "id") it)
          href (string "/news?id=" (item 0) "&"((item 1) 'title)))
     ;TODO: not needed now
     ;(iflet user (get-user req)
     ;   (item-viewed req item))
      (render-content 
        (render "html/news.html" 
          (list "<!--breadcrumbs-->"  (string ((item 1) 'title)" &lt; <a href=/news>news</a> &lt; <a href=/>home</a>"))
          (list "<!--vote-->"         (vote-link req href item))
          (list "<!--score-->"        (score item))
          (list "<!--by-->"           (item-by item))
          (list "<!--title-->"        ((item 1) 'title) )
          (list "<!--comment-link-->" (comment-link req item (string "/news?id="(item 0)) ))
          (list "<!--comments-->"     (if ((item 1) 'children)
                                        (tostring (comment-list req 
                                                    (sort-items (item-scores ((item 1) 'children) comments*) g* m*) href))
                                        ""))
          (list "<!--body-->"         (string "<a href=\"" ((item 1) 'link) "\">" 
                                                "<img src=\"" (post-img item) "\"/>"
                                              "</a>"
                                              "<div>"
                                                ((item 1) 'body)
                                              "</div>")))
          "news" (string " News: " ((item 1) 'title)) req))
    (render-content 
      (render "html/news.html" 
        (list "<!--breadcrumbs-->" "news &lt; <a href=/>home</a>")
        (list "<!--body-->" (string "<ul>" (apply li (post-list req)) "</ul>")))
        "news" " News" req)))


(defop submit req
   (if (get-user req)
     (let user (get-user req)
        (render-content 
           (render "html/submit.html" 
              (list "<!--fnid-->"  (rflink (fn (req) 
                                    (tostring (new-post 
                                      0 
                                      (arg req "title") 
                                      (arg req "link")
                                      (arg req "body")
                                      user
                                      (arg req "img")
                                      (arg req "x")
                                      (arg req "y")
                                      (arg req "w")
                                      (arg req "h")))
                                    "news"))))
                      "submit" " Submit" req))  
          (render-content (tostring  
                            (login-form "Login" 'login login-handler (list (fn (a b))  "submit")))
                               "submit" " Submit" req)))


(defop magazine req
  (render-content "<ul><li><a href=\"\">123</a></li></ul>" "magazine" " Magazine" req))

(defop bonus req
  (if (get-user req)
      (render-content (render "html/bonus.html" ) "bonus" " Bonus" req)
      (render-content (tostring  
                        (login-form "Login" 'login login-handler (list (fn (a b))  "bonus"))
                            ) "bonus" " Bonus" req)))
     
(defop login req 
  (render-content (tostring  
                        (login-form "Log in" 'login login-handler (list (fn (a b))  "/"))
                            ) "home" " Login" req))
                            
(defop logout req
  (aif (get-user req)
     (do 
        (logout-user it)
          (render-content "Logged out." "home logout" " Logout" req) )
          (render-content "You were not logged in." "home logout" " Logout" req) ))
          
(defop register req
  (render-content 
   (render "html/register.html" 
    (list "<!--body-->" (tostring (aform (fn (req) (render-content "asdf")) (submit)))))
    "register" " Register" req))
                
                
                
(def failed-login (switch msg afterward)
  (flink (fn ignore (render-content (tostring  
            (login-form "Login" 'login login-handler afterward))
              ) "login" " Login")))
              
(def text-age (a)
  (tostring
    (if (>= a 1440) (pr (plural (trunc (/ a 1440)) "day")    " ago")
        (>= a   60) (pr (plural (trunc (/ a 60))   "hour")   " ago")
                    (pr (plural (trunc a)          "minute") " ago"))))

(def item-age (i) (minutes-since i!time))              
