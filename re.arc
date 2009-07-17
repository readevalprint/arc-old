;By Conrad Barski, Sep 2008
;Licensed under GPLv3

(def charlist (str)
  (coerce str 'cons))

(def ord (c)
  (coerce c 'int))

(def match-parens (text (o n 0) (o acc))
  (when text
    (withs ((cur . rest) text
            f (fn ()
                (cons cur acc)))
         (case cur
           #\\ (match-parens cdr.rest n (cons car.rest (f)))
           #\( (match-parens rest ++.n (f))
           #\) (if (is n 0)
                   (list rev.acc rest)
                   (match-parens rest --.n (f)))
           (match-parens rest n (f))))))

(def match-brak (text)
  (let (a b) (split copy.text (pos #\] text))
       (list a cdr.b)))

(def parse-delim (text)
  (with (comma (pos #\, text)
         end (pos #\} text))
    (list (ord:string:cut text 0 comma) (ord:string:cut text (+ comma 1) end) (cut text (+ end 1)))))

(def find-pipe (text (o acc))
  (when text
    (case car.text
      #\( (let (sub rest) (match-parens cdr.text)
               (find-pipe rest (+ '(#\)) rev.sub '(#\() acc)))
      #\| (list rev.acc cdr.text)
      #\\ (find-pipe cddr.text (cons cadr.text (cons car.text acc)))
      #\) nil
      (find-pipe cdr.text (cons car.text acc)))))

(def match-pipes (re text acc cont)
  (iflet (part rest) find-pipe.re
         (or (match-core part text acc nil)
             (match-pipes rest text acc cont))
         (match-core re text acc cont)))

(def match-char (char c)
  (when (and char (isnt c #\nul))
    (if (is car.char #\^) (no:match-char cdr.char c)
        (is car.char #\\) (case cadr.char 
                            #\s (in c #\  #\tab #\return #\newline #\return) 
                            #\w (or (is c #\_) alphadig.c)
                            #\d (<= #\0 c #\9)
                            (or (is c cadr.char) (match-char cddr.char c)))
        (and (is cadr.char #\-) (>= ord.c (ord:car char)) (<= ord.c (ord:car:cddr char))) t
        (is car.char c) t
        (match-char cdr.char c))))

(= shorthand* '(#\s #\w #\d))

(def match-core (re text acc cont)
  (withs (subex (fn (sub rest)
                  (case car.rest
                    #\* (match-delim sub 0 nil cdr.rest text acc cont)
                    #\+ (match-delim sub 1 nil cdr.rest text acc cont)
                    #\? (match-delim sub 0 1 cdr.rest text acc cont)
                    #\{ (let (from to rest2) (parse-delim:cdr rest)
                             (match-delim sub from to rest2 text acc cont))
                    (if (and (isnt car.sub #\[) (len> sub 1))
                        (match-delim sub 1 1 rest text acc cont))))
          dochar (fn (char rest)
                   (if (match-char char car.text) 
                       (aif (subex (+ '(#\[) char '(#\])) rest) it
                            (match-core rest cdr.text (cons car.text acc) cont) it
                            cont (cont))
                       cont (cont))))
    (aif no.re (obj result rev.acc rest text cont cont)
         (is car.re #\() (apply subex (match-parens:cdr re))
         (and (is car.re #\$) (in car.text nil #\newline #\return)) (match-core cdr.re text acc cont)
         (and (is car.re #\^) (or (in car.text #\newline #\nul))) (match-core cdr.re cdr.text acc cont)
         no.text nil
         (is car.re #\\) (if (mem cadr.re shorthand*) (dochar (list car.re cadr.re) cddr.re)
                             (is cadr.re car.text) (match-core cddr.re cdr.text (cons car.text acc) cont)
                             cont (cont))
         (is car.re #\[) (apply dochar (match-brak cdr.re))
         (subex (list:car re) cdr.re) it
         (in car.re #\. car.text) (match-core cdr.re cdr.text (cons car.text acc) cont)
         cont (cont))))

(def match-delim (sub from to re text acc cont)
  (let f (fn ()
           (if (<= from 0)
               (match-pipes re text acc cont)
               cont (cont)))
    (aif (is to 0) (match-pipes re text acc cont)
         (match-pipes sub text nil nil) (match-delim sub
                                                     --.from
                                                     (when to
                                                       --.to)
                                                     re
                                                     it!rest
                                                     (+ (rev it!result) acc)
                                                     f)
         (f))))
  
wipe.re-text*
wipe.re-paths*
wipe.re-fail

(def re-choose (x)
  (if x
      (ccc (fn (cc)
             (push (fn ()
                     (cc:re-choose (and x!cont (x!cont))))
                   re-paths*)
             x))
      (re-fail)))

(def re (str)
  (let ^ (re-choose:match-pipes charlist.str re-text* nil nil)
    (= re-text* ^!rest)
    (string:^ 'result)))

(def settarget (str fun)
  (ccc (fn (cc)
         ((afn (x)
            (when x
              (= re-text* x)
              (= re-fail
                 (fn ()
                   (if re-paths*
                       (pop.re-paths*)
                       (cc:self:cdr x))))
              (let k (fun)
                wipe.re-paths*
                (list k re-text*))))
          str))))

(mac w/target (str . body)
  `(car:settarget (cons #\nul (charlist ,str))
                  (fn ()
                    ,@body)))

(mac draintarget (str . body)
  (w/uniq (f x k)
    `((rfn ,f (,x)
        (when ,x
          (whenlet ,k (settarget ,x
                                 (fn ()
                                   ,@body))
                   (cons (car ,k) (,f (cadr ,k))))))
      (cons #\nul (charlist ,str)))))

