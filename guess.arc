;Guess the number - web app, June 2009
;rev 4 - added stats for most played
;rev 3 - fix for 'avg throwing error on empty lists
;rev 2 - track and display stats
;rev 1 - simple game play


;"Days of hard work and gallons of coffee does a Programmer make."

;ATTN NOOBS LIKE ME
;read http://ycombinator.com/arc/tut.txt as it will explain many things
;also http://arcfn.com/doc/template.html http://arcfn.com/doc/table.html
;and set up REPL to loose less sanity http://arclanguage.org/item?id=10
;don't just look at the code here, load it up, play a game or two, inspect at the vars

;TODO make a method of working with html templates, maybe use 'atstrings

;note: a 'guess is a number, a 'try is a attempt made and the attributes encompassing it

;I'm also looking for someone to pay me to solve problems.
;tools: 
;  python, linux, django, javascript, actionscript, ruby, perl, enough html/css to work with designers.
;skills:
;  automation, OCR, distributed computing, system design, love of learning, making a mean cup of coffee
;interests:
;  lisp, designing shirts, machine learning, chiptunes, portuguese, painting
;(= email "coconutrandom@gmail.com")


(def init ()
  (= guessdir* "arc/guess/" guesslist* (table) )
  (ensure-dir guessdir*))


; this is the dir that our user guesses will be saved, and other vars we'll need
; not sure about the need of the * at the end

(deftem try lastguess 0 answer 0 attempt 0 games (list))
; makes a "template" ,not in the html sense, but a structured table or hash, 
; it is the closest equivalent to a model or object in other languages
; the defaults are set here and if overridden when the template is instantiated later
; you can access values of a template with the ! mark as in try!answer
; however when saving templates (which is a table), it more resembles pickling in python
; because in lisp,the code is data.

(def load-tries ()
  (each user (map string (dir guessdir*))
      (= (guesslist* user) (temloadall 'try (string guessdir* user)))))
; loads tries, takes every file in guessdir* and uses 
; the filename as 'user in guesslist*


(def save-try (try user) 
  (= (guesslist* user) (list try)) 
  ;save inside list for consistency of hash/table creation
  (save-table try (string guessdir* user)))
;saves a 'try with a corresponding user to the guesslist* and to the file
  
(def user-try (user)
  ((check (guesslist* user) [no (is _ nil)] (list (inst 'try 'answer (rand 100)))) 0)) 
;'check is used to get or make a try for a user, 0 is for consistency table/hash creation, see save-try
  
(def make-guess (guess user)
  (with (try (user-try user) guess (coerce (if (> (len guess) 0) guess "0") 'int))
    (++ try!attempt)
    (= try!lastguess guess)
    (save-try try user)
    (if (is guess try!answer) ;you guessed it
        (do
          (update-stats user)
          "win") ; you got it, good job!
        "guess"))) ;need to guess again, this is the defop to redirect to

      
(def new-game (user)
  (let try (user-try user)
    (= try!lastguess 0 
       try!attempt 0 
       try!answer (rand 100))
    (save-try try user)))

(def update-stats (user)
  (let try (user-try user)
    (= try!games (cons try!attempt try!games))
    (save-try try user)))
    
(def hint (user)
  (let try (user-try user)
    (if 
      (< try!lastguess try!answer) (string try!lastguess " is too low")
      (> try!lastguess try!answer) (string try!lastguess " is too high"))))

(def user-average (user)
  (let try (user-try user)
    (avg try!games)))

(def user-best (user)
  (let try (user-try user)
    (best < try!games)))

(def list-best ()
  (w/table b (table) 
    (maptable (fn (key val) (= (b key) (best < ((val 0) 'games)))) guesslist*)))
    ;b contains the best game of each user mapped to user name

(def list-most ()
  (w/table b (table) 
    (maptable (fn (key val) (= (b key) (len((val 0) 'games)))) guesslist*)))

(def gamesplayed ()
  (let b 0 
    (maptable (fn (key val) 
                (= b (+ b (len ((val 0) 'games) )))) 
      guesslist*) 
    b))
  
  
(def list-avg ()
  (w/table b (table) 
    (maptable (fn (key val) 
                (let x (val 0) 
                  (if (< 4 (len (x 'games))) (= (b key)(avg (x 'games))))))
                guesslist*)))
;b contains the average game of each user mapped to user name

(def bestof (l)
  (sort (fn (a b)
         (< (a 1) (b 1)))
   (tablist l)))

    
(def show-stats (user)
  (let try (user-try user)
    `(("your games played: " ,(len try!games))
    ("your average guesses: " ,(nearest (user-average user) .01))
    ("your best game: " ,(user-best user)))))

(def show-global-stats ()
  `(("number of players: " ,(len guesslist*))
    ("number of games: " ,(gamesplayed))
    ("<br> most played: " ,(let b (firstn 10 (rev (bestof (list-most))))
                                (map [list "<br>" (_ 0) " " (_ 1) ] b)))
    ("<br> best average after 4 games: " ,(let b (firstn 10 (bestof (list-avg))) 
                                (map [list "<br>" (_ 0) " " (nearest (_ 1) .01) ] b)))
    ("<br> best game: " ,(let b (firstn 10 (bestof (list-best))) 
                                (map [list "<br>" (_ 0) " " (nearest(_ 1) .01) ] b)))))
;this is so very crufty, optimally this would be passed to a html template


(defopl guess req
  (arform 
    [let user (get-user _) (make-guess (arg _ "g") user)]
    (pr (hint (get-user req)))
    (br)
    (single-input "Guess" 'g 5 "Submit" )))
;handles a request to /guess
;uses arform (redirect form) to handles guesses, as a user may have won or need t guess again
;this is decided but the return on 'make-guess
    
(defopl win req
  (with (try (user-try (get-user req)) user (get-user req))
    (if (is try!lastguess try!answer)
        (do 
          (pr "good job")
          (br2)
          (pr "you guessed " try!answer " in " try!attempt " guess(es)")
          (br2)
          (map [prn (string _) "<br>"] (show-stats user)) ;ugh, arc needs an model,view,template framework, this is ugly
          (br)
          (map [prn (string _) "<br>"] (show-global-stats)) ;copy/paste
          (rlinkf "play again" (req) (do  (new-game user)) "guess"))
      (pr "cheater"))))
;is there a way to makes sure a redirect is authorized via fnid?

                 
(def gsv ()
  (init)
  (load-tries)
  (thread (asv))) ;makes a thread so you can still have your command line
  
