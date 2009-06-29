;Guess the number - web app, June 2009 rev 2
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



(= guessdir* "arc/guess/" guesslist* (table) )
; this is the dir that our user guesses will be saved, and other vars we'll need
; not sure about the need of the * at the end

(deftem try lastguess 0 answer 0 attempt 0 total 1 best 0 average 0)
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
; like: #hash(("tim" . (#hash((total . 0) (best . 0) (average . 0) (lastguess . 0) (answer . 33) (attempt . 0)) . nil)))


(def save-try (try user) 
  (= (guesslist* user) (list try)) 
  ;save inside list for consistency of hash/table creation
  (save-table try (string guessdir* user)))
;saves a 'try with a corresponding user to the guesslist* and to the file
  
(def user-try (user)
  ((check (guesslist* user) [no (is _ nil)] (list (inst 'try 'answer (rand 100)))) 0)) 
;'check is used to get or make a try for a user, 0 is for consistency table/hash creation, see save-try
  
(def make-guess (guess user)
  (with (try (user-try user) guess (coerce guess 'int))
    (++ try!attempt)
    (= try!lastguess guess)
    (save-try try user)
    (if (is guess try!answer) ;you guessed it
        "win" ; you got it, good job!
        "guess"))) ;need to guess again, this is the defop to redirect to

      
(def new-game (user)
  (let try (user-try user)
    (= try!lastguess 0 
      try!total (+ 1 try!total)
      try!attempt 0 
      try!answer (rand 100))
    (save-try try user)))

(def hint (user)
  (let try (user-try user)
    (if 
      (< try!lastguess try!answer) (string try!lastguess " is too low")
      (> try!lastguess try!answer) (string try!lastguess " is too high"))))

(def update-stats (user)
  (let try (user-try user)
    (= try!best (max try!answer try!best))
    (= try!average (/ (+ try!average try!attempt) try!total ))
    ))

(def show-stats (user)
  (let try (user-try user)
    `(("games played:" ,try!total)
    ("average guesses:" ,try!average)
    ("best game:" ,try!best))))


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
          (update-stats user)
          (pr "good job")
          (br2)
          (pr "you guessed " try!answer " in " try!attempt " guess(es)")
          (br2)
          (map [prn (string _) "<br>"] (show-stats user)) ;ugh, arc needs an model,view,template framework, this is ugly
          (br)
          (rlinkf "play again" (req) (new-game user) "guess"))
      (pr "cheater"))))
;is there a way to makes sure a redirect is authorized via fnid?

                 
(def gsv ()
  (ensure-dir guessdir*)
  (load-tries)
  (thread (asv))) ;makes a thread so you can still have your command line
  
