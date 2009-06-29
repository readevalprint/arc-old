(= guessdir* "arc/guesses/"  maxid* 0  guesses* (table))

(deftem guess id nil  title nil  text nil)


(defop hello2 req
  (w/link (pr "there") 
    (pr "here")))

(defop hello3 req
  (w/link (w/link (pr "end")
            (pr "middle"))
    (pr "start")))

(defop hello4 req
  (aform [w/link (pr "you said: " (arg _ "foo"))
           (pr "click here")]
    (input "foo")
    (submit)))

(def guess1 (y r)
	(if (is y r)
		(do (prn "yay")                  
			(pr "you got it"))
		(do (prn y " is too " (if (< y r) "low" "high"))
			)))
              
(defop guess req
  (guess (get-user req)
     '((int g 1 t t "Enter guess:"))
     (fn () (guess1 (alref (req 'args) "g") 2))
     "Guess"))
		
(defopl example req
  (let user (get-user req)
    (uform user req (prn "Hello " user)
      (prn "This is the example page.") (submit))))

(defopl vars-form req
  (vars-form (get-user req)
     '((int g 1 t t "Enter guess:"))
     (fn (name val) (prn name " " val) (br))
     (fn () (guess1 (alref (req 'args) "g") 2))
     "Guess"))

(defop cookies req (prn (alref (req 'cooks) "sessionid" )))
