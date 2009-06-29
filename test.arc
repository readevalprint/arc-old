; Guess the number from 0 to 100. June 5, 2009 rev 1

; To run:
; arc> (load "test.arc")
; arc> (guess)


(def guess ()
	(with (tries 10 r (rand 100))
		(while (> tries 1)
			(= tries (- tries 1))
			(prn tries " tries")
			(pr "your guess: ")
			(= y (read))
			(prn)
			(if (< y r) (prn y " is too low" )
				(> y r) (prn y " is too high" )
				(do 
					(prn "yay")
					(pr "you got it in " (- 10 tries) " tries.")
					(= tries 1))))))
					
(guess)
