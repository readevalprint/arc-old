;calculate a random number where:  a <= rand < b
(def rand2 (a b)
  (+ (rand (- b a)) a))

(def make-matrix (i j (o fill 0.0))
  (if (< 0 j)
    (do
      (make-matrix j 0 (make-matrix i 0 fill)))
      (if (< 0 i)
        (cons fill (make-matrix (- i 1) 0 fill)))))
 
    
(def sigmoid (x)
  (/ 1 (+ 1 (expt 2.71828183 x))))
  ;our sigmoid fn, no tanh in arc :'(
  
(def dsigmoid (y)
  (- 1 (expt y 2)))
  
(def initNN (nn ni nh no)
  )
