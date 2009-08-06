(load "erp.arc")

(def post-scores ()
  (accum accfn
    (each x posts*
      (accfn (list (coerce (x 0) 'int) (- (len ((x 1) 'up)) (len ((x 1) 'down))))))))
      

(def post-weight (p g m)
  (nearest (- (p 1) (expt (* m (- maxpost* (p 0)) ) g)) .01))

(def sort-weight (a b g m) ;g = gravity , m = multiplier of the post id
  (prn "(post-weight "a" "g" "m") :"(post-weight a g m)) 
  (prn "(post-weight "b" "g" "m") :"(post-weight b g m)) 
  (> (post-weight a g m) (post-weight b g m)))

(def sort-best (a b)
 (> (a 1) (b 1)))
  
  
(def sort-posts (posts g m)
  (sort (fn (a b) (sort-weight a b g m)) posts))
  
(def pullnil ()
  (each x posts*
    (= ((x 1) 'up) (pull nil ((x 1) 'up)))
    (prn x)
    (save-post x))
    nil)
