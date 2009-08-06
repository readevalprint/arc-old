(load "erp.arc")

(def item-scores (ids from) ;ids would be a list of posts* ids or the entire list
  (accum accfn
    (each id ids
      (with (item (from id))
        (accfn (list (pos id ids) (- (len (item 'up)) (len (item 'down))) id))))))

(def sort-items (items g m)
  (sort (fn (a b) (sort-item-weight a b g m)) items))

(def sort-item-weight (a b g m) ;g = gravity , m = multiplier of the post pos in list
  ;(prn "(post-weight "a" "g" "m") :"(post-weight a g m)) 
  ;(prn "(post-weight "b" "g" "m") :"(post-weight b g m)) 
  (> (item-weight a g m) (item-weight b g m)))

(def item-weight (c g m) ;(= c (pos score id))
  (nearest (- (c 1) (expt (* m (c 0) ) g)) .01))


(def post-weight (p g m)
  (nearest (- (p 1) (expt (* m (- maxpost* (p 0)) ) g)) .01))

(def post-scores ()
  (accum accfn
    (each x posts*
      (accfn (list (x 0) (- (len ((x 1) 'up)) (len ((x 1) 'down))))))))

(def sort-post-weight (a b g m) ;g = gravity , m = multiplier of the post id
  ;(prn "(post-weight "a" "g" "m") :"(post-weight a g m)) 
  ;(prn "(post-weight "b" "g" "m") :"(post-weight b g m)) 
  (> (post-weight a g m) (post-weight b g m)))

(def sort-best (a b)
 (> (a 1) (b 1)))
  
  
(def sort-posts (posts g m) ; (= posts ((id score)...))
  (sort (fn (a b) (sort-post-weight a b g m)) posts))
  
(def pullnil ()
  (each x posts*
    (= ((x 1) 'up) (pull nil ((x 1) 'up)))
    (prn x)
    (save-post x))
    nil)
