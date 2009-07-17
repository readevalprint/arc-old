(def prime (start end)
  (if (even start)
    (prn start " is even"))
  (if (< start end)
    (prime (+ start 1) end)))
    
