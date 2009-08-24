(load "erp.arc")
        
(def readlinebytes (inf)
  ; returns ("string" ([list of bytes]))
  (if (peekc inf)
    (with (charbuf nil bytebuf nil) 
      (list 
        (tostring 
          (while (and (peekc inf) (isnt #\newline (peekc inf)))
            (pr (coerce (peekc inf) 'string)) 
            (push (readb inf) bytebuf))
            (pr (coerce (peekc inf) 'string))
            (push (readb inf) bytebuf))
         bytebuf))
     nil))

(def parseform (file s)
  (time:w/infile inf file
    (with (filename nil args nil name nil path nil state nil out nil sep s)
      (whilet l (readlinebytes inf)
        (when (headmatch sep (car l))
          ;if new part, save last one
          (when filename
            (if out (close out))
            (= state nil))
          ;check Content-Disposition:
          (let contdisp (readline inf)
            (iflet i (findsubseq "name=\"" contdisp)
              (if (isnt i -1)
                (= name (cut contdisp (+ 6 i) (findsubseq "\"" contdisp (+ 6 i) )))))
            (iflet i (findsubseq "filename=\"" contdisp)
              (if (is i -1)
                (= state 'data) 
                (do
                  (readline inf)
                  (= state nil)
                  (= filename (cut contdisp (+ 10 i) (findsubseq "\"" contdisp (+ 10 i))))
                  (when (isnt "" filename)
                    (= state 'file1)
                    (= out (outfile (= path (+ file "_" (rand-string 4) "_" filename)) 'append))
                    (push (list name (list filename path)) args))))
              (= state nil))  
            (readline inf)))
        (if (is state 'data)
          )
        (if (is state 'file1)
          (= state 'file2)
          (if (is state 'file2)
            (map [writeb _ out]  (rev (cadr l))))))
      (if out (close out))
      args)))
      
      
(def string-to-list (str)
  ;
  ;  returns a list of chars of the given str
  ;  arc> (stringtolist "Hello world!")
  ;  (#\H #\e #\l #\l #\o #\space #\w #\o #\r #\l #\d #\!)
  ;
  (with (line nil) 
    (w/instring ins str 
      (whiler c (readc ins ) nil 
        (push c line)))
        line))
        
              
