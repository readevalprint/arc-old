(load "erp.arc")

(= upload-dir* "out/")
(= media-url* "http://127.0.0.1/static/")

(defop upload req
  (pr "<html>
<body>
<h1>form with files</h1>
<form action=\"\" method=\"post\"
enctype=\"multipart/form-data\">
<label for=\"file1\">File 1:</label>
<input type=\"file\" name=\"file1\" id=\"file\" /><br/>
<label for=\"foo\">Foo:</label>
<input name=\"foo\" value=\"bar\" /><br/>
<label for=\"files\">File 2:</label>
<input type=\"file\" name=\"file2\" id=\"file\" />
<br />
<input type=\"submit\" value=\"Submit\" name=\"submit\"/>
</form>
<br/>
<h1>normal form</h1>
<form action=\"\" method=\"post\">
<label for=\"foo\">Foo:</label>
<input name=\"foo\" value=\"bar\" />
<br />
<input type=\"submit\" value=\"Submit\" name=\"submit\"/>
</form>
<pre>" 
req
"</pre>
"(links req)"
</body>
</html> "))

(def links (req)
  (tostring
    (with (f1 (erp:alref req!args "file1")
          f2 (alref req!args "file2"))
      (if f1 (pr "<a href=\"" media-url* (cadr f1)"\">" (car f1) "</a>"))
      (if f2 (pr "<a href=\"" media-url* (cadr f2)"\">" (car f2) "</a>"))
    )))

(def handle-request-thread (i o ip)
  (with (nls 0 lines nil line nil responded nil t0 (msec))
    (after
      (whilet c (unless responded (readc i))
        (if srv-noisy* (pr c))
        (if (is c #\newline)
            (if (is (++ nls) 2) 
                (let (type op args n cooks seperator) (parseheader (rev lines))
                  (let t1 (msec)
                    (case type
                      get  (respond o op args cooks ip)
                      post (handle-post i o op args n cooks ip seperator)
                           (respond-err o "Unknown request: " (car lines)))
                    (log-request type op args cooks ip t0 t1)
                    (set responded)))
                (do (push (string (rev line)) lines)
                    (wipe line)))
            (unless (is c #\return)
              (push c line)
              (= nls 0))))
      (close i o)))
  (harvest-fnids))

(def parseheader (lines)
  (let (type op args) (parseurl (car lines))
    (list type
          op
          args
          (and (is type 'post)
               (some (fn (s)
                       (and (begins s "Content-Length:")
                            (errsafe:coerce (cadr (tokens s)) 'int)))
                     (cdr lines)))
          (some (fn (s)
                  (and (begins s "Cookie:")
                       (parsecookies s)))
                  (cdr lines))
          (and (is type 'post)
                 (some (fn (s)
                       (and (begins s "Content-Type: multipart/form-data")
                            (cut s (+ 9 (errsafe:findsubseq "boundary=" s))))) ;magic 9 for len of "boundery="
                     (cdr lines))))))

; if this is part of a file, it should be written to disk here
; but for now it will be in the args as (argname ("filename" "temp-filepath"))
; it would be nice if it wrote it directly where you wanted, perhaps specified in the form...
; assoc the form fnid to a list of file locations?
                
(def handle-post (i o op args n cooks ip seperator)
  (if srv-noisy* (pr "Post Contents: "))
    (if (no n)
      (respond-err o "Post request without Content-Length.")
      (if seperator
        (time (with ( line nil    
                charbuf nil     
                bytebuf nil     
                char nil     
                byte nil     
                namesep (string-to-list "name=\"")
                filenamesep (string-to-list "filename=\"")
                buflen (+ 6 (len (string-to-list seperator))) ;to account for various #/n #/r or #/- around it
                seperator (string-to-list seperator) 
                seplen (len (string-to-list seperator) )
                count 0     
                state 'seperator 
                b nil
                name ""
                val ""
                filename ""
                contenttype ""
                postargs nil
                tempfile ""
                wait nil
                out nil
                outf nil)
                
            (while (> n 0)
                  
              (when (>= (len charbuf) buflen)
                (= charbuf (rev charbuf))
                (= char (pop charbuf))
                (= charbuf (rev charbuf))
;                (= char (last charbuf))
;                (= charbuf (cut charbuf 0 (- (len charbuf) 1)))
                
                (= bytebuf (rev bytebuf))
                (= byte (pop bytebuf))
                (= bytebuf (rev bytebuf)))
              
              (while (< (len charbuf) buflen)
                (-- n)
                (push (peekc i) charbuf)
                (push (mz:read-byte i) bytebuf))
            
              (when (iso seperator (cut charbuf 4 (+ 4 seplen)))
                (++ count)
                (if (is state 'data)
                  (push (list name val) postargs))
                (when (is state 'filedata)
                  (if outf (close outf))
                  (push (list name (list filename out)) postargs))
                (= val "" name "" filename "" arg "")
                (= state 'formname)
                (= buflen (len namesep))
                (= charbuf (cut charbuf 0 buflen)))

;               (prn)
;               (pr "c:")
;               (write charbuf)
;               (prn)
;               (pr "b1:")
;               (write (cut charbuf 2 (+ 2 (len seperator))))
;               (prn)
;               (pr "b1:")
;               (write (cut charbuf (len seperator)))
;               (prn)
;               (pr "s:")
;               (write seperator)
;               (prn)
;               (if srv-noisy* (erp state))

              
              (case state
                formname 
                  (if (iso charbuf namesep)
                    (= state 'wait1))

                wait1 
                  (if (is char #\") ;"
                    (= state 'getname))

                getname 
                  (if (isnt char #\") ;"
                    (= name (+ name char))
                    (do 
                      (if srv-noisy* (erp name))
                      (= state 'findtype)
                      (= buflen (len filenamesep))))

                findtype 
                    (if (iso charbuf filenamesep)
                      (= state 'wait2)
                      (when (or (iso (cut charbuf (- buflen 3)) '(#\newline #\return #\newline))
                                (iso (cut charbuf  (- buflen 3)) '(#\return #\newline #\newline)))
                        (= buflen (+ 6 seplen))
                        (= state 'data)))
                
                data
                 (if (and (isnt char #\return) (isnt char #\newline))
                        (= val (+ val char)))
                    
                wait2
                  (if (is char #\") ;"
                    (= state 'isfile))

                isfile 
                  (if (isnt char #\") ;"
                    (= filename (+ filename char))
                    (do 
                      (= state 'contenttype)
                      (= buflen 4)))
                      
                contenttype
                  (if (or (iso charbuf '(#\newline #\return #\newline #\return))
                          (iso charbuf '(#\return #\newline #\return #\newline)))
                    (do
                      (= contenttype (+ contenttype char))
                      (if srv-noisy* (erp contenttype))
                      (= state 'wait3)
                      (= charbuf nil)
                      (= char nil)
                      (= byte nil)
                      (= bytebuf nil)
                      (= buflen (+ 7 seplen))
                      (= out (string upload-dir* (rand-string 10) "_" filename))
                      (= outf (outfile out 'append)))
                    (if (and (isnt char #\newline) (isnt char #\return) )
                      (= contenttype (+ contenttype char))))
                    
                wait3
                  (if charbuf (= state 'filedata))
                      

                filedata ;woot finally!
                  (if char
                    (mz:write-byte byte outf))))
                    
            (if srv-noisy* (pr "\n\n"))
            (if outf (close outf))
            (respond o op (+ (erp postargs) args) cooks ip)))
      (let line nil
        (whilet c (and (> n 0) (readc i))
          (if srv-noisy* (pr c))
          (-- n)
          (push c line)) 
        (if srv-noisy* (pr "\n\n"))
        (respond o op (+ (parseargs (string (rev line))) args) cooks ip)))))
          
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
        
        
;(def testbuf ()
;  (with ( line line* 
;          buf nil 
;          n (len seperator*) 
;          match seperator* 
;          count 0 
;          state 'seperator)
;    (while line
;      (= c (pop line))
;      (w/appendfile af "out3" (w/stdout af (pr c)))
;      (push c buf)
;      (if (<= n (len buf))
;        (= buf (cut buf 0 n )))
;      ;(prn "s: " seperator*)
;      ;(prn "b: " buf)
;      (if (iso buf match)
;        (++ count)
;        (case state
;          'seperator (do
;            (= match (list #\C #\o #\n #\t #\e #\n #\t #\- #\D #\i #\s #\p #\o #\s #\i #\t #\i #\o #\n #\: #\space #\f #\o #\r #\m #\- #\d #\a #\t #\a #\;))
;            (= state 'content-disposition))
;          'content-disposition nil
;          ;(getfilename (whilet c (check (a (pop line)) (and (isnt a #\newline) a)))
;        ))) ;change state here: check file, filename before #\n, content-type before next #\n
;    (erp count)))
;    
;    
;    
