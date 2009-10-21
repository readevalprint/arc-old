(def upsrv () (thread (asv 37379)))

(load "erp.arc")
(load "parseform-2.arc")
(= maxpostsize* 102400000) ;5 megabytes
(= upload-dir* "/home/tim/sites/arc3.0/out/")
(= media-url* "/static/")

(defop upload req
  (pr "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"
\"http://www.w3.org/TR/html4/strict.dtd\">

<html><head><title>File uploading in Arc</title></head>
<body>
<p>give it your best shot! 10mb limit</p>
<h1>form with files</h1>
<form action=\"\" method=\"post\"
enctype=\"multipart/form-data\">
<label for=\"file1\">File 1:</label>
<input type=\"file\" name=\"file1\" id=\"file\" /><br/>
<label for=\"foo1\">Foo:</label>
<input name=\"foo1\" value=\"bar1\" /><br/>
<label for=\"foo2\">Foo2:</label>
<textarea name=\"foo2\" value=\"bar2\" >
</textarea><br/>

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
<br />
<a href=\"/static/out/\">see all uploaded files and cached form data</a>
</body>
</html>"))

(def links (req)
  (tostring
    (with (f1 (alref req!args "file1")
          f2 (alref req!args "file2"))
      (if f1 (pr "<a href=\"" media-url* (cadr f1)"\">" (car f1) "</a><br/>"))
      (if f2 (pr "<a href=\"" media-url* (cadr f2)"\">" (car f2) "</a>"))
    )))

(def handle-request-thread (i o ip)
  (with (nls 0 lines nil line nil responded nil t0 (msec))
    (after
      (whilet c (unless responded (readc i))
        (if srv-noisy* (pr c))
        (if (is c #\newline)
            (if (is (++ nls) 2) 
                (let (type op args n cooks seperator) (erp:parseheader (rev lines))
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
  (erp lines)
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
    (if (> n maxpostsize*)
      (respond-err o (string "Post size too large. Maximum is: " maxpostsize*))
      (if seperator
        (with (postargs nil out (string upload-dir* (rand-string 4)))
          (w/outfile outf out
            ; write the buffer to file
            (time:mz:write-bytes (read-bytes n i) outf))
          (= postargs (parseform out seperator))
          (respond o op (+ postargs args) cooks ip))
        (let line nil
          (whilet c (and (> n 0) (readc i))
            (if srv-noisy* (pr c))
            (-- n)
            (push c line))
          (if srv-noisy* (pr "\n\n"))
          (respond o op (+ (parseargs (string (rev line))) args) cooks ip))))))
        
; arc> (mz:regexp-match #rx"^news/?([0-9]*)/?([0-9]*)/?$" "news/33/44/")

(def urlmap (op)
  (erp op)
  (let op (string op)
  (if (erp:mz:regexp-match #rx"news/?([0-9]*)/?" op)
    news)))

(def respond (str op args cooks ip)
  (w/stdout str
    (iflet f (erp:urlmap op)
      (let req (inst 'request 'args args 'cooks cooks 'ip ip)
        (do (prn header*)
            (f str req)))
      (iflet f (srvops* op)
             (let req (inst 'request 'args args 'cooks cooks 'ip ip)
               (if (redirector* op)
                   (do (prn rdheader*)
                       (prn "Location: " (f str req))
                       (prn))
                   (do (prn header*)
                       (awhen (max-age* op)
                         (prn "Cache-Control: max-age=" it))
                       (f str req))))
             (let filetype (static-filetype op)
               (aif (and filetype (file-exists (string staticdir* op)))
                    (do (prn (type-header* filetype))
                        (awhen static-max-age*
                          (prn "Cache-Control: max-age=" it))
                        (prn)
                        (w/infile i it
                          (whilet b (readb i)
                            (writeb b str))))
                    (respond-err str unknown-msg*)))))))

          
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
        
