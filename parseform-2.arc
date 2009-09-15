(load "erp.arc")
(= upload-dir* "/home/tim/sites/arc3.0/out/")

(def systr body (tostring:system:string body))

(def parseform (raw sep)
  (withs (seplines (systr "grep -na ^`head -n 1 \""raw"\" | head -c -2` \""raw"\"")
          seplist (map [tokens _ #\:] (tokens seplines)))
      (getformdata raw seplist)))

(def getformdata (raw seplist) 
  (withs (start (+ 1 (coerce (caar seplist) 'int ))
          end (- (coerce (car:cadr seplist) 'int ) 1)
          filename (form-file-name raw start)
          inputname (form-input-name raw start))
    (if filename
      (cons (list inputname (list filename (form-file-contents raw start end filename)) )
            (if (cddr seplist)(getformdata raw (cdr seplist))))
      (cons (list inputname (form-input-contents raw start end)) 
            (if (cddr seplist)(getformdata raw (cdr seplist)))))))

(def form-input-contents (raw start end)
  (systr "sed -n "(+ 2 start)","end"p \""raw"\" | head -c -2"))

(def form-file-contents (raw start end filename)
  (let out (string upload-dir* (rand-string 10) "_" filename)
    (systr "sed -n "(+ 3 start)","end"p \""raw"\" | head -c -2 >> \""out"\"")
    out))

(def form-input-name (raw start)
  (withs (line (systr "sed -n "start"p \""raw"\""))
    (iflet p (posmatch "name=\"" line)
      (cut line (+ p 6) (posmatch "\"" line (+ p 6)))))) ; 10 for len of filename="

(def form-file-name (raw start)
  "file name or nil"
  (withs (line (systr "sed -n "start"p \""raw"\""))
    (iflet p (posmatch "filename=\"" line)
      (cut line (+ p 10) (posmatch "\"" line (+ p 10)))))) ; 10 for len of filename="
 
