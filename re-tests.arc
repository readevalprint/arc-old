;By Conrad Barski, Sep 2008
;Licensed under GPLv3

(load "re.arc")

(mac test (a b)
  (w/uniq ^
    `(let ,^ ,a
          (unless (iso ,^ ',b)
                  (prn "*fail* " (tostring:write ',a) "->" ,^)))))

(test (w/target "a"
                (re "(xy)*"))
      "")

(test (w/target "x" 
                (re "x"))
      "x")

(test (w/target "a" 
                (re "a*"))
      "")

(test (w/target "xa" 
                (re "x*a"))
      "xa")

(test (w/target "xy"
                (re "x")
                (re "y"))
      "y")

(test (w/target "x"
                (re "a"))
      nil)

(test (w/target "xa"
                (re "a"))
      "a")

(test (w/target "aaaax2ybbb" 
                (re "x.y"))
      "x2y")

(test (w/target "aaaax2ybbb"
                (re "x")
                (do1 (re ".")
                     (re "y")))
      "2")

(test (w/target "aaaax2zbbbb" 
                (re "x.y"))
      nil)

(test (w/target "aaaax2zbbbb"
                (re "x.y"))
      nil)

(test (w/target "aaaaaxybbbbb" 
                (re "xc*y"))
      "xy")

(test (w/target "aaaaaxcccybbbb" 
                (re "x")           
                (re "c*y"))
      "cccy")

(test (w/target "aaaaaxcdcybbbb" 
                (re "xc*y"))
      nil)

(test (w/target "aaaaaxybbbbb" 
                (re "x")
                (do1 (re "c*")
                     (re "y")))
      "")

(test (w/target "aaaxxxcccybbbbb" 
                (re "x")
                (do1 (re "c*")
                     (re "y")))
      "ccc")

(test (w/target "aaaaaxcgccybbbb"
                (re "x")
                (do1 (re "c*.c*")
                     (re "y")))
      "cgcc")

(test (w/target "a"
                (re "(xy)*"))
      "")

(test (w/target "xyb"
                (list (re "(xy)*")
                      (re "b")))
      ("xy" "b"))

(test (w/target "aaaxyxyb"
                (list (re "a")
                      (re "(xy)*")
                      (re "b")))
      ("a" "xyxy" "b"))

(test (w/target "aaaaxxkykyzzbbbbb"
                (list (re "a")
                      (re "(xx(ky)*zz)*")
                      (re "b")))
      ("a" "xxkykyzz" "b"))

(test (w/target "aaaaxxkykyzzbbbbb"
                (list (re "a")
                      (re "(xx(ky)*kyzz)*")
                      (re "b")))
      ("a" "xxkykyzz" "b"))

(test (w/target "aaaaaxcccybbbb" 
                (re "xc+y"))
      "xcccy")

(test (w/target "aaaaaxcdcybbbb" 
                (re "xc+y"))
      nil)

(test (w/target "aaaacccccbbbb" 
                (re "ac+"))
      "accccc")

(test (w/target "a" 
                (re "b|a"))
      "a")

(test (w/target "jax" 
                (re "b|ax|ay"))
      "ax")

(test (w/target "jay" 
                (re "b|ax|ay"))
      "ay")

(test (w/target "fau" 
                (re "f(a|b|c)u"))
      "fau")

(test (w/target "iiiiiifaxuiiiii" 
                (re "f(b|ax|ay)u"))
      "faxu")

(test (w/target "uuuuabcuuuu"
                (re "ab?c"))
      "abc")

(test (w/target "uuuuacuuuu"
                (re "ab?c"))
      "ac")

(test (w/target "uuuubuuuu"
                (re "[abc]"))
      "b")

(test (w/target "uuuubuuuu"
                (re "[a-c]"))
      "b")

(test (w/target "uuuubuuuu"
                (re "[^s-z]"))
      "b")

(test (w/target "saDwTzrtbds"
                (re "[^a-cA-Z][^a-cA-Z][^a-cA-Z]"))
      "zrt")

(test (w/target "axaaxxxxaaxxa"
                (re "ax{2,3}a"))
      "axxa")

(test (w/target "axaaxxxxaaxxabtbbttttbbttb"
                (re "ax{2,3}a")
                (do1 (re ".*")
                     (re "bt{2,3}b")))
      "btbbttttb")

(test (w/target "blue gry graey grey red"
                (re "gr[ae]y"))
      "grey")

(test (w/target "quack Iraq is a country"
                (re "q[^u]"))
      "q ")

(test (w/target "^^^f"
                (re "[\\^]*[^\\^]"))
      "^^^f")

(test (w/target "asdfasdf^^^foowqerasdf"
                (re "[\\^]*[^\\^].."))
      "^^^foo")

(test (w/target "adsfasdf345adsasdf"
                (re "\\d*a"))
      "345a")

(test (w/target "adsfasdf345adsasdf"
                (re "[\\d]+a"))
      "345a")

(test (w/target "dddfooeddddfooi"
                (re "foo.$"))
      "fooi")

(test (w/target "dddfooe\nddddfooi"
                (re "foo.$"))
      "fooe")

(test (w/target "dddfooeddd\nfooi"
                (re "^foo."))
      "fooi")

(test (w/target "fooeddd\\nfooi"
                (re "^foo."))
      "fooe")

(test (draintarget "iiiiiaXaiiiiiaYaiiii"
                   (re "a.a"))
      ("aXa" "aYa"))

(test (draintarget "foo\nbar\nbaz"
                   (re "^\\w+$"))
      ("foo" "bar" "baz"))

(test (draintarget "foo bar baz"
                   (re "^|\\s")
                   (re "\\w+"))
      ("foo" "bar" "baz"))

#| This example works, but sometimes gives the error "attempted to cross a continuation barrier"- I'm still investigating...
(def name ()
  (re "[a-z]+"))

(test (map tablist 
           (draintarget "asdweotwerw 2weroq bob.smith@google.com sdfaiosda,sdf weqop82uopdisj [op2';ksadj;ldfp8u2pj ldsajf9p2u8t;jsd;lfashdf924hr;owjsad;fj lisa.north@yahoo.com weqporiuasf90 0289u 9wefh 9psda'pwej2lh2t getsdj;sadf"
                        (w/table p
                                 (re " ")
                                 (= p!first (name))
                                 (re "\\.")
                                 (= p!last (name))
                                 (re "@")
                                 (= p!domain (name))
                                 (re "\\.")
                                 (= p!tld (name))
                                 (re " "))))
      (((tld "com") (domain "google") (last "smith") (first "bob")) ((tld "com") (domain "yahoo") (last "north") (first "lisa"))))
|#

