(defsystem :clcrf
  :description "A Common Lisp CRF toolkit"
  :version 1
  :author "Arne Skjærholt <arnsholt@gmail.com"
  :components ((:file "clcrf")
               (:file "circular" :depends-on ("clcrf"))
               (:file "corpus" :depends-on ("clcrf"))
               (:file "crf" :depends-on ("clcrf" "quarks" "corpus"))
               (:file "quarks" :depends-on ("clcrf"))
               (:file "lbfgs" :depends-on ("clcrf" "circular"))
               (:file "t/test" :depends-on ("clcrf")))
  :depends-on ("cl-ppcre"))

; vim: ts=2:sw=2:sts=2:syntax=lisp
