(defsystem :clcrf
  :description "A Common Lisp CRF toolkit"
  :version 1
  :author "Arne Skjærholt <arnsholt@gmail.com"
  :components ((:file "clcrf")
               (:file "crf" :depends-on ("clcrf" "quarks"))
               (:file "quarks" :depends-on ("clcrf"))
               (:file "lbfgs" :depends-on ("clcrf"))
               (:file "t/test" :depends-on ("crf")))
  :depends-on ("cl-ppcre"))

; vim: ts=2:sw=2:syntax=lisp
