(defsystem :clcrf
  :description "A Common Lisp CRF toolkit"
  :version 1
  :author "Arne Skjærholt <arnsholt@gmail.com"
  :components ((:file "crf")
               (:file "t/test" :depends-on ("crf")))
  :depends-on ("cl-ppcre"))
