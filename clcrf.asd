(defsystem :clcrf
  :description "A Common Lisp CRF toolkit"
  :version 1
  :author "Arne Skjærholt <arnsholt@gmail.com"
  :components ((:file "crf"))
  :depends-on ("cl-ppcre"))
