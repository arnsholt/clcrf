(load "~/quicklisp/setup.lisp")
(require :asdf)
(let ((*standard-output* (make-broadcast-stream)))
  (asdf:load-system :clcrf))

(in-package :clcrf)

(test 3
  (let ((template (compile-template "u:%x[-1,0]/%x[+1,2]"))
        (sequence '(("a1" "b1" "c1") ("a2" "b2" "c2") ("a3" "b3" "c3"))))
    (is (funcall template sequence 1) "u:a1/c3"   "simple template application")
    (is (funcall template sequence 0) "u:_X-1/c2" "template application underflow")
    (is (funcall template sequence 2) "u:a2/_X+1" "template application overflow")))

; vim: ts=2:sw=2:syntax=lisp
