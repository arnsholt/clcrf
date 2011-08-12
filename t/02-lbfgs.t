(load "~/quicklisp/setup.lisp")
(require :asdf)
(let ((*standard-output* (make-broadcast-stream)))
  (asdf:load-system :clcrf))

(in-package :clcrf)

(test 1
  (ok t "stub"))

; vim: ts=2:sw=2:sts=2:syntax=lisp
