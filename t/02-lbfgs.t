(load "~/quicklisp/setup.lisp")
(require :asdf)
(let ((*standard-output* (make-broadcast-stream)))
  (asdf:load-system :clcrf))

(in-package :clcrf)

; A simple function to optimize: x^2 - 3x + y^2 - 4 y + 7
; Fits the required properties for L-BFGS, minimal value is 7, at (3, 4).
(defun gradient (args)
  (let ((x (aref args 0))
        (y (aref args 1)))
    (values (+ (* (- x 3) (- x 3)) (* (- y 4) (- y 4)) 7) ; f(x)
            (make-array 2 :initial-contents (list (- (* 2 x) 6) (- (* 2 y) 8)))))) ; g(x)

(test 1
  (lbfgs #'gradient :dimen 2)
  (ok t "stub"))

; vim: ts=2:sw=2:sts=2:syntax=lisp
