(load "~/quicklisp/setup.lisp")
(require :asdf)
(let ((*standard-output* (make-broadcast-stream)))
  (asdf:load-system :clcrf))

(in-package :clcrf)

; In "Numerical optimization" (Springer, 2006), Nocedal and Wright suggest the
; Rosenbrock function as a test for an implementation of L-BFGS. Let's do
; that. The function and its derivatives are:
; f(x,y) = 100(y - x^2)^2 + (1-x)^2
; d/dx f = -(400 x(y-x^2) + 2(1-x))
; d/dy f = 200(y-x^2)
(defun rosenbrock (x)
  (let ((x (aref x 0))
        (y (aref x 1)))
    (values (+ (* 100 (sq (- y (sq x)))) (sq (- 1 x)))
            (make-array 2 :initial-contents (list (- (+ (* 400 x (- y (sq x))) (* 2 (- 1 x))))
                                                  (* 200 (- y (sq x))))))))

(defun sq (x)
  (* x x))

(test 1
  (lbfgs #'rosenbrock :dimen 2 :initial-x (make-array 2 :initial-contents '(-1.0 -1.0)))
  (ok t "stub"))

; vim: ts=2:sw=2:sts=2:ft=lisp
