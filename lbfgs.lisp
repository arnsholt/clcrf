(in-package :clcrf)

(defun lbfgs (gradient &key (owl nil) initial-x)
  )

(defun line-search ()
  )

; XXX: Will not complain if given vectors of different lengths.
(defun inner-product (a b)
  (loop for x across a
        for y across b
        summing (* x y)))

(defun outer-product (a b)
  (make-array (list (length a) (length b))
              :initial-contents (map 'list (lambda (x) (map 'list (lambda (y) (* x y)) b)) a)))

(defun transpose (m)
  (loop with rows = (array-dimension m 0)
        with columns = (array-dimension m 1)
        with transpose = (make-array (list columns rows))
        for row below rows
        do (loop
             for column below columns
             do (setf (aref transpose column row) (aref m row column)))
        finally (return transpose)))
