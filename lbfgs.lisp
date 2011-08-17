(in-package :clcrf)

(defun lbfgs (gradient &key dimen (history 5))
  (loop with H0 = (unit dimen)
        with x-history = (mk-circular history)
        with g-history = (mk-circular history)
        with x = (make-array dimen :initial-element 1.0 :element-type 'single-float)
        while nil
        for k from 0
        for (y g) = (multiple-value-list (funcall gradient x))
        for d = (find-direction k H0 x-history g g-history history)
        for alpha = (line-search) ; TODO
        ))

(defun find-direction (k H0 x-history g g-history history)
  (loop with alpha = (make-array history)
        with incr  = (if (<= k history) 0 (- k history))
        with bound = (if (<= k history) k history)
        for q = (loop
                  for i from (1- bound) downto 0
                  for j   = (+ i incr)
                  for q   = g then (sub-v-v q (prod-s-v alpha (cref g-history j)))
                  for rho = (/ 1 (inner-product (cref g-history j) (cref x-history j)))
                  for a   = (* rho (inner-product (cref x-history j) q))
                  do (setf (aref alpha i) a)
                  finally (return q))
        for i below history
        for j    = (+ i incr)
        for r    = (prod-m-v H0 q) then (add-m-m r (outer-product (cref g-history j) (sub-v-v (aref alpha i) beta)))
        for rho  = (/ 1 (inner-product (cref g-history j) (cref x-history j)))
        for beta = (* rho (inner-product (cref g-history j) r))
            ))

; XXX: Will not complain if given vectors of different lengths.
(defun inner-product (a b)
  (loop for x across a
        for y across b
        summing (* x y)))

(defun outer-product (a b)
  (make-array (list (length a) (length b)) :element-type 'single-float
              :initial-contents (map 'list (lambda (x) (map 'list (lambda (y) (* x y)) b)) a)))

(defun transpose (m)
  (loop with rows = (array-dimension m 0)
        with columns = (array-dimension m 1)
        with transpose = (make-array (list columns rows) :element-type 'single-float)
        for row below rows
        do (loop
             for column below columns
             do (setf (aref transpose column row) (aref m row column)))
        finally (return transpose)))

; XXX: Will not complain if the matrices are incompatible.
(defun add-m-m (a b)
  (loop with sum = (make-array (array-dimensions a) :element-type 'single-float)
        for i below (array-dimension a 0)
        do (loop
             for j below (array-dimension a 1)
             do (setf (aref sum i j) (+ (aref a i j) (aref b i j))))
        finally (return sum)))

; XXX: Will not complain if given vectors of different lengths.
(defun sub-v-v (a b)
  (map 'vector (lambda (x y) (- x y)) a b))

; XXX: Will not complain if given vectors of different lengths.
(defun prod-s-v (s v)
  (map 'vector (lambda (x) (* s x)) v))

; XXX: Will not complain if the matrices are incompatible.
(defun prod-m-m (a b)
  (loop with rows    = (array-dimension a 0) ; No. of rows in result
        with columns = (array-dimension b 1) ; No. of cols in result
        with inner   = (array-dimension a 1) ; Dimension of "inner" edges of the matrices
        with product = (make-array (list rows columns) :element-type 'single-float)
        for row below rows
        do (loop
             for column below columns
             for value = (loop
                           for i below inner
                           sum (* (aref a row i) (aref b i column)))
             do (setf (aref product row column) value))
        finally (return product)))

; XXX: Will not complain if the matrix and vector are incompatible.
(defun prod-m-v (m v)
  (loop with length  = (array-dimension m 0)
        with inner   = (array-dimension m 1)
        with product = (make-array length :element-type 'single-float)
        for i below length
        for value = (loop
                      for j below inner
                      sum (* (aref m i j) (aref v j)))
        do (setf (aref product i) value)
        finally (return product)))

(defun unit (dimen)
  (loop with m = (make-array (list dimen dimen) :element-type 'single-float)
        for i below dimen
        do (setf (aref m i i) 1.0)
        finally (return m)))

; vim: ts=2:sw=2:sts=2:syntax=lisp
