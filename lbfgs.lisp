(in-package :clcrf)

(defun lbfgs (gradient &key dimen)
  (loop with H0 = (unit dimen)
        while nil))

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
