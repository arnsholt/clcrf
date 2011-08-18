(in-package :clcrf)

(defun lbfgs (gradient &key dimen (history 5))
  (loop with H0 = (unit dimen)
        with x-history = (mk-circular history)
        with g-history = (mk-circular history)
        with x = (make-array dimen :initial-element 1.0 :element-type 'single-float)
        with (y g) = (multiple-value-list (funcall gradient x))
        for k from 0
        for d = (find-direction k H0 x-history g g-history history)
        for (alpha x-prime g-prime) = (multiple-value-list (line-search gradient d x y g))
        do (setf (cref x-history k) (sub-v-v x-prime x)
                 (cref g-history k) (sub-v-v g-prime g)
                 x x-prime
                 g g-prime)
           (format t "L-BFGS: alpha: ~a; next x: ~a; next g: ~a~%" alpha x-prime g-prime)))

(defun find-direction (k H0 x-history g g-history history)
  (let* ((incr  (if (<= k history) 0 (- k history)))
         (bound (if (<= k history) k history))
         (q (make-array (1+ bound)))
         (alpha (make-array bound)))
    (loop initially (setf (aref q bound) g)
          for i from (1- bound) downto 0
          for j   = (+ i incr)
          for rho = (/ 1 (inner-product (cref g-history j) (cref x-history j)))
          for a   = (* rho (inner-product (cref x-history j) (aref q (1+ i))))
          do (setf (aref alpha i) a
                   (aref q     i) (sub-v-v (aref q (1+ i)) (prod-s-v a (cref g-history j)))))
    (loop with r = (make-array (1+ bound))
          initially (setf (aref r 0) (prod-m-v H0 (aref q 0)))
          for i below bound
          for j    = (+ i incr)
          for rho  = (/ 1 (inner-product (cref g-history j) (cref x-history j)))
          for beta = (* rho (inner-product (cref g-history j) (aref r i)))
          do (setf (aref r (1+ i)) (add-v-v (aref r i) (prod-s-v (- (aref alpha i) beta) (cref x-history j))))
          finally (return (prod-s-v -1.0 (aref r bound))))))

(defun line-search (gradient direction x y g)
  (loop with g-times-d = (inner-product g direction)
        for alpha = 1 then (* 0.1 alpha)
        for x-prime = (add-v-v x (prod-s-v alpha direction))
        for (y-prime g-prime) = (multiple-value-list (funcall gradient x-prime))
        while (not (and (<= y-prime (+ y (* 1e-4 alpha g-times-d)))
                        (>= (inner-product g-prime direction) (* 0.9 g-times-d))))
        finally (return (values alpha x-prime g-prime))))

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

; XXX: Will not complain if given vectors of different lengths.
(defun add-v-v (a b)
  (map 'vector (lambda (x y) (+ x y)) a b))

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
