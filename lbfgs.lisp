(in-package :clcrf)

(defun lbfgs (gradient &key dimen (history 5) initial-x)
  (loop with x-history = (mk-circular history)
        with g-history = (mk-circular history)
        with rho       = (mk-circular history)
        with x = (if initial-x initial-x (make-array dimen :initial-element 1.0 :element-type 'single-float))
        with (y g) = (multiple-value-list (funcall gradient x))
        for k from 0
        for d = (prod-s-v -1.0 g) then (find-direction k x-history g g-history history rho)
        for (alpha x-prime g-prime) = (multiple-value-list (line-search gradient d x y g))
        do (setf (cref x-history k) (sub-v-v x-prime x)
                 (cref g-history k) (sub-v-v g-prime g)
                 (cref rho       k) (/ 1 (inner-product (cref g-history k) (cref x-history k)))
                 x x-prime
                 g g-prime)
           (format t "L-BFGS: alpha: ~a (~a); next x: ~a; next g: ~a~%" alpha d x-prime g-prime)))

(defun find-direction (k x-history g g-history history rhos)
  (loop with H0     = (unit (length g) :value (/ (inner-product (cref x-history (1- k)) (cref g-history (1- k)))
                                                 (inner-product (cref g-history (1- k)) (cref g-history (1- k)))))
        with alphas = (mk-circular history)
        with q      = (loop
                        with q    = g
                        for i     from (1- k) downto (max (- k history) 0)
                        for rho   = (/ 1 (inner-product (cref g-history i) (cref x-history i)))
                        for alpha = (* rho (inner-product (cref x-history i) q))
                        do (setf (cref rhos i)   rho
                                 (cref alphas i) alpha
                                 q               (sub-v-v q (prod-s-v alpha (cref g-history i))))
                        finally (return q))
        with r       = (prod-m-v H0 q)
        for  i       from (max (- k history) 0) below k
        for beta     = (* (cref rhos i) (inner-product (cref g-history i) r))
        do (setf r (add-v-v r (prod-s-v (- (cref alphas i) beta) (cref x-history i))))
        finally (return (prod-s-v -1.0 r))))

(defun line-search (gradient direction x y g)
  (loop with g-times-d = (inner-product g direction)
        for alpha = 1.0 then (* 0.1 alpha)
        for x-prime = (add-v-v x (prod-s-v alpha direction))
        for (y-prime g-prime) = (multiple-value-list (funcall gradient x-prime))
        for armijo = (+ y (* 1e-4 alpha g-times-d))
        do (format t "  Trying alpha=~a (x'=~a y'=~a g'=~a)~%" alpha x-prime y-prime g-prime)
        if (eql 0.0 alpha) do (error "alpha=0.0")
        until (<= y-prime armijo)
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

(defun unit (dimen &key (value 1.0))
  (loop with m = (make-array (list dimen dimen) :element-type 'single-float)
        for i below dimen
        do (setf (aref m i i) value)
        finally (return m)))

; vim: ts=2:sw=2:sts=2:syntax=lisp
