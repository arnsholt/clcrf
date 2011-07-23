(in-package :cl-user)
(defpackage :clcrf (:use "COMMON-LISP"))

(in-package :clcrf)

;;; Quick and dirty hack for string <-> int conversion.
(defstruct quarks
  (size 0)
  (string-to-int (make-hash-table :test #'equal))
  (int-to-string (make-array 100 :adjustable 't :fill-pointer 0)))

(defun add-quark (quarks item)
  (incf (quarks-size quarks))
  (setf (gethash item (quarks-string-to-int quarks))
        (vector-push-extend item (quarks-int-to-string quarks))))

(defun quarks-from-list (list)
  (let ((quarks (make-quarks)))
    (map nil (lambda (item) (add-quark quarks item)) list)
    quarks))

(defstruct crf
  templates
  tagset
  observations
  weights)

(defmethod print-object ((crf crf) stream)
  (format stream "<CRF: ~a templates, ~a tags, ~a observations>"
          (length (crf-templates crf))
          (quarks-size (crf-tagset crf))
          (quarks-size (crf-observations crf))))

(defun read-crf (filename)
  (with-open-file (file filename :direction :input)
    (let ((templates (read file))
          (tagset (read file))
          (observations (read file))
          (weights (read file)))
      (make-crf :templates (compile-templates templates)
                :tagset (quarks-from-list tagset)
                :observations (quarks-from-list observations)
                :weights (munge-weights weights)))))

(defun compile-templates (templates)
  (map 'vector #'compile-template templates))

(defun compile-template (template)
  (loop
    with start = 0
     and format = ""
     and arguments = ()
     and end = nil
    for (match-start match-end reg-start reg-end) = (multiple-value-list
                                                      (cl-ppcre:scan "%x\\[([+-]?\\d+),([+-]?\\d+)\\]" template :start start))
    while match-start
    do (setf format (concatenate 'string format (subseq template start match-start) "~a"))
       (setf start match-end)
       (let ((first-start  (aref reg-start 0))
             (first-end    (aref reg-end   0))
             (second-start (aref reg-start 1))
             (second-end   (aref reg-end   1)))
         (setf arguments (cons (list 'get-relative (parse-integer template :start first-start  :end first-end)
                                                   (parse-integer template :start second-start :end second-end)
                                                   'sequence 'position)
                             arguments)))
    finally (setf format (concatenate 'string format (subseq template start)))
            ;(return `(lambda (sequence position) (format nil ,format ,@(reverse arguments)))))) ; For debugging.
            (return (compile nil
                      `(lambda (sequence position) (format nil ,format ,@(reverse arguments)))))))

(defun get-relative (row-offset column sequence position)
  (let ((row (+ position row-offset))
        (length (length sequence)))
    (cond ((>= row length) (format nil "_X+~a" (- (1+ row) length)))
          ((<  row 0)      (format nil "_X~a" row))
          (t (elt (elt sequence row) column)))))
  ;(elt (elt sequence (+ position row-offset)) column))

(defun munge-weights (weights)
  (let ((hash (make-hash-table :test #'eql)))
    (map nil (lambda (tuple) (setf (gethash (first tuple) hash) (munge-weight (rest tuple)))) weights)
    hash))

(defun munge-weight (tuple)
  (let ((sign (cond ((equal "-" (first tuple)) -1)
                    ((equal "" (first tuple))   1)
                    (t (error "Bad sign."))))
        (mantissa (read-string-hex  (second tuple)))
        (exponent (read-from-string (third  tuple))))
    (* (scale-float mantissa exponent) sign)))

;;; Read a hexadecimal floating point number from a string. The number is
;;; assumed to be on the format 1.xxxxxxx and only the fractional part is
;;; actually parsed.
(defun read-string-hex (string)
  (loop with mantissa = 1
        for i from 1 to (- (length string) 2)
        for numerator = (parse-integer string :start (+ i 1) :end (+ i 2) :radix 16)
        for denominator = (expt 16 i)
        do (incf mantissa (/ numerator denominator))
        finally (return (float mantissa))))

(defun decode-crf (crf input)
  ; TODO: Apply patterns to input to get observations.
  (loop with tags = (make-array (length input))
        with prev = (make-array (quarks-size (crf-tagset crf)) :initial-element 0)
        with cur  = (make-array (quarks-size (crf-tagset crf)) :initial-element 0)
        for i from 0 to (1- (length input)))
  )
