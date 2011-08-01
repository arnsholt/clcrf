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

(defun quarks-to-int (quarks string)
  (gethash string (quarks-string-to-int quarks)))

(defun quarks-to-string (quarks int)
  (aref (quarks-int-to-string quarks) int))

(defstruct crf
  templates
  tagset
  observations
  offsets
  weights)

(defmethod print-object ((crf crf) stream)
  (format stream "<CRF: ~a templates, ~a tags, ~a observations>"
          (length (crf-templates crf))
          (quarks-size (crf-tagset crf))
          (quarks-size (crf-observations crf))))

; TODO: Compute feature offsets for all observations.
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

(defun compute-offsets (crf)
  (let* ((obs-count (quarks-size (crf-observations crf)))
         (offsets   (make-array obs-count))
         (offset    0)
         (Y         (quarks-size (crf-tagset crf))))
    (loop for i below obs-count
          for observation = (quarks-to-string (crf-observations crf) i)
          if (equal "u" (subseq observation 0 1)) do (setf (aref offsets i) offset)
                                                     (incf offset Y)
          if (equal "b" (subseq observation 0 1)) do (setf (aref offsets i) offset)
                                                     (incf offset (* Y Y))
          if (equal "*" (subseq observation 0 1)) do (setf (aref offsets i) offset)
                                                     (incf offset (+ Y (* Y Y))))
    (setf (crf-offsets crf) offsets)))

(defun compile-templates (templates)
  (mapcar #'compile-template (fix-templates templates)))

; TODO: Handle * templates like wapiti.
(defun fix-templates (templates)
  (mapcan (lambda (template) (let ((first (string-downcase (subseq template 0 1))))
                               (cond ((or (equal first "u") (equal first "b")) (list template))
                                     ((equal first "*") (list
                                                          (concatenate 'string "u" (subseq template 1))
                                                          (concatenate 'string "b" (subseq template 1))))
                                     (t (error (format nil "Bad template type ~a" first))))))
          templates))

; TODO: Handle casefolding for %X[row,col].
; TODO: Handle absolute row offsets.
; TODO: Handle %t and %m. What does %m insert when there is no match?
(defun compile-template (template &key (compile-p t))
  (loop
    with start = 0
     and format = ""
     and arguments = ()
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
            (return (let ((lambda
                            `(lambda (sequence position)
                               (declare (ignorable sequence)
                                        (ignorable position))
                               (format nil ,format ,@(reverse arguments)))))
                      (if compile-p
                        (compile nil lambda)
                        lambda)))))

(defun get-relative (row-offset column sequence position)
  (let ((row (+ position row-offset))
        (length (length sequence)))
    (cond ((>= row length) (format nil "_X+~a" (- (1+ row) length)))
          ((<  row 0)      (format nil "_X~a" row))
          (t (elt (elt sequence row) column)))))

(defun apply-templates (crf input)
  (loop for position from 0
        for row in input ; Not read. Used to only iterate over valid indices into input.
        collect (mapcar (lambda (template) (funcall template input position)) (crf-templates crf))))

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
  (loop with input = (apply-templates crf input)
        with psi   = (psi crf input)
        with Y     = (quarks-size (crf-tagset crf))
        with L     = (length input)
        with back  = (make-array (list L Y))
        with prev  = (make-array Y :initial-element 0)
        with cur   = (make-array Y :initial-element 0)
        initially (loop for q below Y do (setf (aref cur q) (aref psi 0 0 q)))
        for i from 1 below L
        ; Swap prev. and cur:
        do (psetf prev cur cur prev)
        ; Clear new cur. vector:
        do (loop for i from 0 below Y do (setf (aref cur i) 0))
        do (loop
             for q from 0 below Y
             do (loop
                  for q-prime from 0 below Y
                  for potential = (+ (aref psi i q-prime q) (aref prev q-prime))
                  if (> potential (aref cur q)) do (setf (aref cur q) potential (aref back i q) q-prime)))
        finally (return (decode-backtrace back psi (argmax (lambda (x) x) cur) (1- L)))))

(defun decode-backtrace (back psi q i)
  (let ((q-prime (aref back i q)))
    (if (> i 1)
      (append (decode-backtrace back psi q-prime (1- i)) (list (list q (aref psi i q-prime q))))
      (list (list q (aref psi 1 q-prime q)) (list q-prime (aref psi 0 0 q-prime))))))

(defun argmax (lambda list)
  (loop with best-val = nil
        with best-idx = nil
        for i from 0
        for item across list
        for value = (funcall lambda (elt list i))
        if (or (not best-idx) (> value best-val)) do (setf best-val value best-idx i)
        finally (return (values best-idx best-val))))

; TODO: Handle * observations.
(defun unigram-potential (crf observation q)
  (if (and (equal "u" (subseq observation 0 1)) (quarks-to-int (crf-observations crf) observation))
    (let ((base-offset (aref (crf-offsets crf) (quarks-to-int (crf-observations crf) observation))))
      (gethash (+ base-offset q) (crf-weights crf) 0))
    0))

; TODO: Handle * observations.
(defun bigram-potential (crf observation q-prime q)
  (if (and (equal "b" (subseq observation 0 1)) (quarks-to-int (crf-observations crf) observation))
    (let ((base-offset (aref (crf-offsets crf) (quarks-to-int (crf-observations crf) observation)))
          (bigram-offset (+ (* q-prime (quarks-size (crf-tagset crf))) q)))
      (gethash (+ base-offset bigram-offset) (crf-weights crf) 0))
    0))

(defun annotate-tags (crf psi tags)
  (loop with quarks = (mapcar (lambda (x) (quarks-to-int (crf-tagset crf) x)) tags)
        with prev = 0
        for cur in quarks
        for i from 0
        for tag in tags
        collect (list tag (aref psi i prev cur))))

(defun psi (crf input)
  (let* ((L (length input))
         (Y (quarks-size (crf-tagset crf)))
         (psi (make-array (list L Y Y) :initial-element 0)))
    (loop for i below L
          do (loop
            for q below Y
            for potential = (reduce #'+ (mapcar (lambda (observation)
                                                  (unigram-potential crf observation q))
                                                (elt input i)))
            do (loop for q-prime below Y do (incf (aref psi i q-prime q) potential))))
    (loop for i from 1 below L
          do (loop
               for q below Y
               do (loop
                    for q-prime below Y
                    for potential = (reduce #'+ (mapcar (lambda (observation)
                                                               (bigram-potential crf observation q-prime q))
                                                             (elt input i)))
                    do (incf (aref psi i q-prime q) potential)))
          finally (return psi))))

(defun read-corpus (filename)
  (with-open-file (file filename :direction :input)
    (loop for sentence = (read-sentence file)
          while sentence collect sentence)))

(defun read-sentence (file)
  (loop with got-data = nil
        for line = (cl-ppcre:regex-replace-all "\\A\\s+|\\s+\\z" (read-line file nil nil) "")
        if (and (not line) (not got-data)) return nil ; Don't loop eternally at EOF.
        while (or (not got-data) (< 0 (length line)))
        if (and (not got-data) (< 0 (length line))) do (setf got-data t)
        if got-data collect (cl-ppcre:split "\\s+" line)))

; vim: ts=2:sw=2
