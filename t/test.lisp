(in-package :clcrf)
(defvar *count*)
(defvar *todo*)

(defmacro test (plan &body tests)
  `(let ((*count* 0)
         (*todo* nil))
     ,(if plan
        `(plan ,plan))
     ,@tests
     (if (> *count* ,plan)
       (diag "Looks like you planned ~a tests but ran ~a." ,plan *count*))))

(defmacro todo (reason &body tests)
  `(let ((*todo* ,reason))
     ,@tests))

(defun plan (count)
  (format t "1..~a~%" count))

(defun ok (result message)
  (if result
    (pass message)
    (fail message)))

(defun diag (message &rest data)
  ;(format t "# ~a~%" message))
  (format t "# ")
  (apply #'format t message data)
  (format t "~%"))

(defun is (got expected message &key (test #'equal))
  (let ((result (funcall test got expected)))
    (ok result message)
    (if (not result) (diag "Got: ~a, expected ~a" got expected))))

;;; XXX: Epic hack to make sure floats are more or less equal.
(defun is-almost (x y &optional message)
    (let* ((ratio (abs (- 1 (/ x y))))
           (result (> 1.0e-6 ratio)))
      (ok result message)
      (if (not result) (diag "abs(1 - ~a/~a) = ~a > 1.0e-6" x y ratio))))

(defun pass (message)
  (format t "ok ~a" (incf *count*))
  (if message (format t " - ~a" message))
  (if *todo* (format t " # TODO ~a" *todo*))
  (format t "~%"))

(defun fail (message)
  (format t "not ok ~a" (incf *count*))
  (if message (format t " - ~a" message))
  (if *todo* (format t " # TODO ~a" *todo*))
  (format t "~%"))
