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
       (diag (format nil "Looks like you planned ~a tests but ran ~a." ,plan *count*)))))

(defmacro todo (reason &body tests)
  `(let ((*todo* ,reason))
     ,@tests))

(defun plan (count)
  (format t "1..~a~%" count))

(defun ok (result message)
  (if result
    (pass message)
    (fail message)))

(defun diag (message)
  (format t "# ~a~%" message))

(defun is (got expected message &key (test #'equal))
  (ok (funcall test got expected) message))

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
