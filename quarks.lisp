(in-package :clcrf)

;;; Quick and dirty hack for string <-> int conversion.
(defstruct quarks
  (size 0 (:type fixnum))
  (string-to-int (make-hash-table :test #'equal) (:type hash-table))
  (int-to-string (make-array 100 :adjustable 't :fill-pointer 0) (:type (simple-array string (*)))))

(declaim (ftype (function quarks fixnum) quarks-size))

(defun add-quark (quarks item)
  (incf (quarks-size quarks))
  (setf (gethash item (quarks-string-to-int quarks))
        (vector-push-extend item (quarks-int-to-string quarks))))

(defun insert-quark (quarks item)
  (or (quarks-to-int quarks item) (add-quark quarks item)))

(defun quarks-from-list (list)
  (let ((quarks (make-quarks)))
    (map nil (lambda (item) (add-quark quarks item)) list)
    quarks))

(defun quarks-to-int (quarks string)
  (gethash string (quarks-string-to-int quarks)))

(defun quarks-to-string (quarks int)
  (aref (quarks-int-to-string quarks) int))

; vim: ts=2:sw=2:sts=2
