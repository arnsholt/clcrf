(in-package :clcrf)

(defstruct circular
  buffer
  size)

(defun mk-circular (size)
  (make-circular :size size :buffer (make-array size)))

(defun cref (circular index)
  (aref (circular-buffer circular) (rem index (circular-size circular))))

(defun cref-set (circular index value)
  (setf (aref (circular-buffer circular) (rem index (circular-size circular))) value))

(defsetf cref cref-set)

; vim: ts=2:sw=2:sts=2:syntax=lisp
