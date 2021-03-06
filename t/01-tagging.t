(load "~/quicklisp/setup.lisp")
(require :asdf)
(let ((*standard-output* (make-broadcast-stream)))
  (asdf:load-system :clcrf))

(in-package :clcrf)

;;; XXX: Epic hack to make sure floats are more or less equal.
(defun is-almost (x y)
    (> 1.0e-6 (abs (- 1(/ x y))))  )

(test 22
  (let* ((sequence (corpus-sentence (read-corpus "t/test.cor") 0))
         (crf (read-crf "t/test.crf"))
         (gold '(("-s---fb--i" 10.056463)
                 ("3spia----i" 12.653277)
                 ("-s---pn--i" 6.782944)
                 ("-srppfn--i" 8.601043)
                 ("---------n" 14.715457)
                 ("-p---fa--i" 10.164168)
                 ("-p---pa--i" 8.230162)
                 ("-p---fg--i" 10.751122)
                 ("-s---fa--i" 10.861322)
                 ("3ppia----i" 10.423610)
                 ("-p---mn--i" 9.964561)
                 ("-s---fa--i" 10.378350)
                 ("-p---mn--i" 7.388685)
                 ("-s---fa--i" 10.659187)
                 ("-p---mn--i" 9.633917)
                 ("3p---mg--i" 10.592282)
                 ("-s---fb--i" 8.422921)
                 ("-p---mn--i" 6.561709)
                 ("1s---fb--i" 10.083181)
                 ("-p---mn--i" 9.914193)
                 ("3ppip----i" 11.642814)))
         (tagged (decode-crf crf sequence)))
  (is (mapcar (lambda (tag) (quarks-to-string (crf-tagset crf) tag)) (mapcar #'first tagged))
              (mapcar #'first gold) "tagging consistent with wapiti" :test #'equalp)
  (mapcar (lambda (x y) (ok (is-almost (second x) (second y)) "potential consistent with wapiti")) tagged gold)))

; vim: ts=2:sw=2:sts=2:syntax=lisp
