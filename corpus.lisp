(in-package :clcrf)

(defstruct corpus
  (sentences (make-array 100 :adjustable t :fill-pointer 0 :element-type 'sentence)))

(defstruct sentence
  (observations (make-array 10 :adjustable t :fill-pointer 0 :element-type 'observation)))

(defstruct observation
  observations
  tag)

(defun read-corpus (filename &key (tagged nil))
  (with-open-file (file filename :direction :input)
    (loop with corpus = (make-corpus)
          for sentence = (read-sentence file :tagged tagged)
          while sentence do (vector-push-extend sentence (corpus-sentences corpus))
          finally (return corpus))))

(defun corpus-sentence (corpus num)
  (aref (corpus-sentences corpus) num))

(defun sentence-as-list (sentence)
  (map 'list #'observation-observations (sentence-observations sentence)))

(defun read-sentence (file &key (tagged nil))
  (loop with got-data = nil
        with sentence = (make-sentence)
        for line = (cl-ppcre:regex-replace-all "\\A\\s+|\\s+\\z" (read-line file nil nil) "")
        if (and (not line) (not got-data)) return nil ; Don't loop eternally at EOF.
        while (or (not got-data) (< 0 (length line)))
        if (and (not got-data) (< 0 (length line))) do (setf got-data t)
        if got-data do (vector-push-extend (make-observation-from-line line :tagged tagged)
                                           (sentence-observations sentence))
        finally (return sentence)))

(defun make-observation-from-line (line &key (tagged nil))
  (let ((observation (make-observation))
        (observations (cl-ppcre:split "\\s+" line)))
    (setf (observation-observations observation)
          (if tagged (butlast observations) observations)
          (observation-tag observation)
          (if tagged (last observations) nil))
    observation))

; vim: ts=2:sw=2
