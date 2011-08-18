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

(defun corpus-stats (corpus crf)
  (loop with total-uni = 0
        with total-bi  = 0
        with sentences = 0
        for sentence across (corpus-sentences corpus)
        for as-list  = (apply-templates crf (sentence-as-list sentence))
        for unigrams = (uniques (unigram-observations crf as-list))
        for bigrams  = (uniques (bigram-observations crf as-list))
        do (incf total-uni (length unigrams))
           (incf total-bi  (length bigrams))
           (incf sentences)
        finally (format t "~a sentences, ~,2f unigrams (~,2f features) and ~,2f bigrams per sentence."
                        sentences (/ total-uni sentences) (* (/ total-uni sentences) (quarks-size (crf-tagset crf)))
                        (/ total-bi sentences))))

(defun uniques (seq)
  (loop with max = -1
        for idx in (sort (mapcan (lambda (x) x) seq) #'<)
        if (> idx max) do (setf max idx)
                      and collect idx))

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

; vim: ts=2:sw=2:sts=2
