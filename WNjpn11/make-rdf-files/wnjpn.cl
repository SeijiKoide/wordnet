
(in-package :wnjpn)

(eval-when (:load-toplevel :execute)
  (defparameter *wordnet-jpn-directory*
    (merge-pathnames "wnjpn.db" (user-homedir-pathname)))
  ) ; end of eval-when

(defparameter *dbfile* "wnjpn.db")

(defvar *db*)


(defmacro with-open-jpn (&rest body)
  `(with-open-database (*db* (merge-pathnames *dbfile* *wordnet-jpn-directory*))
     ,@body))

(defun synset (offset pos)
  (format nil "~8,'0D-~A" offset (ecase pos
                                   (:noun "n")
                                   (:verb "v")
                                   (:adjective "a")
                                   (:adjectivesatellite "a")
                                   (:adverb "r"))))

(defun offset (synsetid)
  (let ((offset&p (wn:split-seq-using synsetid #\-)))
    (let ((offsetstr (first offset&p))
          (p (second offset&p)))
      (let ((pos (cond ((string= p "n") :noun)
                       ((string= p "v") :verb)
                       ((string= p "a") :adjective)
                       ((string= p "r") :adverb))))
        (values (parse-integer offsetstr) pos)))))

(defun get-jpn-synonyms (offset pos)
  "returns a list of synonymous lemmas for synsetid from offset and pos."
  (loop for (wid) in (execute-to-list *db* "select wordid from sense where (lang = \"jpn\") and (synset = ?)" (synset offset pos))
      collect (execute-single *db* "select lemma from word where wordid = ?" (format nil "~D" wid))))

#|
(with-open-jpn
     (print *db*))
(with-open-jpn
    (execute-to-list *db* "select wordid from sense where (lang = \"jpn\") and (synset = ?)" "02787772-n"))
(with-open-jpn
    (execute-to-list *db* "select wordid from sense where (lang = \"jpn\") and (synset = ?)" "00006269-n"))
(with-open-jpn
    (get-jpn-synonyms 2787772 :noun))
(with-open-jpn
    (get-jpn-synonyms 6269 :noun))
|#

(defun get-jpn-gloss (offset pos)
  "<pos> may be a ss_type."
  (let* ((gloss-definition
          (execute-single *db* "select def from synset_def where lang = \"jpn\" and synset = ?" (synset offset pos)))
         (gloss-examples
          (wn::squash 
           (execute-to-list *db* "select def from synset_ex where lang = \"jpn\" and synset = ?" (synset offset pos)))))
    (if (equal gloss-examples '(nil))
        (format nil "~A" gloss-definition)
      (format nil "~A~{; ~A~}" gloss-definition gloss-examples)
      )))

#|
(in-package :wnjpn)
(with-open-jpn 
  (synsets-in-jpndb "ɏ" :noun))
|#

(defun synsets-in-jpndb (jpn-word pos)
  "returns a list of sysnset-ids related to <jpn-word> and <pos>."
  (let* ((wordid (execute-single *db*
                                 "select wordid from word where (lang = \"jpn\") and (lemma = ?) and (pos = ?)"
                                 jpn-word
                                 (ecase pos
                                   (:noun "n")
                                   (:verb "v")
                                   (:adjective "a")
                                   (:adjectivesatellite "a")
                                   (:adverb "r"))))
         (synsets (execute-to-list *db* "select synset from sense where wordid = ?" (format nil "~D" wordid)))
         (synsets2 (loop for synset in synsets do (assert (= 1 (length synset))) collect (car synset))))
    synsets2))

(defun get-jpn-synsetid (offset jpn-word pos)
  "returns a synsetnumber for <offset> w.r.t <jpn-word>."
  (let* ((wordid (execute-single *db*
                                 "select wordid from word where (lang = \"jpn\") and (lemma = ?) and (pos = ?)"
                                 jpn-word
                                 (ecase pos
                                   (:noun "n")
                                   (:verb "v")
                                   (:adjective "a")
                                   (:adjectivesatellite "a")
                                   (:adverb "r"))))
         (synsets (execute-to-list *db* "select synset from sense where wordid = ?" (format nil "~D" wordid)))
         (synsets2 (loop for synset in synsets do (assert (= 1 (length synset))) collect (car synset))))
    (1+ (position (synset offset pos) synsets2 :test #'string=))))
#|
(in-package :wnjpn)
(with-open-jpn
    (get-jpn-synsetid 6269 "жը" :noun))
(with-open-jpn
    (execute-to-list *db* "select wordid from word where lemma = ?" "min"))
00001740-v
|#

#|
(with-open-jpn
    (get-jpn-gloss 2787772 :noun))
(with-open-jpn
    (get-jpn-gloss 09213565 :noun))
(with-open-jpn
    (get-jpn-gloss 00001740 :noun))
|#

#|
(in-package :wnjpn)
(with-open-jpn
    (execute-to-list *db* "select wordid from word where (lang = \"jpn\") and (pos = \"n\") and lemma = ?" "©©"))
|#
(defun search-word-of-WNjpn-noun (word)
  (let ((id-list 
         (with-open-jpn
             (execute-to-list *db* "select wordid from word where (lang = \"jpn\") and (pos = \"n\") and lemma = ?" (string word)))))
    (when id-list (wn::squash id-list))))
