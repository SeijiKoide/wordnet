;;;-*- Mode: common-lisp; syntax: common-lisp; package: wn; base: 10 -*-
;;;
;;;; WordNet Dictionary Information Retrieval and OWL Conversion Program
;;; This program is applied to Allegro8.1, 8.2, 9.0 Modern, sbcl, and WordNet.
;;;
;;; WordNet Schema is based on W3C Working Draft 19 June 2006
;;; See http://www.w3.org/TR/2006/WD-wordnet-rdf-20060619/
;;;
;; History
;; -------
;; 2011.10.21    Independent parts of WordNet.cl from SWCLOS is extracted here.
;; 2009.05.04    WordNet common routines are separated to here from WordNet30
;; 2009.05.03    Revised for SWCLOS2 and undated.
;; 2005.01.21    File obtained from Ruml's web site.
;;; ==================================================================================

;; This program does not require SWCLOS.

;;;
;;;
;;;

(in-package :wn)

;;; For example, word "mouse" has been linked as follows.
;;; ----------------------------------------------------------------------------------
;;; word-mouse -+- wordsense-mouse-noun-1 --------- synset-mouse-noun-1
;;;             |
;;;             |      wordsense-computer_mouse-noun-1 -+
;;;             |                                       |
;;;             +- wordsense-mouse-noun-2 --------- synset-mouse-noun-2
;;;             +- wordsense-mouse-noun-3 --------- synset-mouse-noun-3
;;;             +- wordsense-mouse-noun-4 --------- synset-mouse-noun-4
;;;             |
;;;             |          wordsense-sneak-verb-1 -+   other senses +
;;;             |                                  |                |
;;;             +- wordsense-mouse-verb-1 -------- synset-sneak-verb-1
;;;             +- wordsense-mouse-verb-2 ---------synset-mouse-verb-2
;;; ----------------------------------------------------------------------------------

;;;
;;;; Get Word
;;;
;;; The purpose of this function is getting word definition from a lexical form rather than 
;;; synset ID. 

(defun get-data-entry (offset pos)
  (with-open-file (stream (pos-data-file pos) :direction :input)
    (file-position stream offset)        ; offset is a file offset in data file.
    (let ((line (read-line stream t)))   ; then just read one line for this synset.
      (destructuring-bind
          (this-offset . tokens) (split-seq-using line)
        (assert (= (parse-integer this-offset) offset) ()
                "offset in data file not file position")
        (let ((t-pos (ss_type-from-symbol (second tokens))))
          (assert (or (eq t-pos pos)
                      (and (eq t-pos :adjectivesatellite)
                           (eq pos :adjective))) ()
                  "bad synset pos in: ~S" tokens))
        (list offset pos tokens)))))

(defun get-index-entry (key pos)
  "returns the entry that starts from synset_cnt for <key> in index file of <pos>.
   Note that <key> must be lower case and underscored for collocation." 
  (with-open-file (stream (pos-index-file pos) :direction :input)
    (%get-index-entry stream key pos)))

(defun %get-index-entry (stream key pos)
  (declare (optimize (speed 3) (safety 1)))
  (let ((pos-position (1+ (length key)))
        (entry (binary-search-lines stream (concatenate 'cl:string key " "))))
    (when entry
      (assert (or (eq pos
                      (ss_type-from-symbol (subseq entry pos-position (1+ pos-position))))
                  (and (eq pos :adjectivesatellite)
                       (string= "a" (subseq entry pos-position (1+ pos-position))))))
      (subseq entry (+ pos-position 2)))))

;;;
;;;; Load Synset
;;;

(defun offsets-in-index-entry (key pos)
  "returns synset offsets of <key> for <pos>.
   <key> must be downcased, and collocated by underscore if it is a collocation."
  (with-open-file (index-stream (pos-index-file pos) :direction :input)
    (let ((index-tokens (%get-index-entry index-stream key pos)))
      (when index-tokens
        (destructuring-bind
            (sense-count num-pointers . tokens) (split-seq-using index-tokens)
          (declare (ignore sense-count))
          (with-list-split-after (parse-integer num-pointers)
            (pointers tokens) tokens
            (declare (ignore pointers))
            (destructuring-bind (num-synsets TAGSENSE-CNT . synsets) tokens
              (declare (ignore num-synsets TAGSENSE-CNT))
              synsets)))))))

(defun get-synset-number-in-index-entry (key offset pos)
  "returns a synset number of <offset> in index entry for <key> in <pos>."
  (let ((synsets (offsets-in-index-entry key pos)))
    (1+ (position offset synsets :key #'parse-integer))))

(defun get-synonymous-words-from (offset pos)
  (setq offset (parse-integer offset))
  (let ((tokens (third (get-data-entry offset pos))))
    (destructuring-bind (file-num p1 num-senses . senses-and-more) tokens
      (declare (ignore p1 file-num))
      (setq num-senses (parse-integer num-senses :radix 16))
      (with-list-split-after (* 2 num-senses) (sense-stuff pointers-and-more) senses-and-more
        (declare (ignore pointers-and-more))
        (loop for (syn nil) on (if (eq pos :adjective)
                                   (mapcar #'get-adj-word sense-stuff)
                                 sense-stuff)
            by #'cddr
            collect syn)))))

(defun make-synset-name-for-offset (offset pos &optional (package *wn-package*))
  (setq offset (parse-integer offset))
  (let ((*wn-package* package)
        (tokens (third (get-data-entry offset pos))))
    (destructuring-bind (file-num p1 num-senses . senses-and-more) tokens
      (declare (ignore file-num))
      (setq num-senses (parse-integer num-senses :radix 16))
      (with-list-split-after (* 2 num-senses) (sense-stuff pointers-and-more) senses-and-more
        (declare (ignore pointers-and-more))
        (let ((primary-name
               (if (eq pos :adjective) (get-adj-word (car sense-stuff)) (car sense-stuff))))
          (make-synset-name
           primary-name
           (ss_type-from-symbol p1)
           (get-synset-number-in-index-entry
            (string-downcase primary-name) offset pos)
           *wn-package*))))))

(defun get-ss_type-for-offset (offset pos)
  (ss_type-from-symbol (second (third (get-data-entry (parse-integer offset) pos)))))

(defun make-sense-name-for-offset (offset pos target &optional (package *wn-package*))
  (setq offset (parse-integer offset))
  (let ((*wn-package* package)
        (tokens (third (get-data-entry offset pos))))
    (destructuring-bind (file-num p1 num-senses . senses-and-more) tokens
      (declare (ignore file-num))
      (setq num-senses (parse-integer num-senses :radix 16))
      ;; mix 0 mingle 0 commix 0 unify 0 amalgamate 0 014 ...
      (with-list-split-after (* 2 num-senses) (sense-stuff pointers-and-more) senses-and-more
        (declare (ignore pointers-and-more))
        (let ((t-name (nth (* 2 (1- target)) sense-stuff)))
          (when (eq pos :adjective)
            (setq t-name (get-adj-word t-name)))
          (make-sense-name
           t-name (ss_type-from-symbol p1)
           (get-synset-number-in-index-entry
            (string-downcase t-name) offset pos)
           *wn-package*))))))

(defun binary-search-lines (stream entry-start)
  "uses a binary search to find the line in the file to which stream is
opened that begins with entry-start.  Obviously, lines in the file must
be sorted.  They need not all be the same length."
  (let ((start 0)
        (end (file-length stream))
        (num-reads 0))
    (loop
      (if (> start end)
          (progn
            ;(format t "*** In ~D reads, failed on: ~A~%" num-reads entry-start)
            (return cl:nil))
        (let ((try-offset (floor (+ start end) 2)))
          (multiple-value-bind
                (entry actual-offset)
              (next-entry-from-offset stream try-offset)
            (incf num-reads)
            ;(format t "~D - ~D > ~D = ~D: ~A~%"
            ;  start end try-offset actual-offset (first-n entry 10))
            (cond ((or (null entry)
                       (> actual-offset end))
                   ;; try-offset was in middle of last entry; look earlier
                   (setf end (1- try-offset)))
                  ((seq-starts-as entry-start entry)
                   ;; got it!
                   ;(format t "*** In ~D reads, got: ~A~%" num-reads (first-n entry 20))
                   ;; Caution: this does not print a whole string.
                   (return entry))
                  ((string> entry entry-start)
                   ;; look earlier
                   (setf end (1- try-offset)))
                  ((string< entry entry-start)
                   ;; look later
                   (setf start (+ actual-offset (length entry))))
                  (t (error "bad string comparisons")))))))))

(defun next-entry-from-offset (stream offset)
  "returns the next complete line after offset in stream, and the
position of its first character"
  ;; find line from offset
  (cond ((<= offset 0)
         (file-position stream 0))
        (t (file-position stream (1- offset))
           (skip-line stream)))
  ;; record position and read line
  (let ((actual (file-position stream)))
    (values (read-line stream cl:nil cl:nil)
            actual)))

;;;============================================================================--
;;;
;;; Conversion WordNet to OWL files
;;;
;;; ----------------------------------------------------------------------------------

;;;
;;;; Verb Frames
;;;

(defun make-frame-sentence (verb frame)
  "<verb> is a string to be inserted into <frame> by substituting pattern \"----\" in the <frame>."
  (let ((frame-seq (split-seq-using frame)))
    (cond ((find "----" frame-seq :test #'string=)
           (make-spaced-str (substitute verb "----" frame-seq :test #'string=)))
          ((find "----s" frame-seq :test #'string=)
           (make-spaced-str (substitute (concatenate 'string verb "s") "----s" frame-seq :test #'string=)))
          ((find "----ing" frame-seq :test #'string=)
           (make-spaced-str (substitute (concatenate 'string verb "ing") "----ing" frame-seq :test #'string=)))
          ((error "Cant happen!")))))

;;;
;;;; Count List and Tag Count
;;;

(defun countlist-file ()
  (merge-pathnames "cntlist.rev" *wnsearchdir*))

(defun get-tagcount-in-cntlistrev (key type synnumber)
  "returns the tagcount of <key> word of <type> in cntlist.rev file for <key>."
  (with-open-file (cntlist-stream (countlist-file) :direction :input)
    (let ((file-len (file-length cntlist-stream))
          (entry nil)           ; side effect at read-key
          (target-pos nil))     ; side effect at read-key
      (flet ((line-start (pos)
                         "moves file position backward from <pos> and returns the line-start position."
                         (file-position cntlist-stream pos)
                         (loop until (char= (read-char cntlist-stream) #\newline)
                             do (cond ((plusp pos) (decf pos))
                                      (t (return 0)))
                               (file-position cntlist-stream pos)
                             finally (return (file-position cntlist-stream))))
             (read-key ()
                       "In condition of line start, reads a key word up to '%'."
                       (setq target-pos (file-position cntlist-stream))
                       (setq entry (read-line cntlist-stream nil nil))
                       (unless entry (return-from get-tagcount-in-cntlistrev nil))
                       (let ((p (position #\% entry)))
                         (assert p)
                         (subseq entry 0 p))))
        ;; search target-pos and entry with binary search
        ;; set target-pos and entry
        (loop for pos = (floor (/ file-len 2)) then (floor (/ (+ pos1 pos2) 2))
            with pos1 = 0 and pos2 = file-len and lemma1 and lemma2
            until (progn (setq pos (line-start pos))
                    (or (string= key (setq lemma1 (read-key)))        ; previous line key
                        (string= key (setq lemma2 (read-key)))))      ; next line key
            do (when (= pos1 pos) (return-from get-tagcount-in-cntlistrev nil))
              (assert (< pos1 pos pos2))
              (cond ((string< key lemma1) (setq pos2 pos))
                    ((string> key lemma2) (setq pos1 pos))
                    ((and (string< lemma1 key) (string< key lemma2)) ; not found
                     (return-from get-tagcount-in-cntlistrev nil))))
        ;; collect all target entries that matches key to lemma
        (let ((this-pos target-pos)
              (this-line entry)
              (previous-lines nil)
              (next-lines nil))
          (loop for pos = (- this-pos 4) then (- pos 4)
              do (setq pos (line-start pos))
                (if (string= key (read-key)) (push entry previous-lines)
                  (return)))
          (file-position cntlist-stream this-pos)
          (read-line cntlist-stream)  ; this-line
          (loop while (string= key (read-key))
              do (push entry next-lines))
          (format t "~%~S" (append previous-lines (cons this-line (reverse next-lines))))
          (loop for line in (append previous-lines (cons this-line (reverse next-lines)))
              do (destructuring-bind (sense_key sense_number tag_cnt) (split-seq-using line)
                   (destructuring-bind (lemma lex_sense) (split-seq-using sense_key #\%)
                     (assert (string= lemma key))
                     (destructuring-bind (ss_type lex_filenum lex_lexid . more) (split-seq-using lex_sense #\:)
                       (declare (ignore lex_filenum lex_lexid more))
                       (setq ss_type (ss_type-from-number (parse-integer ss_type)))
                       (setq sense_number (parse-integer sense_number))
                       (setq tag_cnt (parse-integer tag_cnt))
                       (when (and (eq type ss_type)
                                  (= sense_number synnumber))
                         (return tag_cnt)))))))))))

(defun index-sense-file ()
  (merge-pathnames "index.sense" *wnsearchdir*))

(defun get-tagcount-from-index-sense (key type synnumber)
  (with-open-file (index-sense-stream (index-sense-file) :direction :input)
    (let ((file-len (file-length index-sense-stream))
          (entry nil)           ; side effect at read-key
          (target-pos nil))     ; side effect at read-key
      (flet ((line-start (pos)
                         "moves file position backward from <pos> and returns the line-start position."
                         (file-position index-sense-stream pos)
                         (loop until (char= (read-char index-sense-stream) #\newline)
                             do (cond ((plusp pos) (decf pos))
                                      (t (return 0)))
                               (file-position index-sense-stream pos)
                             finally (return (file-position index-sense-stream))))
             (read-key ()
                       "In condition of line start, reads a key word up to '%'."
                       (setq target-pos (file-position index-sense-stream))
                       (setq entry (read-line index-sense-stream nil nil))
                       (unless entry (return-from get-tagcount-from-index-sense nil))
                       (let ((p (position #\% entry)))
                         (assert p)
                         (subseq entry 0 p))))
        ;; search target-pos and entry with binary search
        ;; set target-pos and entry
        (loop for pos = (floor (/ file-len 2)) then (floor (/ (+ pos1 pos2) 2))
            with pos1 = 0 and pos2 = file-len and lemma1 and lemma2
            until (progn (setq pos (line-start pos))
                    (or (string= key (setq lemma1 (read-key)))        ; previous line key
                        (string= key (setq lemma2 (read-key)))))      ; next line key
            do (when (= pos1 pos) (return-from get-tagcount-from-index-sense nil))
              (assert (< pos1 pos pos2))
              (cond ((string< key lemma1) (setq pos2 pos))
                    ((string> key lemma2) (setq pos1 pos))
                    ((and (string< lemma1 key) (string< key lemma2)) ; not found
                     (return-from get-tagcount-from-index-sense nil))))
        ;; collect all target entries that matches key to lemma
        (let ((this-pos target-pos)
              (this-line entry)
              (previous-lines nil)
              (next-lines nil))
          (loop for pos = (- this-pos 4) then (- pos 4)
              do (setq pos (line-start pos))
                (if (string= key (read-key)) (push entry previous-lines)
                  (return)))
          (file-position index-sense-stream this-pos)
          (read-line index-sense-stream)  ; this-line
          (loop while (string= key (read-key))
              do (push entry next-lines))
;;;          (format t "~%~S" (append previous-lines (cons this-line (reverse next-lines))))
          (loop for line in (append previous-lines (cons this-line (reverse next-lines)))
              do (destructuring-bind (sense_key offset sense_number tag_cnt) (split-seq-using line)
                   (declare (ignore offset))
                   (destructuring-bind (lemma lex_sense) (split-seq-using sense_key #\%)
                     (assert (string= lemma key))
                     (destructuring-bind (ss_type lex_filenum lex_lexid . more) (split-seq-using lex_sense #\:)
                       (declare (ignore lex_filenum lex_lexid more))
                       (setq ss_type (ss_type-from-number (parse-integer ss_type)))
                       (setq sense_number (parse-integer sense_number))
                       (setq tag_cnt (parse-integer tag_cnt))
                       (when (and (eq type ss_type)
                                  (= sense_number synnumber))
                         (return tag_cnt)))))))))))

;;;
;;;; Verb Frames
;;;
;;; Each verb synset contains a list of generic sentence frames illustrating the types of simple sentences 
;;; in which the verbs in the synset can be used. For some verb senses, example sentences illustrating actual 
;;; uses of the verb are provided. (See Verb Example Sentences in wndb(5WN) .) Whenever there is no example sentence, 
;;; the generic sentence frames specified by the lexicographer are used. The generic sentence frames are entered in 
;;; a synset as a comma-separated list of integer frame numbers. The following list is the text of the generic frames, 
;;; preceded by their frame numbers.

(defparameter *generic-frame-texts*
  (list "Something ----s"                              ; frame number 1
        "Somebody ----s"
        "It is ----ing"
        "Something is ----ing PP"
        "Something ----s something Adjective/Noun"
        "Something ----s Adjective/Noun"
        "Somebody ----s Adjective"
        "Somebody ----s something"
        "Somebody ----s somebody"
        "Something ----s somebody"
        "Something ----s something"
        "Something ----s to somebody"
        "Somebody ----s on something"
        "Somebody ----s somebody something"
        "Somebody ----s something to somebody"
        "Somebody ----s something from somebody"
        "Somebody ----s somebody with something"
        "Somebody ----s somebody of something"
        "Somebody ----s something on somebody"
        "Somebody ----s somebody PP"
        "Somebody ----s something PP"
        "Somebody ----s PP"
        "Somebody's (body part) ----s"
        "Somebody ----s somebody to INFINITIVE"
        "Somebody ----s somebody INFINITIVE"
        "Somebody ----s that CLAUSE"
        "Somebody ----s to somebody"
        "Somebody ----s to INFINITIVE"
        "Somebody ----s whether INFINITIVE"
        "Somebody ----s somebody into V-ing something"
        "Somebody ----s something with something"
        "Somebody ----s INFINITIVE"
        "Somebody ----s VERB-ing"
        "It ----s that CLAUSE"
        "Something ----s INFINITIVE"                              ; frame number 35
        ))

;;;;;;;;;;;;;;;  miscellaneous ;;;;;;;;;;;

(defun pos-filename-affix (pos)
  (cdr (assoc pos '((:noun . "noun")
                    (:verb . "verb")
                    (:adjective . "adj")
                    (:adjectivesatellite . "adj")
                    (:adverb . "adv")))))

(defun ss_type-from-symbol (symbol)
  (cdr (assoc symbol '(("n" . :noun)
                       ("v" . :verb)
                       ("a" . :adjective)
                       ("s" . :adjectivesatellite)
                       ("r" . :adverb))
              :test #'equal)))

(defun ss_type-from-number (num)
  (cdr (assoc num '((1 . :noun)
                    (2 . :verb)
                    (3 . :adjective)
                    (4 . :adverb)
                    (5 . :adjectivesatellite))
              :test #'=)))

(defun relation-from-symbol (symbol source-type)
  (ecase source-type
    (:noun
     (cdr (assoc symbol '(("@" . :hypernym)
                          ("@i" . :hypernym-instance) ; wn21
                          ("~" . :hyponym)
                          ("~i" . :hyponym-instance) ; wn21
                          ("#m" . :holonym-member)
                          ("#s" . :holonym-substance)
                          ("#p" . :holonym-part)
                          ("%m" . :meronym-member)
                          ("%s" . :meronym-substance)
                          ("%p" . :meronym-part)
                          ("=" . :attribute)
                          ("+" . :derivationally-related)
                          ("!" . :antonym)
                          ("-" . :member-of-this-domain)
                          (";" . :domain-of-synset)
                          (";r" . :domain-of-synset-REGION)
                          (";c" . :domain-of-synset-CATEGORY)
                          (";u" . :domain-of-synset-USAGE)
                          ("-c" . :member-of-this-domain-CATEGORY)
                          ("-r" . :member-of-this-domain-REGION) 
                          ("-u" . :member-of-this-domain-USAGE)
                          )
                 :test #'equal)))
    (:verb
     (cdr (assoc symbol '(("@" . :hypernym)
                          ("~" . :hyponym)
                          ("!" . :antonym)
                          ("*" . :entailment)
                          (">" . :cause)
                          ("^" . :see-also)
                          ("<" . :participle)
                          ("+" . :derivationally-related)      
                          ("$" . :verb-group)
                          (";" . :domain-of-synset)
                          ("-c" . :member-of-this-domain-CATEGORY)
                          (";u" . :domain-of-synset-USAGE)
                          (";c" . :domain-of-synset-CATEGORY)
                          (";r" . :domain-of-synset-REGION)
                          )
                 :test #'equal)))
    (:adjective
     (if (eql (char symbol 0) #\\)
         :adjective-pertains-to
       (cdr (assoc symbol '(("!" . :antonym)
                            ("&" . :similar-to)
                            ("<" . :participle-of-verb)
                            ("=" . :attribute-of)
                            ("^" . :see-also)
                            (";c" . :domain-of-synset-CATEGORY)
                            ("+" . :derivationally-related)  ; this is needed in practice
                            (";r" . :domain-of-synset-REGION)
                            (";u" . :domain-of-synset-USAGE)
                            )
                   :test #'equal))))
    (:adverb
     (if (eql (char symbol 0) #\\)
         :adverb-pertains-to
       (cdr (assoc symbol '(("!" . :antonym)
                            ("+" . :derivationally-related)  ; this is needed in practice
                            (";c" . :domain-of-synset-CATEGORY)
                            (";r" . :domain-of-synset-REGION)
                            (";u" . :domain-of-synset-USAGE)
                            )
                   :test #'equal))))))

;;; EOF
