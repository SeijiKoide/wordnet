;;;
;;;; Japanese WordNet Information Retrieval and OWL Conversion Program
;;;                  http://www.w3.org/TR/2006/WD-wordnet-rdf-20060619/
;;; This program is applied to Allegro8.1, 8.2, 9.0 Modern, sbcl, and Japanese WordNet.
;;; This program is based on program for Princeton WordNet. Most of code is copied from 
;;; one for Princeton WordNet. So, this program does not need the former one but need 
;;; Princeton WordNet 3.0, because this program uses it.
;;;
;; History
;; -------
;; 2011/06/17    File created.
;;; ==================================================================================

(in-package :wn)

(defparameter *outputted-word-list* nil)
(defparameter *jpn-outputted-word-list* nil)

;;;
;;;;
;;;

#|
(in-package :wn)
(make-one-big-rdf-file)
|#

(defun make-one-big-rdf-file ()
  (setq *outputted-word-list* nil)
  (setq *jpn-outputted-word-list* nil)
  (with-open-file (outstream *one-big-rdf-file-path* :direction :output)
    (format outstream *xml-decl*)
    (format outstream *doc-decl*)
    (format outstream *rdf-decl-prolog*)
    (wnjpn:with-open-jpn
      (%make-rdf-file-of-word :noun outstream)
      (%make-rdf-file-of-word :verb outstream)
      (%make-rdf-file-of-word :adjective outstream)
      (%make-rdf-file-of-word :adverb outstream)
      (%make-rdf-file-without-word :noun outstream)
      (%make-rdf-file-without-word :verb outstream)
      (%make-rdf-file-without-word :adjective outstream)
      (%make-rdf-file-without-word :adverb outstream))
    (format outstream *rdf-decl-epilog*)
    t))

(defun %make-rdf-file-of-word (pos outstream)
  "outputs word entries of <pos> to <outstream>. 
   A word may appears as synonymous word but not as primary name in data files. 
   Therefore, it is needed to state a word for all synonymous words in synset entries."
  (with-open-file (data-stream (pos-data-file pos) :direction :input)
    (loop for entry = (read-line data-stream nil nil)
        while entry
        unless (char= #\Space (char entry 0))
        do (destructuring-bind
               (offset file-num p1 num-senses . senses-and-more) (split-seq-using entry)
             (declare (ignore file-num))
             (let ((ss_type (ss_type-from-symbol p1)))
               (assert (or (eq ss_type pos)
                           (and (eq ss_type :adjectivesatellite)
                                (eq pos :adjective))) ()
                       "bad synset pos in: ~S" entry)
               (setq offset (parse-integer offset))
               (with-list-split-after (* 2 (parse-integer num-senses :radix 16))
                 (sense-stuff pointers-and-more) senses-and-more
                 (declare (ignore pointers-and-more))
                 ;; print a primary-name
                 (format t "~%Reading ~S ... " (if (eq pos :adjective) (get-adj-word (car sense-stuff)) (car sense-stuff)))
                 (let ((synonyms (loop for (word nil) on sense-stuff by #'cddr collect word)))
                   (loop for word in synonyms with word-name
                       do (when (eq pos :adjective) (setq word (get-adj-word word)))
                         (setq word-name (make-word-name word))
                         ;; Word entity output
                         (unless (member word-name *outputted-word-list*)
                           (let ((noun-sense-names
                                  (loop for offset in (offsets-in-index-entry (string-downcase word) :noun)
                                      with i = 0
                                      collect (make-sense-name word :noun (incf i))))
                                 (verb-sense-names
                                  (loop for offset in (offsets-in-index-entry (string-downcase word) :verb)
                                      with i = 0
                                      collect (make-sense-name word :verb (incf i))))
                                 (adjective-sense-names
                                  (loop for offset in (offsets-in-index-entry (string-downcase word) :adjective)
                                      with i = 0
                                      collect (make-sense-name word (get-ss_type-for-offset offset :adjective) (incf i))))
                                 (adverb-sense-names
                                  (loop for offset in (offsets-in-index-entry (string-downcase word) :adverb)
                                      with i = 0
                                      collect (make-sense-name word :adverb (incf i)))))
                             (format outstream
                                 (if (collocated-p word) *collocation-description-template*
                                   *word-description-template*)
                               (string-downcase (package-name (symbol-package word-name)))
                               word-name (underscore2space word)
                               (append (mappend #'(lambda (name) (list (string-downcase (package-name (symbol-package name))) name)) noun-sense-names)
                                       (mappend #'(lambda (name) (list (string-downcase (package-name (symbol-package name))) name)) verb-sense-names)
                                       (mappend #'(lambda (name) (list (string-downcase (package-name (symbol-package name))) name)) adjective-sense-names)
                                       (mappend #'(lambda (name) (list (string-downcase (package-name (symbol-package name))) name)) adverb-sense-names)))
                             (push word-name *outputted-word-list*))))
                   ;; get synonimous japanese words from wnjpn.db
                   (let ((*wn-package* *wnja-package*))
                     (loop for word in (wnjpn:get-jpn-synonyms offset pos) with word-name
                         do (setq word-name (make-word-name word))   ; no adjective marker in Japanese
                           ;; Japanese word entity output
                           (unless (member word-name *jpn-outputted-word-list*)
                             ;; this is not proper for Japanese grammer but it is as is.
                             (let ((noun-sense-names
                                    (loop for synset in (wnjpn:synsets-in-jpndb word :noun)
                                        with i = 0
                                        collect (make-sense-name word :noun (incf i))))
                                   (verb-sense-names
                                    (loop for synset in (wnjpn:synsets-in-jpndb word :verb)
                                        with i = 0
                                        collect (make-sense-name word :verb (incf i))))
                                   (adjective-sense-names
                                    (loop for synset in (wnjpn:synsets-in-jpndb word :adjective)
                                        with i = 0
                                        collect (make-sense-name word :adjective (incf i))))
                                   (adverb-sense-names
                                    (loop for synset in (wnjpn:synsets-in-jpndb word :adverb)
                                        with i = 0
                                        collect (make-sense-name word :adverb (incf i)))))
                               (format outstream
                                   (if (collocated-p word) *collocation-description-template*
                                     *word-description-template*)
                                 (string-downcase (package-name (symbol-package word-name))) word-name (underscore2space word)
                                 (append
                                  (mappend #'(lambda (name) (list (string-downcase (package-name (symbol-package name))) name))
                                           noun-sense-names)
                                  (mappend #'(lambda (name) (list (string-downcase (package-name (symbol-package name))) name))
                                           verb-sense-names)
                                  (mappend #'(lambda (name) (list (string-downcase (package-name (symbol-package name))) name))
                                           adjective-sense-names)
                                  (mappend #'(lambda (name) (list (string-downcase (package-name (symbol-package name))) name))
                                           adverb-sense-names)))
                               (push word-name *jpn-outputted-word-list*))))))))))))

(defun %make-rdf-file-without-word (pos outstream)
  "outputs sense resources and synset resources of <pos> to <outstream>."
  (with-open-file (data-stream (pos-data-file pos) :direction :input)
    (loop for entry = (read-line data-stream nil nil)
        while entry
        unless (char= #\Space (char entry 0))
        do (destructuring-bind
               (offset file-num p1 num-senses . senses-and-more) (split-seq-using entry)
             (declare (ignore file-num))
             (let ((ss_type (ss_type-from-symbol p1)))
               (assert (or (eq ss_type pos)
                           (and (eq ss_type :adjectivesatellite)
                                (eq pos :adjective))) ()
                       "bad synset pos in: ~S" entry)
               (setq offset (parse-integer offset))
               (with-list-split-after (* 2 (parse-integer num-senses :radix 16))
                 (sense-stuff pointers-and-more) senses-and-more
                 (let ((primary-name (if (eq pos :adjective) (get-adj-word (car sense-stuff)) (car sense-stuff)))
                       (synonyms (loop for (word nil) on (cddr sense-stuff) by #'cddr collect word)))
                   ;; print a primary-name
                   (format t "~%Reading ~S ... " primary-name)
                   (with-list-split-after (* 4 (parse-integer (car pointers-and-more)))
                     (pointer-stuff rest) (cdr pointers-and-more)
                     (let ((word&frame-list nil)
                           (gloss-stuff nil))
                       (cond ((eq pos :verb) ; there are frames
                              (with-list-split-after (* 3 (parse-integer (car rest)))
                                (frame-stuff gst) (cdr rest)
                                (setq gloss-stuff gst)
                                (when frame-stuff
                                  (loop for (symbol fnum wnum) on frame-stuff by #'cdddr
                                      do 
                                        (assert (string= symbol "+"))
                                        (setq fnum (parse-integer fnum))
                                        (setq wnum (parse-integer wnum :radix 16))
                                        (let ((words (if (zerop wnum)
                                                         (loop for (word nil) on sense-stuff by #'cddr
                                                             collect word)
                                                       (list (nth (* 2 (1- wnum)) sense-stuff))))
                                              (frame (nth (1- fnum) *generic-frame-texts*)))
                                          (loop for word in words
                                              do (push (cons word frame) word&frame-list))))
                                  (setq word&frame-list (nreverse word&frame-list)))))
                             (t (setq gloss-stuff rest)))
                       ;; "| a building in which the business of banking transacted; ... "
                       (assert (or (null gloss-stuff) (string= (car gloss-stuff) "|")))
                       (let ((synnumber
                              (get-synset-number-in-index-entry
                               (string-downcase primary-name) offset pos)))
                         (let ((word-name (make-word-name primary-name))
                               (subjective-word-sense-name
                                (make-sense-name primary-name ss_type synnumber))
                               (subjective-synset-name
                                (make-synset-name primary-name ss_type synnumber))
                               (tagcount
                                (get-tagcount-from-index-sense
                                 (string-downcase primary-name) ss_type synnumber))
                               (synonymous-word-sense-names
                                (loop for syno in synonyms
                                    do (when (eq pos :adjective) (setq syno (get-adj-word syno)))
                                    collect (make-sense-name
                                             syno ss_type
                                             (get-synset-number-in-index-entry
                                              (string-downcase syno) offset pos))))
                               ;;
                               (jpn-word-sense-names             ; turn to objective word sense names
                                (loop for jpnw in (wnjpn:get-jpn-synonyms offset pos)
                                    collect (make-jpn-sense-name offset jpnw pos *wnja-package*)))
                               ;;
                               (derivationallyRelated-source&target-sense-name-lists
                                (loop for (relation target-offset p which) on pointer-stuff by #'cddddr
                                    when (string= relation "+")
                                    collect
                                      (let ((source (parse-integer (subseq which 0 2) :radix 16))  ; "304" -> 3
                                            (target (parse-integer (subseq which 2) :radix 16)))   ; "304" -> 4
                                        (let ((s-name (nth (* 2 (1- source)) sense-stuff))
                                              (target-sense-name
                                               (make-sense-name-for-offset
                                                target-offset (ss_type-from-symbol p) target)))
                                          (when (eq pos :adjective) (setq s-name (get-adj-word s-name)))
                                          (cons s-name target-sense-name)))))
                               (pertainsTo-source&target-sense-name-lists
                                (loop for (relation target-offset p which) on pointer-stuff by #'cddddr
                                    when (string= relation "\\")                     ; \ 01731720 a 0203 or \ 00094515 a 0101
                                    collect
                                      (let ((source (parse-integer (subseq which 0 2) :radix 16))   ; 2
                                            (target (parse-integer (subseq which 2) :radix 16)))    ; 3
                                        (let ((s-name (nth (* 2 (1- source)) sense-stuff))  ; merely 0 simply 0 just 0 only 2 but 1
                                              (target-sense-name
                                               (make-sense-name-for-offset
                                                target-offset (ss_type-from-symbol p) target)))
                                          (when (eq pos :adjective) (setq s-name (get-adj-word s-name)))
                                          (cons s-name target-sense-name)))))
                               (antonym-source&target-sense-name-lists
                                (loop for (relation target-offset p which) on pointer-stuff by #'cddddr
                                    when (string= relation "!")
                                    collect
                                      (let ((source (parse-integer (subseq which 0 2) :radix 16))  ; "304" -> 3
                                            (target (parse-integer (subseq which 2) :radix 16)))   ; "304" -> 4
                                        (let ((s-name (nth (* 2 (1- source)) sense-stuff))
                                              (target-sense-name
                                               (make-sense-name-for-offset
                                                target-offset (ss_type-from-symbol p) target)))
                                          (when (eq pos :adjective) (setq s-name (get-adj-word s-name)))
                                          (cons s-name target-sense-name)))))
                               (participleof-source&target-sense-name-lists
                                (loop for (relation target-offset p which) on pointer-stuff by #'cddddr
                                    when (string= relation "<")
                                    collect
                                      (let ((source (parse-integer (subseq which 0 2) :radix 16))   ; 2
                                            (target (parse-integer (subseq which 2) :radix 16)))    ; 3
                                        (let ((s-name (nth (* 2 (1- source)) sense-stuff))  ; merely 0 simply 0 just 0 only 2 but 1
                                              (target-sense-name
                                               (make-sense-name-for-offset
                                                target-offset (ss_type-from-symbol p) target)))
                                          (when (eq pos :adjective) (setq s-name (get-adj-word s-name)))
                                          (cons s-name target-sense-name))))))
                           ;; Word entity output
                           ;; empty code
                           ;; First word sense output
                           (word-sense-output outstream primary-name ss_type subjective-word-sense-name
                                              word-name subjective-synset-name
                                    derivationallyRelated-source&target-sense-name-lists
                                    pertainsTo-source&target-sense-name-lists
                                    antonym-source&target-sense-name-lists
                                    participleof-source&target-sense-name-lists
                                    word&frame-list
                                    tagcount)
                           
                           ;; First synset output
                           (synset-output outstream primary-name ss_type subjective-synset-name offset
                                          (cons subjective-word-sense-name
                                                (append synonymous-word-sense-names
                                                        jpn-word-sense-names))
                                          pointer-stuff gloss-stuff)

                           ;; synonyms and synonym senses
                           (loop for syno in synonyms
                               for sense-name in synonymous-word-sense-names
                               do
                                 (when (eq pos :adjective) (setq syno (get-adj-word syno)))
                                 (word-sense-output outstream syno ss_type sense-name
                                                    (make-word-name syno) subjective-synset-name
                                                    derivationallyRelated-source&target-sense-name-lists
                                                    pertainsTo-source&target-sense-name-lists
                                                    antonym-source&target-sense-name-lists
                                                    participleof-source&target-sense-name-lists
                                                    word&frame-list
                                                    (get-tagcount-from-index-sense
                                                     (string-downcase syno)
                                                     ss_type
                                                     (get-synset-number-in-index-entry
                                                      (string-downcase syno) offset pos))))
                           
                           ;; print japanese word sense as subject
                           (loop for subjective-word-sense-name in jpn-word-sense-names
                                 for jpnw in (wnjpn:get-jpn-synonyms offset pos)
                               do (%%word-sense-header-output outstream ss_type
                                                              subjective-word-sense-name
                                                              jpnw 0
                                                              (make-word-name jpnw *wnja-package*)
                                                              subjective-synset-name)
                                 (format outstream
                                     (ecase ss_type
                                       (:noun *noun-word-sense-description-tail*)
                                       (:verb *verb-word-sense-description-tail*)
                                       (:adjective *adjective-word-sense-description-tail*)
                                       (:adjectivesatellite *adjectivesatellite-word-sense-description-tail*)
                                       (:adverb *adverb-word-sense-description-tail*)))
                                 ))))))))))))


(defun word-sense-output (outstream word ss_type sense-name word-name synset-name
                                    derivationallyRelated-source&target-sense-name-lists
                                    pertainsTo-source&target-sense-name-lists
                                    antonym-source&target-sense-name-lists
                                    participleof-source&target-sense-name-lists
                                    word&frame-list
                                    tagcount)
  ;; header and some bodies
  (%%word-sense-header-output outstream ss_type sense-name word tagcount word-name synset-name)
  ;; other bodies
  (%%word-sense-body-output outstream ss_type word
                            derivationallyRelated-source&target-sense-name-lists
                            pertainsTo-source&target-sense-name-lists
                            antonym-source&target-sense-name-lists
                            participleof-source&target-sense-name-lists
                            word&frame-list)
  ;; tail
  (format outstream
      (ecase ss_type
        (:noun *noun-word-sense-description-tail*)
        (:verb *verb-word-sense-description-tail*)
        (:adjective *adjective-word-sense-description-tail*)
        (:adjectivesatellite *adjectivesatellite-word-sense-description-tail*)
        (:adverb *adverb-word-sense-description-tail*)))
  )

(defun synset-output (outstream word ss_type synset-name offset 
                                sense-names pointer-stuff gloss-stuff)
  "outputs synset resources for <word> and <ss_type> of <offset> 
   with <sense-names>, <pointer-stuff>, and <gloss-stuff>."
  (let ((hyponym-target-synset-names
         (loop for (relation target-offset pos nil) on pointer-stuff by #'cddddr
             when (string= relation "@")  ; :hypernym is ***:hyponymOf
             collect
               (make-synset-name-for-offset target-offset (ss_type-from-symbol pos))))
        (hyponym-instance-target-synset-names
         (loop for (relation target-offset pos nil) on pointer-stuff by #'cddddr
             when (string= relation "@i")
             collect
               (make-synset-name-for-offset target-offset (ss_type-from-symbol pos))))
        (hypernym-target-synset-names
         (loop for (relation target-offset pos nil) on pointer-stuff by #'cddddr
             when (string= relation "~")  ; :hyponym is ***:hypernymOf
             collect
               (make-synset-name-for-offset target-offset (ss_type-from-symbol pos))))
        (hypernym-instance-target-synset-names
         (loop for (relation target-offset pos nil) on pointer-stuff by #'cddddr
             when (string= relation "~i")
             collect
               (make-synset-name-for-offset target-offset (ss_type-from-symbol pos))))
        (membermeronym-target-synset-names
         (loop for (relation target-offset p nil) on pointer-stuff by #'cddddr
             when (string= relation "#m")
             collect
               (make-synset-name-for-offset target-offset (ss_type-from-symbol p))))
        (memberholonym-target-synset-names
         (loop for (relation target-offset p nil) on pointer-stuff by #'cddddr
             when (string= relation "%m")
             collect
               (make-synset-name-for-offset target-offset (ss_type-from-symbol p))))
        (partmeronym-target-synset-names
         (loop for (relation target-offset p nil) on pointer-stuff by #'cddddr
             when (string= relation "#p")
             collect
               (make-synset-name-for-offset target-offset (ss_type-from-symbol p))))
        (partholonym-target-synset-names
         (loop for (relation target-offset p nil) on pointer-stuff by #'cddddr
             when (string= relation "%p")
             collect
               (make-synset-name-for-offset target-offset (ss_type-from-symbol p))))
        (substancemeronym-target-synset-names
         (loop for (relation target-offset p nil) on pointer-stuff by #'cddddr
             when (string= relation "#s")
             collect
               (make-synset-name-for-offset target-offset (ss_type-from-symbol p))))
        (substanceholonym-target-synset-names
         (loop for (relation target-offset p nil) on pointer-stuff by #'cddddr
             when (string= relation "%s")
             collect
               (make-synset-name-for-offset target-offset (ss_type-from-symbol p))))
        (similarity-target-synset-names
         (loop for (relation target-offset p nil) on pointer-stuff by #'cddddr
             when (string= relation "&")
             collect
               (make-synset-name-for-offset target-offset (ss_type-from-symbol p))))
        (attribute-target-synset-names
         (loop for (relation target-offset p nil) on pointer-stuff by #'cddddr
             when (string= relation "=")
             collect
               (make-synset-name-for-offset target-offset (ss_type-from-symbol p))))
        (sameverbgroupas-target-synset-names
         (loop for (relation target-offset p nil) on pointer-stuff by #'cddddr
             when (string= relation "$")
             collect
               (make-synset-name-for-offset target-offset (ss_type-from-symbol p))))
        (entailment-target-synset-names
         (loop for (relation target-offset p nil) on pointer-stuff by #'cddddr
             when (string= relation "*")
             collect
               (make-synset-name-for-offset target-offset (ss_type-from-symbol p))))
        (causes-target-synset-names
         (loop for (relation target-offset p nil) on pointer-stuff by #'cddddr
             when (string= relation ">")
             collect
               (make-synset-name-for-offset target-offset (ss_type-from-symbol p))))
        #+:never (member-of-this-domain
                  (loop for (relation nil nil nil) on pointer-stuff by #'cddddr
                      when (string= relation "-")
                      do (error "Cant happen!")))
        #+:never (domain-of-synset
                  (loop for (relation nil nil nil) on pointer-stuff by #'cddddr
                      when (string= relation ";")
                      do (error "Cant happen!")))
        (classified-by-topic-target-names              ; classifiedByTopic
         (loop for (relation target-offset p nil) on pointer-stuff by #'cddddr
             when (string= relation ";c")
             collect (make-synset-name-for-offset target-offset (ss_type-from-symbol p))))
        (classified-by-usage-target-names              ; classfiedByUsage
         (loop for (relation target-offset p nil) on pointer-stuff by #'cddddr
             when (string= relation ";u")
             collect (make-synset-name-for-offset target-offset (ss_type-from-symbol p))))
        (classified-by-region-target-names
         (loop for (relation target-offset p nil) on pointer-stuff by #'cddddr
             when (string= relation ";r")
             collect (make-synset-name-for-offset target-offset (ss_type-from-symbol p))))
        (member-of-this-domain-topic-target-names        ; classifiesTopic
         (loop for (relation target-offset p nil) on pointer-stuff by #'cddddr
             when (string= relation "-c")
             collect (make-synset-name-for-offset target-offset (ss_type-from-symbol p))))
        (member-of-this-domain-region-target-names
         (loop for (relation target-offset p nil) on pointer-stuff by #'cddddr
             when (string= relation "-r")
             collect (make-synset-name-for-offset target-offset (ss_type-from-symbol p))))
        (member-of-this-domain-usage-target-names
         (loop for (relation target-offset p nil) on pointer-stuff by #'cddddr
             when (string= relation "-u")
             collect (make-synset-name-for-offset target-offset (ss_type-from-symbol p)))))
    ;; header and some bodies output
    (%%synset-header-output outstream ss_type synset-name word offset)
    ;; body
    (loop for sense-name in (sort (copy-list sense-names) #'string<)
        do (format outstream *synset-description-template-body*
             (string-downcase (package-name (symbol-package sense-name)))
             sense-name))
    (when hyponym-target-synset-names
      (ecase ss_type ((:noun :verb) t))
      (format outstream
          "~{  <wn20schema:hyponymOf rdf:resource=\"&~A;~A\"/>~^~%~}~%"
        (mapcan #'(lambda (synset) (list (string-downcase (package-name (symbol-package synset))) synset))
          (sort hyponym-target-synset-names #'string<))))
    (when hyponym-instance-target-synset-names
      (ecase ss_type ((:noun :verb) t))
      (format outstream
          "~{  <wn21schema:instanceHyponymOf rdf:resource=\"&~A;~A\"/>~^~%~}~%"
        (mapcan #'(lambda (synset) (list (string-downcase (package-name (symbol-package synset))) synset))
          (sort hyponym-instance-target-synset-names #'string<))))
    (when hypernym-target-synset-names
      (ecase ss_type ((:noun :verb) t))
      (format outstream
          "~{  <wn20schema:hypernymOf rdf:resource=\"&~A;~A\"/>~^~%~}~%"
        (mapcan #'(lambda (synset) (list (string-downcase (package-name (symbol-package synset))) synset))
          (sort hypernym-target-synset-names #'string<))))
    (when hypernym-instance-target-synset-names
      (ecase ss_type ((:noun :verb) t))
      (format outstream
          "~{  <wn21schema:instanceHypernymOf rdf:resource=\"&~A;~A\"/>~^~%~}~%"
        (mapcan #'(lambda (synset) (list (string-downcase (package-name (symbol-package synset))) synset))
          (sort hypernym-instance-target-synset-names #'string<))))
    (when membermeronym-target-synset-names
      (ecase ss_type (:noun t))
      (format outstream
          "~{  <wn20schema:memberMeronymOf rdf:resource=\"&~A;~A\"/>~^~%~}~%"
        (mapcan #'(lambda (synset) (list (string-downcase (package-name (symbol-package synset))) synset))
          (sort membermeronym-target-synset-names #'string<))))
    (when memberholonym-target-synset-names
      (ecase ss_type (:noun t))
      (format outstream
          "~{  <wn20schema:memberHolonymOf rdf:resource=\"&~A;~A\"/>~^~%~}~%"
        (mapcan #'(lambda (synset) (list (string-downcase (package-name (symbol-package synset))) synset))
          (sort memberholonym-target-synset-names #'string<))))
    (when partmeronym-target-synset-names
      (ecase ss_type (:noun t))
      (format outstream
          "~{  <wn20schema:partMeronymOf rdf:resource=\"&~A;~A\"/>~^~%~}~%"
        (mapcan #'(lambda (synset) (list (string-downcase (package-name (symbol-package synset))) synset))
          (sort partmeronym-target-synset-names #'string<))))
    (when partholonym-target-synset-names
      (ecase ss_type (:noun t))
      (format outstream
          "~{  <wn20schema:partHolonymOf rdf:resource=\"&~A;~A\"/>~^~%~}~%"
        (mapcan #'(lambda (synset) (list (string-downcase (package-name (symbol-package synset))) synset))
          (sort partholonym-target-synset-names #'string<))))
    (when substancemeronym-target-synset-names
      (ecase ss_type (:noun t))
      (format outstream
          "~{  <wn20schema:substanceMeronymOf rdf:resource=\"&~A;~A\"/>~^~%~}~%"
        (mapcan #'(lambda (synset) (list (string-downcase (package-name (symbol-package synset))) synset))
          (sort substancemeronym-target-synset-names #'string<))))
    (when substanceholonym-target-synset-names
      (ecase ss_type (:noun t))
      (format outstream
          "~{  <wn20schema:substanceHolonymOf rdf:resource=\"&~A;~A\"/>~^~%~}~%"
        (mapcan #'(lambda (synset) (list (string-downcase (package-name (symbol-package synset))) synset))
          (sort substanceholonym-target-synset-names #'string<))))
    (when similarity-target-synset-names
      (ecase ss_type ((:adjective :adjectivesatellite) t))
      (format outstream
          "~{  <wn20schema:similarTo rdf:resource=\"&~A;~A\"/>~^~%~}~%"
        (mapcan #'(lambda (synset) (list (string-downcase (package-name (symbol-package synset))) synset))
          (sort similarity-target-synset-names #'string<))))
    (when attribute-target-synset-names
      (format outstream
          (ecase ss_type
            (:noun      "~{  <wn20schema:attribute rdf:resource=\"&~A;~A\"/>~^~%~}~%")
            (:adjective "~{  <wn20schema:attributeOf rdf:resource=\"&~A;~A\"/>~^~%~}~%"))
        (mapcan #'(lambda (synset) (list (string-downcase (package-name (symbol-package synset))) synset))
          (sort attribute-target-synset-names #'string<))))
    (when sameverbgroupas-target-synset-names
      (ecase ss_type (:verb t))
      (format outstream
          "~{  <wn20schema:sameVerbGroupAs rdf:resource=\"&~A;~A\"/>~^~%~}~%"
        (mapcan #'(lambda (synset) (list (string-downcase (package-name (symbol-package synset))) synset))
          (sort sameverbgroupas-target-synset-names #'string<))))
    (when entailment-target-synset-names
      (ecase ss_type (:verb t))
      (format outstream
          "~{  <wn20schema:entails rdf:resource=\"&~A;~A\"/>~^~%~}~%"
        (mapcan #'(lambda (synset) (list (string-downcase (package-name (symbol-package synset))) synset))
          (sort entailment-target-synset-names #'string<))))
    (when causes-target-synset-names
      (ecase ss_type (:verb t))
      (format outstream
          "~{  <wn20schema:causes rdf:resource=\"&~A;~A\"/>~^~%~}~%"
        (mapcan #'(lambda (synset) (list (string-downcase (package-name (symbol-package synset))) synset))
          (sort causes-target-synset-names #'string<))))
    (when classified-by-topic-target-names
      (format outstream
          "~{  <wn20schema:classifiedByTopic rdf:resource=\"&~A;~A\"/>~^~%~}~%"
        (mapcan #'(lambda (target) (list (string-downcase (package-name (symbol-package target))) target))
          (sort classified-by-topic-target-names #'string<))))
    (when classified-by-usage-target-names
      (format outstream
          "~{  <wn20schema:classifiedByUsage rdf:resource=\"&~A;~A\"/>~^~%~}~%"
        (mapcan #'(lambda (target) (list (string-downcase (package-name (symbol-package target))) target))
          (sort classified-by-usage-target-names #'string<))))
    (when classified-by-region-target-names
      (format outstream
          "~{  <wn20schema:classifiedByRegion rdf:resource=\"&~A;~A\"/>~^~%~}~%"
        (mapcan #'(lambda (target) (list (string-downcase (package-name (symbol-package target)) target))
          (sort classified-by-region-target-names #'string<))))
    (when member-of-this-domain-topic-target-names
      (ecase ss_type (:noun t))
      (format outstream
          "~{  <wn20schema:classifiesTopic rdf:resource=\"&~A;~A\"/>~^~%~}~%"
        (mapcan #'(lambda (target) (list (string-downcase (package-name (symbol-package target))) target))
          (sort member-of-this-domain-topic-target-names #'string<))))
    (when member-of-this-domain-region-target-names
      (ecase ss_type (:noun t))
      (format outstream
          "~{  <wn20schema:classifiesRegion rdf:resource=\"&~A;~A\"/>~^~%~}~%"
        (mapcan #'(lambda (target) (list (string-downcase (package-name (symbol-package target))) target))
          (sort member-of-this-domain-region-target-names #'string<))))
    (when member-of-this-domain-usage-target-names
      (ecase ss_type (:noun t))
      (format outstream
          "~{  <wn20schema:classifiesUsage rdf:resource=\"&~A;~A\"/>~^~%~}~%"
        (mapcan #'(lambda (target) (list (string-downcase (package-name (symbol-package target))) target))
          (sort member-of-this-domain-usage-target-names #'string<))))
    ;; gloss
    (when gloss-stuff
      (format outstream "  <wn20schema:gloss>~A</wn20schema:gloss>~%"
        (escape-gloss-decls (spaced-str (cdr gloss-stuff)))))
    (format outstream "  <wn20schema:gloss xml:lang=\"ja\">~A</wn20schema:gloss>~%"
      (escape-gloss-decls (spaced-str (wnjpn:get-jpn-gloss offset ss_type))))
    ;; tail
    (format outstream (ecase ss_type
                        (:noun *noun-synset-description-tail*)
                        (:verb *verb-synset-description-tail*)
                        (:adjective *adjective-synset-description-tail*)
                        (:adjectivesatellite *adjective-satellite-synset-description-tail*)
                        (:adverb *adverb-synset-description-tail*)))))


;;;
;;; If gloss strings include '<' and '>', they are escaped with '&lt' and '&gt', because
;;; gloss strings are inserted as contents in XML documents.
;;;

;;;
;;; Word dying, dead, etc. appear as noun and adjective. Exactly, the lemma for adjective is "dying(a)".
;;; W3C original does not modify "dying(a)" to "dying" as word, sense, and synset name.
;;; Where we modifiy it with removing "(*)", even if it is an adjective that follows "(*)".
;;; Thus, in this version word-dying and word-dead for noun and word-dying and word-dead for adjective corrides each other.
;;;
