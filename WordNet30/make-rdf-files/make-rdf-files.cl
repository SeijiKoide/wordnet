;;;-*- Mode: common-lisp; syntax: common-lisp; package: wn; base: 10 -*-
;;;
;;;; WordNet Dictionary Information Retrieval and OWL Conversion Program
;;;                  http://www.w3.org/TR/2006/WD-wordnet-rdf-20060619/
;;; This program is applied to Allegro8.1, 8.2, 9.0 Modern, sbcl, and WordNet.
;;;
;;;
;; History
;; -------
;; 2011/06/17    File created.
;;; ==================================================================================

(in-package :wn)

(defparameter *outputted-word-list* nil)

;;;
;;;;
;;;

#|
(in-package :wn)
(make-one-big-rdf-file)
|#

(defun make-one-big-rdf-file ()
  (setq *outputted-word-list* nil)
  (with-open-file (outstream *one-big-rdf-file-path* :direction :output)
    (format outstream *xml-decl*)
    (format outstream *doc-decl*)
    (format outstream *rdf-decl-prolog*)
    (%make-rdf-file-of-word :noun outstream)
    (%make-rdf-file-of-word :verb outstream)
    (%make-rdf-file-of-word :adjective outstream)
    (%make-rdf-file-of-word :adverb outstream)
    (%make-rdf-file-without-word :noun outstream)
    (%make-rdf-file-without-word :verb outstream)
    (%make-rdf-file-without-word :adjective outstream)
    (%make-rdf-file-without-word :adverb outstream)
    (format outstream *rdf-decl-epilog*)
    t))

(defun %make-rdf-file-of-word (pos outstream)
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
                               word-name (underscore2space word)
                               (append noun-sense-names verb-sense-names adjective-sense-names adverb-sense-names))
                             (push word-name *outputted-word-list*)))))))))))

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
                 (let ((primary-name (if (eq pos :adjective) (get-adj-word (car sense-stuff)) (car sense-stuff))))
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
                                        (let ((words (cond ((zerop wnum) (loop for (word nil) on sense-stuff by #'cddr collect word))
                                                           (t (list (nth (* 2 (1- wnum)) sense-stuff)))))
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
                               (source-word-sense-name
                                (make-sense-name primary-name ss_type synnumber))
                               (source-synset-name
                                (make-synset-name primary-name ss_type synnumber))
                               (tagcount
                                (get-tagcount-from-index-sense
                                 (string-downcase primary-name) ss_type synnumber))
                               (synonymous-word-sense-names
                                (loop for (word nil) on (cddr sense-stuff) by #'cddr
                                    do (when (eq pos :adjective) (setq word (get-adj-word word)))
                                    collect (make-sense-name
                                             word ss_type
                                             (get-synset-number-in-index-entry
                                              (string-downcase word) offset pos))))
                               ;;
                               (hyponym-target-synset-names
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
                                          (when (eq pos :adjective)
                                            (setq s-name (get-adj-word s-name)))
                                          (cons s-name target-sense-name)))))
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
                                    collect (make-synset-name-for-offset target-offset (ss_type-from-symbol p))))
                               )
                           ;; Word entity output
                           ;; empty code
                           ;; First word sense output
                           (format outstream
                               (ecase ss_type
                                 (:noun *noun-word-sense-description-temp*)
                                 (:verb *verb-word-sense-description-temp*)
                                 (:adjective *adjective-word-sense-description-temp*)
                                 (:adjectivesatellite *adjectivesatellite-word-sense-description-temp*)
                                 (:adverb *adverb-word-sense-description-temp*))
                             source-word-sense-name (underscore2space primary-name)
                             (or tagcount 0) word-name source-synset-name)
                           ;; other bodies
                           (let ((target-sense-names
                                  (mapcar #'cdr
                                    (remove-if-not #'(lambda (acons) (string= primary-name (car acons)))
                                                   derivationallyRelated-source&target-sense-name-lists))))
                             (when target-sense-names
                               (format outstream
                                   (ecase pos
                                     (:noun "~{  <wn20schema:derivationallyRelated rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%")
                                     (:verb "~{  <wn20schema:derivationallyRelated rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%")
                                     ;; wn21
                                     (:adjective "~{  <wn20schema:derivationallyRelated rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%")
                                     (:adverb "~{  <wn20schema:derivationallyRelated rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"))
                                 (sort target-sense-names #'string<))))
                           (let ((target-sense-names
                                  (mapcar #'cdr
                                    (remove-if-not #'(lambda (acons) (string= primary-name (car acons)))
                                                   pertainsTo-source&target-sense-name-lists))))
                             (when target-sense-names
                               (format outstream
                                   (ecase ss_type
                                     (:adjective "~{  <wn20schema:adjectivePertainsTo rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%")
                                     (:adjectivesatellite "~{  <wn20schema:adjectivePertainsTo rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%")
                                     (:adverb "~{  <wn20schema:adverbPertainsTo rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"))
                                 (sort target-sense-names #'string<))))
                           (let ((target-sense-names
                                  (mapcar #'cdr
                                    (remove-if-not #'(lambda (acons) (string= primary-name (car acons)))
                                                   antonym-source&target-sense-name-lists))))
                             (when target-sense-names
                               (format outstream
                                   (ecase ss_type
                                     (:noun "~{  <wn20schema:antonymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%")
                                     (:verb "~{  <wn20schema:antonymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%")
                                     (:adjective "~{  <wn20schema:antonymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%")
                                     (:adjectivesatellite "~{  <wn20schema:antonymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%")
                                     (:adverb "~{  <wn20schema:antonymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"))
                                 (sort target-sense-names #'string<))))
                           (let ((target-sense-names
                                  (mapcar #'cdr
                                    (remove-if-not #'(lambda (acons) (string= primary-name (car acons)))
                                                   participleof-source&target-sense-name-lists))))
                             (when target-sense-names
                               (format outstream
                                   (ecase ss_type
                                     (:adjective "~{  <wn20schema:participleOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%")
                                     (:adjectivesatellite "~{  <wn20schema:participleOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"))
                                 (sort target-sense-names #'string<))))
                           (let ((frames
                                  (mapcar #'cdr
                                    (remove-if-not #'(lambda (acons) (string= primary-name (car acons)))
                                                   word&frame-list))))
                             (loop for frame in frames
                                 do (format outstream *frame-description-template-body*
                                      (make-frame-sentence primary-name frame))))
                           ;; tail
                           (format outstream
                               (ecase ss_type
                                 (:noun *noun-word-sense-description-tail*)
                                 (:verb *verb-word-sense-description-tail*)
                                 (:adjective *adjective-word-sense-description-tail*)
                                 (:adjectivesatellite *adjectivesatellite-word-sense-description-tail*)
                                 (:adverb *adverb-word-sense-description-tail*)))
                           ;; First synset output
                           (format outstream
                               (ecase ss_type
                                 (:noun *noun-synset-description-temp*)
                                 (:verb *verb-synset-description-temp*)
                                 (:adjective *adjective-synset-description-temp*)
                                 (:adjectivesatellite *adjective-satellite-synset-description-temp*)
                                 (:adverb *adverb-synset-description-temp*))
                             source-synset-name
                             (underscore2space primary-name)
                             (+ offset
                                (ecase pos
                                  (:noun 100000000)
                                  (:verb 200000000)
                                  (:adjective 300000000)
                                  (:adverb 400000000))))
                           (loop for sense-name in (sort (copy-list (cons source-word-sense-name synonymous-word-sense-names))
                                                         #'string<)
                               do (format outstream *synset-description-template-body* sense-name)
                                 )
                           (when hyponym-target-synset-names
                             (format outstream
                                 (ecase pos
                                   (:noun "~{  <wn20schema:hyponymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%")
                                   (:verb "~{  <wn20schema:hyponymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"))
                               (sort hyponym-target-synset-names #'string<)))
                           (when hyponym-instance-target-synset-names
                             (format outstream
                                 (ecase pos
                                   (:noun "~{  <wn21schema:hyponymInstanceOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%")
                                   (:verb "~{  <wn21schema:hyponymInstanceOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"))
                               (sort hyponym-instance-target-synset-names #'string<)))
                           (when hypernym-target-synset-names
                             (format outstream
                                 (ecase pos
                                   (:noun "~{  <wn20schema:hypernymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%")
                                   (:verb "~{  <wn20schema:hypernymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"))
                               (sort hypernym-target-synset-names #'string<)))
                           (when hypernym-instance-target-synset-names
                             (format outstream
                                 (ecase pos
                                   (:noun "~{  <wn21schema:instanceHypernymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%")
                                   (:verb "~{  <wn21schema:instanceHypernymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"))
                               (sort hypernym-instance-target-synset-names #'string<)))
                           (when membermeronym-target-synset-names
                             (format outstream
                                 (ecase pos
                                   (:noun "~{  <wn20schema:memberMeronymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"))
                               (sort membermeronym-target-synset-names #'string<)))
                           (when memberholonym-target-synset-names
                             (format outstream
                                 (ecase pos
                                   (:noun "~{  <wn20schema:memberHolonymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"))
                               (sort memberholonym-target-synset-names #'string<)))
                           (when partmeronym-target-synset-names
                             (format outstream
                                 (ecase pos
                                   (:noun "~{  <wn20schema:partMeronymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"))
                               (sort partmeronym-target-synset-names #'string<)))
                           (when partholonym-target-synset-names
                             (format outstream
                                 (ecase pos
                                   (:noun "~{  <wn20schema:partHolonymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"))
                               (sort partholonym-target-synset-names #'string<)))
                           (when substancemeronym-target-synset-names
                             (format outstream
                                 (ecase pos
                                   (:noun "~{  <wn20schema:substanceMeronymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"))
                               (sort substancemeronym-target-synset-names #'string<)))
                           (when substanceholonym-target-synset-names
                             (format outstream
                                 (ecase pos
                                   (:noun "~{  <wn20schema:substanceHolonymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"))
                               (sort substanceholonym-target-synset-names #'string<)))
                           (when similarity-target-synset-names
                             (format outstream
                                 (ecase ss_type
                                   (:adjective "~{  <wn20schema:similarTo rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%")
                                   (:adjectivesatellite "~{  <wn20schema:similarTo rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"))
                               (sort similarity-target-synset-names #'string<)))
                           (when attribute-target-synset-names
                             (format outstream
                                 (ecase pos
                                   (:noun "~{  <wn20schema:attribute rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%")
                                   (:adjective "~{  <wn20schema:attributeOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"))
                               (sort attribute-target-synset-names #'string<)))
                           (when sameverbgroupas-target-synset-names
                             (format outstream
                                 (ecase pos
                                   (:verb "~{  <wn20schema:sameVerbGroupAs rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"))
                               (sort sameverbgroupas-target-synset-names #'string<)))
                           (when entailment-target-synset-names
                             (format outstream
                                 (ecase pos 
                                   (:verb "~{  <wn20schema:entails rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"))
                               (sort entailment-target-synset-names #'string<)))
                           (when causes-target-synset-names
                             (format outstream
                                 (ecase pos
                                   (:verb "~{  <wn20schema:causes rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"))
                               (sort causes-target-synset-names #'string<)))
                           (when classified-by-topic-target-names
                             (format outstream
                                 "~{  <wn20schema:classifiedByTopic rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"
                               (sort classified-by-topic-target-names #'string<)))
                           (when classified-by-usage-target-names
                             (format outstream
                                 "~{  <wn20schema:classifiedByUsage rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"
                               (sort classified-by-usage-target-names #'string<)))
                           (when classified-by-region-target-names
                             (format outstream
                                 "~{  <wn20schema:classifiedByRegion rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"
                               (sort classified-by-region-target-names #'string<)))
                           (when member-of-this-domain-topic-target-names
                             (format outstream
                                 (ecase pos
                                   (:noun "~{  <wn20schema:classifiesTopic rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"))
                               (sort member-of-this-domain-topic-target-names #'string<)))
                           (when member-of-this-domain-region-target-names
                             (format outstream
                                 (ecase pos
                                   (:noun "~{  <wn20schema:classifiesRegion rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"))
                               (sort member-of-this-domain-region-target-names #'string<)))
                           (when member-of-this-domain-usage-target-names
                             (format outstream
                                 (ecase pos
                                   (:noun "~{  <wn20schema:classifiesUsage rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"))
                               (sort member-of-this-domain-usage-target-names #'string<)))
                           
                           (when gloss-stuff
                             (format outstream "  <wn20schema:gloss>~A</wn20schema:gloss>~%"
                               (escape-gloss-decls (spaced-str (cdr gloss-stuff)))))
                           (format outstream (ecase ss_type
                                               (:noun *noun-synset-description-tail*)
                                               (:verb *verb-synset-description-tail*)
                                               (:adjective *adjective-synset-description-tail*)
                                               (:adjectivesatellite *adjective-satellite-synset-description-tail*)
                                               (:adverb *adverb-synset-description-tail*)))
                           
                           ;; Other word and word sense
                           (loop for (word nil) on (cddr sense-stuff) by #'cddr
                               for sense-name in synonymous-word-sense-names
                               do
                                 (when (eq pos :adjective)
                                   (setq word (get-adj-word word)))
                                 (let ((word-name (make-word-name word)))
                                   (unless (member word-name *outputted-word-list*)
                                     (let ((sense-names
                                            (loop for offset in (offsets-in-index-entry (string-downcase word) pos)
                                                with i = 0
                                                collect (make-sense-name word ss_type (incf i)))))
                                       (format outstream
                                           (if (collocated-p word) *collocation-description-template*
                                             *word-description-template*)
                                         word-name (underscore2space word) sense-names)
                                       (push word-name *outputted-word-list*)))
                                   ;; word sense head
                                   (format outstream 
                                       (ecase ss_type
                                         (:noun *noun-word-sense-description-temp*)
                                         (:verb *verb-word-sense-description-temp*)
                                         (:adjective *adjective-word-sense-description-temp*)
                                         (:adjectivesatellite *adjectivesatellite-word-sense-description-temp*)
                                         (:adverb *adverb-word-sense-description-temp*))
                                     sense-name (underscore2space word)
                                     (or (get-tagcount-from-index-sense (string-downcase word)
                                                                        ss_type
                                                                        (get-synset-number-in-index-entry
                                                                         (string-downcase word) offset pos))
                                         0)
                                     word-name
                                     source-synset-name)
                                   ;; body
                                   (let ((target-sense-names
                                          (mapcar #'cdr
                                            (remove-if-not #'(lambda (acons) (string= word (car acons)))
                                                           derivationallyRelated-source&target-sense-name-lists))))
                                     (when target-sense-names
                                       (format outstream
                                           (ecase pos
                                             (:noun "~{  <wn20schema:derivationallyRelated rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%")
                                             (:verb "~{  <wn20schema:derivationallyRelated rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%")
                                             ;; wn21
                                             (:adjective "~{  <wn20schema:derivationallyRelated rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%")
                                             (:adverb "~{  <wn20schema:derivationallyRelated rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"))
                                         (sort target-sense-names #'string<))))
                                   (let ((target-sense-names
                                          (mapcar #'cdr
                                            (remove-if-not #'(lambda (acons) (string= word (car acons)))
                                                           pertainsTo-source&target-sense-name-lists))))
                                     (when target-sense-names
                                       (format outstream
                                           (ecase ss_type
                                             (:adjective "~{  <wn20schema:adjectivePertainsTo rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%")
                                             (:adjectivesatellite "~{  <wn20schema:adjectivePertainsTo rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%")
                                             (:adverb "~{  <wn20schema:adverbPertainsTo rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"))
                                         (sort target-sense-names #'string<))))
                                   (let ((target-sense-names
                                          (mapcar #'cdr
                                            (remove-if-not #'(lambda (acons) (string= word (car acons)))
                                                           antonym-source&target-sense-name-lists))))
                                     (when target-sense-names
                                       (format outstream
                                           (ecase ss_type
                                             (:noun "~{  <wn20schema:antonymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%")
                                             (:verb "~{  <wn20schema:antonymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%")
                                             (:adjective "~{  <wn20schema:antonymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%")
                                             (:adjectivesatellite "~{  <wn20schema:antonymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%")
                                             (:adverb "~{  <wn20schema:antonymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"))
                                         (sort target-sense-names #'string<))))
                                   (let ((target-sense-names
                                          (mapcar #'cdr
                                            (remove-if-not #'(lambda (acons) (string= word (car acons)))
                                                           participleof-source&target-sense-name-lists))))
                                     (when target-sense-names
                                       (format outstream
                                           (ecase ss_type
                                             (:adjective "~{  <wn20schema:participleOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%")
                                             (:adjectivesatellite "~{  <wn20schema:participleOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}~%"))
                                         (sort target-sense-names #'string<))))
                                   (let ((frames
                                          (mapcar #'cdr
                                            (remove-if-not #'(lambda (acons) (string= word (car acons)))
                                                           word&frame-list))))
                                     (loop for frame in frames
                                         do (format outstream *frame-description-template-body*
                                              (make-frame-sentence word frame))))
                                   ;; tail
                                   (format outstream
                                       (ecase ss_type
                                         (:noun *noun-word-sense-description-tail*)
                                         (:verb *verb-word-sense-description-tail*)
                                         (:adjective *adjective-word-sense-description-tail*)
                                         (:adjectivesatellite *adjectivesatellite-word-sense-description-tail*)
                                         (:adverb *adverb-word-sense-description-tail*))))))))))))))))

