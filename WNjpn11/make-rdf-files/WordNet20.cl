;;;-*- Mode: common-lisp; syntax: common-lisp; package: wn; base: 10 -*-
;;;
;;;; WordNet Dictionary Information Retrieval and OWL Conversion Program
;;; This program is applied to Allegro8.1, 8.2, 9.0 Modern, sbcl, and WordNet.
;;;
;;;
;;; History
;;; -------
;;; 2011.10.21    Independent parts of WordNet20 from SWCLOS are extracted here.
;;; 2009.04.26    Revised for SWCLOS2 and updated.
;;; 2005.01.21    File obtained from Ruml's web site.
;;;

(in-package :wn)

(setf *wn-package* (find-package :wn20)) ; defined at WordNet.cl

;;;;;;;;;;;;;;; globals ;;;;;;;;;;;

(defparameter *wnhome*
    (or #+:excl (sys:getenv "WNHOME")             ; it is registered in system but commented for 2.1 and 3.0
        #+:sbcl (sb-ext:posix-getenv "WNHOME")
        #-:unix "C:\\WordNet\\WordNet-2.0"
        #+:unix "/usr/local/wordnet/WordNet-2.0"))
(setq *wnsearchdir*
      #-:unix (str (or #+:excl (sys:getenv "WNSEARCHDIR")
                       #+:sbcl (sb-ext:posix-getenv "WNSEARCHDIR")
                       (str *wnhome* "\\dict"))
                   "\\")
      #+:unix (str (or #+:excl (sys:getenv "WNSEARCHDIR")
                       #+:sbcl (sb-ext:posix-getenv "WNSEARCHDIR")
                       (str *wnhome* "/dict"))
                   "/")
    )

;;;
;;;;
;;;

(defparameter *xml-decl* "<?xml version='1.0' encoding='UTF-8'?>~%")
(defparameter *doc-decl* "<!DOCTYPE rdf:RDF [
    <!ENTITY rdf 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'>
    <!ENTITY rdfs 'http://www.w3.org/2000/01/rdf-schema#'>
    <!ENTITY wn20instances 'http://www.w3.org/2006/03/wn/wn20/instances/'>
    <!ENTITY wn20schema 'http://www.w3.org/2006/03/wn/wn20/schema/'>
]>~%~%")
(defparameter *rdf-decl-prolog*
"<rdf:RDF
    xmlns:rdf=\"&rdf;\"
    xmlns:rdfs=\"&rdfs;\"
    xmlns:wn20instances=\"&wn20instances;\"
    xmlns:wn20schema=\"&wn20schema;\"
    xml:lang=\"en-US\">~%")
(defparameter *rdf-decl-epilog* "</rdf:RDF>~%")

(defparameter *noun-synset-description-template* "<wn20schema:NounSynset rdf:about=\"&wn20instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:synsetId>~D</wn20schema:synsetId>
</wn20schema:NounSynset>~%~%")
(defparameter *verb-synset-description-template* "<wn20schema:VerbSynset rdf:about=\"&wn20instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:synsetId>~D</wn20schema:synsetId>
</wn20schema:VerbSynset>~%~%")
(defparameter *adjective-synset-description-template* "<wn20schema:AdjectiveSynset rdf:about=\"&wn20instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:synsetId>~D</wn20schema:synsetId>
</wn20schema:AdjectiveSynset>~%~%")
(defparameter *adjective-satellite-synset-description-template* "<wn20schema:AdjectiveSatelliteSynset rdf:about=\"&wn20instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:synsetId>~D</wn20schema:synsetId>
</wn20schema:AdjectiveSatelliteSynset>~%~%")
(defparameter *adverb-synset-description-template* "<wn20schema:AdverbSynset rdf:about=\"&wn20instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:synsetId>~D</wn20schema:synsetId>
</wn20schema:AdverbSynset>~%~%")

(defparameter *noun-synset-description-temp* 
"<wn20schema:NounSynset rdf:about=\"&~A;~A\"
    rdfs:label=\"~A\">
  <wn20schema:synsetId>~D</wn20schema:synsetId>~%")
(defparameter *noun-synset-description-tail* "</wn20schema:NounSynset>~%~%")
(defparameter *verb-synset-description-temp* 
"<wn20schema:VerbSynset rdf:about=\"&~A;~A\"
    rdfs:label=\"~A\">
  <wn20schema:synsetId>~D</wn20schema:synsetId>~%")
(defparameter *verb-synset-description-tail* "</wn20schema:VerbSynset>~%~%")
(defparameter *adjective-synset-description-temp* 
"<wn20schema:AdjectiveSynset rdf:about=\"&~A;~A\"
    rdfs:label=\"~A\">
  <wn20schema:synsetId>~D</wn20schema:synsetId>~%")
(defparameter *adjective-synset-description-tail* "</wn20schema:AdjectiveSynset>~%~%")
(defparameter *adjective-satellite-synset-description-temp* 
"<wn20schema:AdjectiveSatelliteSynset rdf:about=\"&~A;~A\"
    rdfs:label=\"~A\">
  <wn20schema:synsetId>~D</wn20schema:synsetId>~%")
(defparameter *adjective-satellite-synset-description-tail* "</wn20schema:AdjectiveSatelliteSynset>~%~%")
(defparameter *adverb-synset-description-temp* 
"<wn20schema:AdverbSynset rdf:about=\"&~A;~A\"
    rdfs:label=\"~A\">
  <wn20schema:synsetId>~D</wn20schema:synsetId>~%")
(defparameter *adverb-synset-description-tail* "</wn20schema:AdverbSynset>~%~%")

(defparameter *noun-hyponym-description-template* "<rdf:Description rdf:about=\"&wn20instances;~A\">
~{  <wn20schema:hyponymOf rdf:resource=\"&wn20instances;~A\"/>~^~%~}
</rdf:Description>~%~%")
(defparameter *verb-hyponym-description-template* "<rdf:Description rdf:about=\"&wn20instances;~A\">
~{  <wn20schema:hyponymOf rdf:resource=\"&wn20instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(defparameter *noun-hypernym-description-template* "<rdf:Description rdf:about=\"&wn20instances;~A\">
~{  <wn20schema:hypernymOf rdf:resource=\"&wn20instances;~A\"/>~^~%~}
</rdf:Description>~%~%")
(defparameter *verb-hypernym-description-template* "<rdf:Description rdf:about=\"&wn20instances;~A\">
~{  <wn20schema:hypernymOf rdf:resource=\"&wn20instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(defparameter *derivationallyRelated-description-template* "<rdf:Description rdf:about=\"&wn20instances;~A\">
~{  <wn20schema:derivationallyRelated rdf:resource=\"&wn20instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(defparameter *memberMeronym-description-template* "<rdf:Description rdf:about=\"&wn20instances;~A\">
~{  <wn20schema:memberMeronymOf rdf:resource=\"&wn20instances;~A\"/>~^~%~}
</rdf:Description>~%~%")
(defparameter *memberHolonym-description-template* "<rdf:Description rdf:about=\"&wn20instances;~A\">
~{  <wn20schema:memberHolonymOf rdf:resource=\"&wn20instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(defparameter *partmeronym-description-template* "<rdf:Description rdf:about=\"&wn20instances;~A\">
~{  <wn20schema:partMeronymOf rdf:resource=\"&wn20instances;~A\"/>~^~%~}
</rdf:Description>~%~%")
(defparameter *partholonym-description-template* "<rdf:Description rdf:about=\"&wn20instances;~A\">
~{  <wn20schema:partHolonymOf rdf:resource=\"&wn20instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(defparameter *substanceMeronym-description-template* "<rdf:Description rdf:about=\"&wn20instances;~A\">
~{  <wn20schema:substanceMeronymOf rdf:resource=\"&wn20instances;~A\"/>~^~%~}
</rdf:Description>~%~%")
(defparameter *substanceHolonym-description-template* "<rdf:Description rdf:about=\"&wn20instances;~A\">
~{  <wn20schema:substanceHolonymOf rdf:resource=\"&wn20instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(defparameter *adjective-pertainsTo-description-template* "<rdf:Description rdf:about=\"&wn20instances;~A\">
~{  <wn20schema:adjectivePertainsTo rdf:resource=\"&wn20instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(defparameter *adverb-pertainsTo-description-template* "<rdf:Description rdf:about=\"&wn20instances;~A\">
~{  <wn20schema:adverbPertainsTo rdf:resource=\"&wn20instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(defparameter *classifiedBy-header-template* "<rdf:Description rdf:about=\"&wn20instances;~A\">")
(defparameter *classifiedByTopic-body-template* 
"~%~{  <wn20schema:classifiedByTopic rdf:resource=\"&wn20instances;~A\"/>~^~%~}")
(defparameter *classifiedByUsage-body-template* 
"~%~{  <wn20schema:classifiedByUsage rdf:resource=\"&wn20instances;~A\"/>~^~%~}")
(defparameter *classifiedByRegion-body-template* 
"~%~{  <wn20schema:classifiedByRegion rdf:resource=\"&wn20instances;~A\"/>~^~%~}")
(defparameter *classifiedBy-tail-template*
"~%</rdf:Description>~%~%")

(defparameter *similarity-description-template* "<rdf:Description rdf:about=\"&wn20instances;~A\">
~{  <wn20schema:similarTo rdf:resource=\"&wn20instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(defparameter *antonym-description-template* "<rdf:Description rdf:about=\"&wn20instances;~A\">
~{  <wn20schema:antonymOf rdf:resource=\"&wn20instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(defparameter *sameverbgroupas-description-template* "<rdf:Description rdf:about=\"&wn20instances;~A\">
~{  <wn20schema:sameVerbGroupAs rdf:resource=\"&wn20instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(defparameter *entailment-description-template* "<rdf:Description rdf:about=\"&wn20instances;~A\">
~{  <wn20schema:entails rdf:resource=\"&wn20instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(defparameter *causes-description-template* "<rdf:Description rdf:about=\"&wn20instances;~A\">
~{  <wn20schema:causes rdf:resource=\"&wn20instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(defparameter *participleof-description-template* "<rdf:Description rdf:about=\"&wn20instances;~A\">
~{  <wn20schema:participleOf rdf:resource=\"&wn20instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(defparameter *attribute-description-template* "<rdf:Description rdf:about=\"&wn20instances;~A\">
~{  <wn20schema:attribute rdf:resource=\"&wn20instances;~A\"/>~^~%~}
</rdf:Description>~%~%")
(defparameter *attributeOf-description-template* "<rdf:Description rdf:about=\"&wn20instances;~A\">
~{  <wn20schema:attributeOf rdf:resource=\"&wn20instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(defparameter *seealso-description-template* "<rdf:Description rdf:about=\"&wn20instances;~A\">
~{  <wn20schema:seeAlso rdf:resource=\"&wn20instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(defparameter *gloss-description-template* "<rdf:Description rdf:about=\"&wn20instances;~A\">
  <wn20schema:gloss>~A</wn20schema:gloss>
</rdf:Description>~%~%")

(defparameter *frame-description-template-head* "<rdf:Description rdf:about=\"&wn20instances;~A\">~%")
(defparameter *frame-description-template-body* " <wn20schema:frame>~A</wn20schema:frame>~%")
(defparameter *frame-description-template-tail* "</rdf:Description>~%~%")

(defparameter *word-description-template* 
"<wn20schema:Word rdf:about=\"&wn20instances;~A\"
    wn20schema:lexicalForm=\"~A\">
~{  <wn20schema:sense rdf:resource=\"&wn20instances;~A\"/>~^~%~}
</wn20schema:Word>~%~%")
(defparameter *collocation-description-template* "<wn20schema:Collocation rdf:about=\"&wn20instances;~A\"
    wn20schema:lexicalForm=\"~A\">
~{  <wn20schema:sense rdf:resource=\"&wn20instances;~A\"/>~^~%~}
</wn20schema:Collocation>~%~%")

(defparameter *noun-word-sense-description-template* 
"<wn20schema:NounWordSense rdf:about=\"&wn20instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:inSynset rdf:resource=\"&wn20instances;~A\"/>
  <wn20schema:word rdf:resource=\"&wn20instances;~A\"/>
</wn20schema:NounWordSense>~%~%" )
(defparameter *verb-word-sense-description-template* 
"<wn20schema:VerbWordSense rdf:about=\"&wn20instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:inSynset rdf:resource=\"&wn20instances;~A\"/>
  <wn20schema:word rdf:resource=\"&wn20instances;~A\"/>
</wn20schema:VerbWordSense>~%~%" )
(defparameter *adjective-word-sense-description-template* 
"<wn20schema:AdjectiveWordSense rdf:about=\"&wn20instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:inSynset rdf:resource=\"&wn20instances;~A\"/>
  <wn20schema:word rdf:resource=\"&wn20instances;~A\"/>
</wn20schema:AdjectiveWordSense>~%~%" )
(defparameter *adjectivesatellite-word-sense-description-template* 
"<wn20schema:AdjectiveSatelliteWordSense rdf:about=\"&wn20instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:word rdf:resource=\"&wn20instances;~A\"/>
  <wn20schema:inSynset rdf:resource=\"&wn20instances;~A\"/>
</wn20schema:AdjectiveSatelliteWordSense>~%~%" )
(defparameter *adverb-word-sense-description-template* 
"<wn20schema:AdverbWordSense rdf:about=\"&wn20instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:inSynset rdf:resource=\"&wn20instances;~A\"/>
  <wn20schema:word rdf:resource=\"&wn20instances;~A\"/>
</wn20schema:AdverbWordSense>~%~%" )

(defparameter *noun-word-sense-description-temp* 
"<wn20schema:NounWordSense rdf:about=\"&~A;~A\"
    rdfs:label=\"~A\">
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:word rdf:resource=\"&~A;~A\"/>
  <wn20schema:inSynset rdf:resource=\"&~A;~A\"/>~%" )
(defparameter *verb-word-sense-description-temp* 
"<wn20schema:VerbWordSense rdf:about=\"&~A;~A\"
    rdfs:label=\"~A\">
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:word rdf:resource=\"&~A;~A\"/>
  <wn20schema:inSynset rdf:resource=\"&~A;~A\"/>~%" )
(defparameter *adjective-word-sense-description-temp* 
"<wn20schema:AdjectiveWordSense rdf:about=\"&~A;~A\"
    rdfs:label=\"~A\">
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:word rdf:resource=\"&~A;~A\"/>
  <wn20schema:inSynset rdf:resource=\"&~A;~A\"/>~%" )
(defparameter *adjectivesatellite-word-sense-description-temp* 
"<wn20schema:AdjectiveSatelliteWordSense rdf:about=\"&~A;~A\"
    rdfs:label=\"~A\">
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:word rdf:resource=\"&~A;~A\"/>
  <wn20schema:inSynset rdf:resource=\"&~A;~A\"/>~%" )
(defparameter *adverb-word-sense-description-temp* 
"<wn20schema:AdverbWordSense rdf:about=\"&~A;~A\"
    rdfs:label=\"~A\">
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:word rdf:resource=\"&~A;~A\"/>
  <wn20schema:inSynset rdf:resource=\"&~A;~A\"/>~%" )
(defparameter *noun-word-sense-description-tail* "</wn20schema:NounWordSense>~%~%" )
(defparameter *verb-word-sense-description-tail* "</wn20schema:VerbWordSense>~%~%" )
(defparameter *adjective-word-sense-description-tail* "</wn20schema:AdjectiveWordSense>~%~%" )
(defparameter *adjectivesatellite-word-sense-description-tail* "</wn20schema:AdjectiveSatelliteWordSense>~%~%" )
(defparameter *adverb-word-sense-description-tail* "</wn20schema:AdverbWordSense>~%~%" )

(defparameter *synset-description-template-head* "<rdf:Description rdf:about=\"&wn20instances;~A\">~%")
(defparameter *synset-description-template-body* "  <wn20schema:containsWordSense rdf:resource=\"&~A;~A\"/>~%")
(defparameter *synset-description-template-tail* "</rdf:Description>~%~%" )

(defvar *synset-file-path* "WN20:wordnet-synset.rdf")
(defvar *hyponym-file-path* "WN20:wordnet-hyponym.rdf")
(defvar *hypernym-file-path* "WN20:wordnet-hypernym.rdf")
(defvar *derivationallyRelated-file-path* "WN20:wordnet-derivationallyrelated.rdf")
(defvar *membermeronym-file-path* "WN20:wordnet-membermeronym.rdf")
(defvar *memberholonym-file-path* "WN20:wordnet-membeholonym.rdf")
(defvar *partmeronym-file-path* "WN20:wordnet-partmeronym.rdf")
(defvar *partholonym-file-path* "WN20:wordnet-partholonym.rdf")
(defvar *substancemeronym-file-path* "WN20:wordnet-substancemeronym.rdf")
(defvar *substanceholonym-file-path* "WN20:wordnet-substanceholonym.rdf")
(defvar *pertainsTo-file-path* "WN20:wordnet-pertainsto.rdf")
(defvar *classifiedby-file-path* "WN20:wordnet-classifiedby.rdf")
(defvar *similarity-file-path* "WN20:wordnet-similarity.rdf")
(defvar *antonym-file-path* "WN20:wordnet-antonym.rdf")
(defvar *entailment-file-path* "WN20:wordnet-entailment.rdf")
(defvar *causes-file-path* "WN20:wordnet-causes.rdf")
(defvar *participleof-file-path* "WN20:wordnet-participleof.rdf")
(defvar *sameverbgroupas-file-path* "WN20:wordnet-sameverbgroupas.rdf")
(defvar *attribute-file-path* "WN20:wordnet-attribute.rdf")
(defvar *seealso-file-path* "WN20:wordnet-seealso.rdf")
(defvar *glossary-file-path* "WN20:wordnet-glossary.rdf")
(defvar *frame-file-path* "WN20:wordnet-frame.rdf")
(defvar *wordsenseandwords-file-path* "WN20:wordnet-wordsenseandwords.rdf")
(defvar *one-big-rdf-file-noun-path* "WN20:wordnet-noun.rdf")
(defvar *one-big-rdf-file-verb-path* "WN20:wordnet-verb.rdf")
(defvar *one-big-rdf-file-adjective-path* "WN20:wordnet-adjective.rdf")
(defvar *one-big-rdf-file-adverb-path* "WN20:wordnet-adverb.rdf")
(defvar *one-big-rdf-file-path* (merge-pathnames "wnjpn.rdf" (user-homedir-pathname)))
;;; EOF

;;;
;;; Output utils
;;;

(defun %%word-sense-output (outstream ss_type
                                      subjective-word-sense-name
                                      primary-name tagcount subjective-synset-name
                                      word-name)
  (format outstream
      (ecase ss_type
        (:noun *noun-word-sense-description-template*)
        (:verb *verb-word-sense-description-template*)
        (:adjective *adjective-word-sense-description-template*)
        (:adjectivesatellite *adjectivesatellite-word-sense-description-template*)
        (:adverb *adverb-word-sense-description-template*))
    (string-downcase (package-name (symbol-package subjective-word-sense-name)))
    subjective-word-sense-name
    (underscore2space primary-name)
    (or tagcount 0)
    (string-downcase (package-name (symbol-package subjective-synset-name)))
    subjective-synset-name
    (string-downcase (package-name (symbol-package word-name))) word-name)
  )

(defun %%word-description-output (outstream word-name word senses)
  (format outstream
      (if (collocated-p word) *collocation-description-template* *word-description-template*)
    (string-downcase (package-name (symbol-package word-name)))
    word-name
    (underscore2space word)
    (mapcan #'(lambda (sense) (list (string-downcase (package-name (symbol-package sense)))
                                    sense))
      senses)))

(defun %%word-sense-header-output (outstream ss_type sense-name word tagcount word-name synset-name)
  (format outstream
      (ecase ss_type
        (:noun *noun-word-sense-description-temp*)
        (:verb *verb-word-sense-description-temp*)
        (:adjective *adjective-word-sense-description-temp*)
        (:adjectivesatellite *adjectivesatellite-word-sense-description-temp*)
        (:adverb *adverb-word-sense-description-temp*))
    (string-downcase (package-name (symbol-package sense-name)))
    sense-name
    (underscore2space word)
    (or tagcount 0)
    (string-downcase (package-name (symbol-package word-name))) word-name
    (string-downcase (package-name (symbol-package synset-name))) synset-name))

(defun %%word-sense-body-output (outstream ss_type word
                                           derivationallyRelated-source&target-sense-name-lists
                                           pertainsTo-source&target-sense-name-lists
                                           antonym-source&target-sense-name-lists
                                           participleof-source&target-sense-name-lists
                                           word&frame-list)
  (let (target-sense-names)
    (when (setq target-sense-names
                (mapcar #'cdr
                  (remove-if-not #'(lambda (acons) (string= word (car acons)))
                                 derivationallyRelated-source&target-sense-name-lists)))
      (format outstream
          (ecase ss_type
            (:noun "~{  <wn20schema:derivationallyRelated rdf:resource=\"&~A;~A\"/>~^~%~}~%")
            (:verb "~{  <wn20schema:derivationallyRelated rdf:resource=\"&~A;~A\"/>~^~%~}~%")
            ;; wn21
            (:adjective "~{  <wn20schema:derivationallyRelated rdf:resource=\"&~A;~A\"/>~^~%~}~%")
            (:adjectivesatellite "~{  <wn20schema:derivationallyRelated rdf:resource=\"&~A;~A\"/>~^~%~}~%")
            (:adverb "~{  <wn20schema:derivationallyRelated rdf:resource=\"&~A;~A\"/>~^~%~}~%"))
        (mapcan #'(lambda (sense) (list (string-downcase (package-name (symbol-package sense))) sense))
          (sort target-sense-names #'string<))))
    (when (setq target-sense-names
                (mapcar #'cdr
                  (remove-if-not #'(lambda (acons) (string= word (car acons)))
                                 pertainsTo-source&target-sense-name-lists)))
      (format outstream
          (ecase ss_type
            (:adjective "~{  <wn20schema:adjectivePertainsTo rdf:resource=\"&~A;~A\"/>~^~%~}~%")
            (:adjectivesatellite "~{  <wn20schema:adjectivePertainsTo rdf:resource=\"&~A;~A\"/>~^~%~}~%")
            (:adverb "~{  <wn20schema:adverbPertainsTo rdf:resource=\"&~A;~A\"/>~^~%~}~%"))
        (mapcan #'(lambda (sense) (list (string-downcase (package-name (symbol-package sense))) sense))
          (sort target-sense-names #'string<))))
    (when (setq target-sense-names
                (mapcar #'cdr
                  (remove-if-not #'(lambda (acons) (string= word (car acons)))
                                 antonym-source&target-sense-name-lists)))
      (format outstream
          (ecase ss_type
            (:noun "~{  <wn20schema:antonymOf rdf:resource=\"&~A;~A\"/>~^~%~}~%")
            (:verb "~{  <wn20schema:antonymOf rdf:resource=\"&~A;~A\"/>~^~%~}~%")
            (:adjective "~{  <wn20schema:antonymOf rdf:resource=\"&~A;~A\"/>~^~%~}~%")
            (:adjectivesatellite "~{  <wn20schema:antonymOf rdf:resource=\"&~A;~A\"/>~^~%~}~%")
            (:adverb "~{  <wn20schema:antonymOf rdf:resource=\"&~A;~A\"/>~^~%~}~%"))
        (mapcan #'(lambda (sense) (list (string-downcase (package-name (symbol-package sense))) sense))
          (sort target-sense-names #'string<))))
    (when (setq target-sense-names
                (mapcar #'cdr
                  (remove-if-not #'(lambda (acons) (string= word (car acons)))
                                 participleof-source&target-sense-name-lists)))
      (format outstream
          (ecase ss_type
            (:adjective "~{  <wn20schema:participleOf rdf:resource=\"&~A;~A\"/>~^~%~}~%")
            (:adjectivesatellite "~{  <wn20schema:participleOf rdf:resource=\"&~A;~A\"/>~^~%~}~%"))
        (mapcan #'(lambda (sense) (list (string-downcase (package-name (symbol-package sense))) sense))
          (sort target-sense-names #'string<))))
    (loop for frame in (mapcar #'cdr
                         (remove-if-not #'(lambda (acons) (string= word (car acons)))
                                        word&frame-list))
        do (format outstream *frame-description-template-body*
             (make-frame-sentence word frame)))))

(defun %%synset-header-output (outstream ss_type synset-name word offset)
  (format outstream
      (ecase ss_type
        (:noun *noun-synset-description-temp*)
        (:verb *verb-synset-description-temp*)
        (:adjective *adjective-synset-description-temp*)
        (:adjectivesatellite *adjective-satellite-synset-description-temp*)
        (:adverb *adverb-synset-description-temp*))
    (string-downcase (package-name (symbol-package synset-name)))
    synset-name
    (underscore2space word)
    (+ offset
       (ecase ss_type
         (:noun 100000000)
         (:verb 200000000)
         (:adjective 300000000)
         (:adjectivesatellite 300000000)
         (:adverb 400000000)))))

(defun %%synset-body-output (outstream subjective-word-sense-name synonymous-word-sense-names)
  (loop for sense-name in (sort (copy-list (cons subjective-word-sense-name synonymous-word-sense-names))
                                #'string<)
      do (format outstream *synset-description-template-body*
           (string-downcase (package-name (symbol-package sense-name))) sense-name)))

(defun make-sense-names (word pos)
  (ecase pos ((:noun :verb :adjective :adverb) t))
  (loop for ofs in (offsets-in-index-entry (string-downcase word) pos) with i = 0
      when (and (incf i)
                (some #'(lambda (w) (string= w word)) (get-synonymous-words-from ofs pos)))
      collect (make-sense-name word
                               (if (eq pos :adjective)
                                   (get-ss_type-for-offset ofs :adjective)
                                 pos)
                               i)))

