;;;-*- Mode: common-lisp; syntax: common-lisp; package: wn; base: 10 -*-
;;;
;;; This program is applied to WordNet3.0 and Japanese WordNet 1.1.
;;;
;;;
;;; History
;;; -------
;;; 2011.08.05    Copied and modified from WordNet30
;;;

(in-package :wn)

(setf *wnja-package* (find-package :wnja11))

;;;;;;;;;;;;;;; globals ;;;;;;;;;;;
;;; Note that this is English Wordnet data.
;;; See also wnjpn.cl file.
(eval-when (:execute :load-toplevel :compile-toplevel)
(setq *wnhome*
    (or #+:excl (sys:getenv "WNHOME")             ; it is registered in system but commented for 2.1 and 3.0
        #+:sbcl (sb-ext:posix-getenv "WNHOME")
        #-:unix "C:\\WordNet\\WordNet-3.0"
        #+:unix "/usr/local/wordnet/WordNet-3.0"))
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
)

;;;
;;;;
;;;
(defun make-jpn-sense-name (offset jpn-word pos &optional (package *wn-package*))
  "this <jpn-word> is retrieved by <offset> from wnjpn.db."
  (let ((synsetid (wnjpn:get-jpn-synsetid offset jpn-word pos)))
    (intern (concatenate 'cl:string "wordsense-"
              (iri-escape-for-lexform jpn-word) "-" (string-downcase (string pos)) "-" (format cl:nil "~d" synsetid))
            (find-instance-package package))))

(setq *doc-decl* "<!DOCTYPE rdf:RDF [
    <!ENTITY rdf 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'>
    <!ENTITY rdfs 'http://www.w3.org/2000/01/rdf-schema#'>
    <!ENTITY wn20instances 'http://www.w3.org/2006/03/wn/wn20/instances/'>
    <!ENTITY wn20schema 'http://www.w3.org/2006/03/wn/wn20/schema/'>
    <!ENTITY wn30instances 'http://www.w3.org/2006/03/wn/wn30/instances/'>
    <!ENTITY wnja11instances 'http://wordnet.jp/ja11/instances/'>
    <!ENTITY wn21schema 'http://www.w3.org/2006/03/wn/wn21/schema/'>
]>~%~%")
(setq *rdf-decl-prolog*
"<rdf:RDF
    xmlns:rdf=\"&rdf;\"
    xmlns:rdfs=\"&rdfs;\"
    xmlns:wn20instances=\"&wn20instances;\"
    xmlns:wn20schema=\"&wn20schema;\"
    xmlns:wn30instances=\"&wn30instances;\"
    xmlns:wnja11instances=\"&wnja11instances;\"
    xmlns:wn21schema=\"&wn21schema;\"
    xml:lang=\"en-US\">~%")

(setq *noun-synset-description-template* "<wn20schema:NounSynset rdf:about=\"&wnja11instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:synsetId>~D</wn20schema:synsetId>
</wn20schema:NounSynset>~%~%")
(setq *verb-synset-description-template* "<wn20schema:VerbSynset rdf:about=\"&wnja11instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:synsetId>~D</wn20schema:synsetId>
</wn20schema:VerbSynset>~%~%")
(setq *adjective-synset-description-template* "<wn20schema:AdjectiveSynset rdf:about=\"&wnja11instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:synsetId>~D</wn20schema:synsetId>
</wn20schema:AdjectiveSynset>~%~%")
(setq *adjective-satellite-synset-description-template* "<wn20schema:AdjectiveSatelliteSynset rdf:about=\"&wnja11instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:synsetId>~D</wn20schema:synsetId>
</wn20schema:AdjectiveSatelliteSynset>~%~%")
(setq *adverb-synset-description-template* "<wn20schema:AdverbSynset rdf:about=\"&wnja11instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:synsetId>~D</wn20schema:synsetId>
</wn20schema:AdverbSynset>~%~%")


(setq *noun-hyponym-description-template* "<rdf:Description rdf:about=\"&wnja11instances;~A\">
~{  <wn20schema:hyponymOf rdf:resource=\"&wnja11instances;~A\"/>~^~%~}
</rdf:Description>~%~%")
(setq *verb-hyponym-description-template* "<rdf:Description rdf:about=\"&wnja11instances;~A\">
~{  <wn20schema:hyponymOf rdf:resource=\"&wnja11instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *noun-hypernym-description-template* "<rdf:Description rdf:about=\"&wnja11instances;~A\">
~{  <wn20schema:hypernymOf rdf:resource=\"&wnja11instances;~A\"/>~^~%~}
</rdf:Description>~%~%")
(setq *verb-hypernym-description-template* "<rdf:Description rdf:about=\"&wnja11instances;~A\">
~{  <wn20schema:hypernymOf rdf:resource=\"&wnja11instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *derivationallyRelated-description-template* "<rdf:Description rdf:about=\"&wnja11instances;~A\">
~{  <wn20schema:derivationallyRelated rdf:resource=\"&wnja11instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *memberMeronym-description-template* "<rdf:Description rdf:about=\"&wnja11instances;~A\">
~{  <wn20schema:memberMeronymOf rdf:resource=\"&wnja11instances;~A\"/>~^~%~}
</rdf:Description>~%~%")
(setq *memberHolonym-description-template* "<rdf:Description rdf:about=\"&wnja11instances;~A\">
~{  <wn20schema:memberHolonymOf rdf:resource=\"&wnja11instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *partmeronym-description-template* "<rdf:Description rdf:about=\"&wnja11instances;~A\">
~{  <wn20schema:partMeronymOf rdf:resource=\"&wnja11instances;~A\"/>~^~%~}
</rdf:Description>~%~%")
(setq *partholonym-description-template* "<rdf:Description rdf:about=\"&wnja11instances;~A\">
~{  <wn20schema:partHolonymOf rdf:resource=\"&wnja11instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *substanceMeronym-description-template* "<rdf:Description rdf:about=\"&wnja11instances;~A\">
~{  <wn20schema:substanceMeronymOf rdf:resource=\"&wnja11instances;~A\"/>~^~%~}
</rdf:Description>~%~%")
(setq *substanceHolonym-description-template* "<rdf:Description rdf:about=\"&wnja11instances;~A\">
~{  <wn20schema:substanceHolonymOf rdf:resource=\"&wnja11instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *adjective-pertainsTo-description-template* "<rdf:Description rdf:about=\"&wnja11instances;~A\">
~{  <wn20schema:adjectivePertainsTo rdf:resource=\"&wnja11instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *adverb-pertainsTo-description-template* "<rdf:Description rdf:about=\"&wnja11instances;~A\">
~{  <wn20schema:adverbPertainsTo rdf:resource=\"&wnja11instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *classifiedBy-header-template* "<rdf:Description rdf:about=\"&wnja11instances;~A\">")
(setq *classifiedByTopic-body-template* 
"~%~{  <wn20schema:classifiedByTopic rdf:resource=\"&wnja11instances;~A\"/>~^~%~}")
(setq *classifiedByUsage-body-template* 
"~%~{  <wn20schema:classifiedByUsage rdf:resource=\"&wnja11instances;~A\"/>~^~%~}")
(setq *classifiedByRegion-body-template* 
"~%~{  <wn20schema:classifiedByRegion rdf:resource=\"&wnja11instances;~A\"/>~^~%~}")



(setq *similarity-description-template* "<rdf:Description rdf:about=\"&wnja11instances;~A\">
~{  <wn20schema:similarTo rdf:resource=\"&wnja11instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *antonym-description-template* "<rdf:Description rdf:about=\"&wnja11instances;~A\">
~{  <wn20schema:antonymOf rdf:resource=\"&wnja11instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *sameverbgroupas-description-template* "<rdf:Description rdf:about=\"&wnja11instances;~A\">
~{  <wn20schema:sameVerbGroupAs rdf:resource=\"&wnja11instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *entailment-description-template* "<rdf:Description rdf:about=\"&wnja11instances;~A\">
~{  <wn20schema:entails rdf:resource=\"&wnja11instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *causes-description-template* "<rdf:Description rdf:about=\"&wnja11instances;~A\">
~{  <wn20schema:causes rdf:resource=\"&wnja11instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *participleof-description-template* "<rdf:Description rdf:about=\"&wnja11instances;~A\">
~{  <wn20schema:participleOf rdf:resource=\"&wnja11instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *attribute-description-template* "<rdf:Description rdf:about=\"&wnja11instances;~A\">
~{  <wn20schema:attribute rdf:resource=\"&wnja11instances;~A\"/>~^~%~}
</rdf:Description>~%~%")
(setq *attributeOf-description-template* "<rdf:Description rdf:about=\"&wnja11instances;~A\">
~{  <wn20schema:attributeOf rdf:resource=\"&wnja11instances;~A\"/>~^~%~}
</rdf:Description>~%~%")


(setq *seealso-description-template* "<rdf:Description rdf:about=\"&wnja11instances;~A\">
~{  <wn20schema:seeAlso rdf:resource=\"&wnja11instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *gloss-description-template* "<rdf:Description rdf:about=\"&wnja11instances;~A\">
  <wn20schema:gloss>~A</wn20schema:gloss>
</rdf:Description>~%~%")

(setq *word-description-template* "<wn20schema:Word rdf:about=\"&wnja11instances;~A\"
    wn20schema:lexicalForm=\"~A\">
~{  <wn20schema:sense rdf:resource=\"&wnja11instances;~A\"/>~^~%~}
</wn20schema:Word>~%~%")
(defparameter *word-description-template-sameAs* "<wn20schema:Word rdf:about=\"&wnja11instances;~A\"
    wn20schema:lexicalForm=\"~A\">
  <owl:sameAs rdf:resource=\"&wn30instances;~A\"/>
~{  <wn20schema:sense rdf:resource=\"&wnja11instances;~A\"/>~^~%~}
</wn20schema:Word>~%~%")
(setq *collocation-description-template* "<wn20schema:Collocation rdf:about=\"&wnja11instances;~A\"
    wn20schema:lexicalForm=\"~A\">
~{  <wn20schema:sense rdf:resource=\"&wnja11instances;~A\"/>~^~%~}
</wn20schema:Collocation>~%~%")
(defparameter *collocation-description-template-sameAs* "<wn20schema:Collocation rdf:about=\"&wnja11instances;~A\"
    wn20schema:lexicalForm=\"~A\">
  <owl:sameAs rdf:resource=\"&wn30instances;~A\"/>
~{  <wn20schema:sense rdf:resource=\"&wnja11instances;~A\"/>~^~%~}
</wn20schema:Collocation>~%~%")

(setq *synset-file-path* "WNJA11:wordnet-synset.rdf")
(setq *hyponym-file-path* "WNJA11:wordnet-hyponym.rdf")
(setq *hypernym-file-path* "WNJA11:wordnet-hypernym.rdf")
(setq *derivationallyRelated-file-path* "WNJA11:wordnet-derivationallyrelated.rdf")
(setq *membermeronym-file-path* "WNJA11:wordnet-membermeronym.rdf")
(setq *memberholonym-file-path* "WNJA11:wordnet-membeholonym.rdf")
(setq *partmeronym-file-path* "WNJA11:wordnet-partmeronym.rdf")
(setq *partholonym-file-path* "WNJA11:wordnet-partholonym.rdf")
(setq *substancemeronym-file-path* "WNJA11:wordnet-substancemeronym.rdf")
(setq *substanceholonym-file-path* "WNJA11:wordnet-substanceholonym.rdf")
(setq *pertainsTo-file-path* "WNJA11:wordnet-pertainsto.rdf")
(setq *classifiedby-file-path* "WNJA11:wordnet-classifiedby.rdf")
(setq *similarity-file-path* "WNJA11:wordnet-similarity.rdf")
(setq *antonym-file-path* "WNJA11:wordnet-antonym.rdf")
(setq *entailment-file-path* "WNJA11:wordnet-entailment.rdf")
(setq *causes-file-path* "WNJA11:wordnet-causes.rdf")
(setq *participleof-file-path* "WNJA11:wordnet-participleof.rdf")
(setq *sameverbgroupas-file-path* "WNJA11:wordnet-sameverbgroupas.rdf")
(setq *attribute-file-path* "WNJA11:wordnet-attribute.rdf")
(setq *seealso-file-path* "WNJA11:wordnet-seealso.rdf")
(setq *glossary-file-path* "WNJA11:wordnet-glossary.rdf")
(setq *frame-file-path* "WNJA11:wordnet-frame.rdf")
(setq *wordsenseandwords-file-path* "WNJA11:wordnet-wordsenseandwords.rdf")
(setq *one-big-rdf-file-noun-path* "WNJA11:wordnet-noun.rdf")
(setq *one-big-rdf-file-verb-path* "WNJA11:wordnet-verb.rdf")
(setq *one-big-rdf-file-adjective-path* "WNJA11:wordnet-adjective.rdf")
(setq *one-big-rdf-file-adverb-path* "WNJA11:wordnet-adverb.rdf")
(setq *one-big-rdf-file-path* (merge-pathnames "wnjpn.rdf" (user-homedir-pathname)))

(setq *noun-word-sense-description-temp* 
"<wn20schema:NounWordSense rdf:about=\"&wnja11instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:word rdf:resource=\"&wnja11instances;~A\"/>
  <wn20schema:inSynset rdf:resource=\"&wnja11instances;~A\"/>~%" )
(defparameter *noun-word-sense-description-temp-sameAs* 
"<wn20schema:NounWordSense rdf:about=\"&wnja11instances;~A\"
    rdfs:label=\"~A\">
  <owl:sameAs rdf:resource=\"&wn30instances;~A\"/>
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:word rdf:resource=\"&wnja11instances;~A\"/>
  <wn20schema:inSynset rdf:resource=\"&wnja11instances;~A\"/>~%" )
(setq *verb-word-sense-description-temp* 
"<wn20schema:VerbWordSense rdf:about=\"&wnja11instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:word rdf:resource=\"&wnja11instances;~A\"/>
  <wn20schema:inSynset rdf:resource=\"&wnja11instances;~A\"/>~%" )
(defparameter *verb-word-sense-description-temp-sameAs* 
"<wn20schema:VerbWordSense rdf:about=\"&wnja11instances;~A\"
    rdfs:label=\"~A\">
  <owl:sameAs rdf:resource=\"&wn30instances;~A\"/>
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:word rdf:resource=\"&wnja11instances;~A\"/>
  <wn20schema:inSynset rdf:resource=\"&wnja11instances;~A\"/>~%" )
(setq *adjective-word-sense-description-temp* 
"<wn20schema:AdjectiveWordSense rdf:about=\"&wnja11instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:word rdf:resource=\"&wnja11instances;~A\"/>
  <wn20schema:inSynset rdf:resource=\"&wnja11instances;~A\"/>~%" )
(defparameter *adjective-word-sense-description-temp-sameAs* 
"<wn20schema:AdjectiveWordSense rdf:about=\"&wnja11instances;~A\"
    rdfs:label=\"~A\">
  <owl:sameAs rdf:resource=\"&wn30instances;~A\"/>
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:word rdf:resource=\"&wnja11instances;~A\"/>
  <wn20schema:inSynset rdf:resource=\"&wnja11instances;~A\"/>~%" )
(setq *adjectivesatellite-word-sense-description-temp* 
"<wn20schema:AdjectiveSatelliteWordSense rdf:about=\"&wnja11instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:word rdf:resource=\"&wnja11instances;~A\"/>
  <wn20schema:inSynset rdf:resource=\"&wnja11instances;~A\"/>~%" )
(defparameter *adjectivesatellite-word-sense-description-temp-sameAs* 
"<wn20schema:AdjectiveSatelliteWordSense rdf:about=\"&wnja11instances;~A\"
    rdfs:label=\"~A\">
  <owl:sameAs rdf:resource=\"&wn30instances;~A\"/>
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:word rdf:resource=\"&wnja11instances;~A\"/>
  <wn20schema:inSynset rdf:resource=\"&wnja11instances;~A\"/>~%" )
(setq *adverb-word-sense-description-temp* 
"<wn20schema:AdverbWordSense rdf:about=\"&wnja11instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:word rdf:resource=\"&wnja11instances;~A\"/>
  <wn20schema:inSynset rdf:resource=\"&wnja11instances;~A\"/>~%" )
(defparameter *adverb-word-sense-description-temp-sameAs* 
"<wn20schema:AdverbWordSense rdf:about=\"&wnja11instances;~A\"
    rdfs:label=\"~A\">
  <owl:sameAs rdf:resource=\"&wn30instances;~A\"/>
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:word rdf:resource=\"&wnja11instances;~A\"/>
  <wn20schema:inSynset rdf:resource=\"&wnja11instances;~A\"/>~%" )

(defun %%word-sense-header-output (outstream ss_type sense-name word tagcount word-name synset-name)
  (if (eql (symbol-package sense-name) (find-package :wnja11instances))
      ;; Japanese sense
      (format outstream
          (ecase ss_type
            (:noun *noun-word-sense-description-temp*)
            (:verb *verb-word-sense-description-temp*)
            (:adjective *adjective-word-sense-description-temp*)
            (:adjectivesatellite *adjectivesatellite-word-sense-description-temp*)
            (:adverb *adverb-word-sense-description-temp*))
        sense-name (underscore2space word)
        (or tagcount 0)
        word-name
        synset-name)
    ;; English sense
    (format outstream
        (ecase ss_type
          (:noun *noun-word-sense-description-temp-sameAs*)
          (:verb *verb-word-sense-description-temp-sameAs*)
          (:adjective *adjective-word-sense-description-temp-sameAs*)
          (:adjectivesatellite *adjectivesatellite-word-sense-description-temp-sameAs*)
          (:adverb *adverb-word-sense-description-temp-sameAs*))
      sense-name (underscore2space word) sense-name
      (or tagcount 0)
      word-name
      synset-name)))

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
            (:noun "~{  <wn20schema:derivationallyRelated rdf:resource=\"&wnja11instances;~A\"/>~^~%~}~%")
            (:verb "~{  <wn20schema:derivationallyRelated rdf:resource=\"&wnja11instances;~A\"/>~^~%~}~%")
            ;; wn21
            (:adjective "~{  <wn20schema:derivationallyRelated rdf:resource=\"&wnja11instances;~A\"/>~^~%~}~%")
            (:adjectivesatellite "~{  <wn20schema:derivationallyRelated rdf:resource=\"&wnja11instances;~A\"/>~^~%~}~%")
            (:adverb "~{  <wn20schema:derivationallyRelated rdf:resource=\"&wnja11instances;~A\"/>~^~%~}~%"))
        (sort target-sense-names #'string<)))
    (when (setq target-sense-names
                (mapcar #'cdr
                  (remove-if-not #'(lambda (acons) (string= word (car acons)))
                                 pertainsTo-source&target-sense-name-lists)))
      (format outstream
          (ecase ss_type
            (:adjective "~{  <wn20schema:adjectivePertainsTo rdf:resource=\"&wnja11instances;~A\"/>~^~%~}~%")
            (:adjectivesatellite "~{  <wn20schema:adjectivePertainsTo rdf:resource=\"&wnja11instances;~A\"/>~^~%~}~%")
            (:adverb "~{  <wn20schema:adverbPertainsTo rdf:resource=\"&wnja11instances;~A\"/>~^~%~}~%"))
        (sort target-sense-names #'string<)))
    (when (setq target-sense-names
                (mapcar #'cdr
                  (remove-if-not #'(lambda (acons) (string= word (car acons)))
                                 antonym-source&target-sense-name-lists)))
      (format outstream
          (ecase ss_type
            (:noun "~{  <wn20schema:antonymOf rdf:resource=\"&wnja11instances;~A\"/>~^~%~}~%")
            (:verb "~{  <wn20schema:antonymOf rdf:resource=\"&wnja11instances;~A\"/>~^~%~}~%")
            (:adjective "~{  <wn20schema:antonymOf rdf:resource=\"&wnja11instances;~A\"/>~^~%~}~%")
            (:adjectivesatellite "~{  <wn20schema:antonymOf rdf:resource=\"&wnja11instances;~A\"/>~^~%~}~%")
            (:adverb "~{  <wn20schema:antonymOf rdf:resource=\"&wnja11instances;~A\"/>~^~%~}~%"))
        (sort target-sense-names #'string<)))
    (when (setq target-sense-names
                (mapcar #'cdr
                  (remove-if-not #'(lambda (acons) (string= word (car acons)))
                                 participleof-source&target-sense-name-lists)))
      (format outstream
          (ecase ss_type
            (:adjective "~{  <wn20schema:participleOf rdf:resource=\"&wnja11instances;~A\"/>~^~%~}~%")
            (:adjectivesatellite "~{  <wn20schema:participleOf rdf:resource=\"&wnja11instances;~A\"/>~^~%~}~%"))
        (sort target-sense-names #'string<)))
    (loop for frame in (mapcar #'cdr
                         (remove-if-not #'(lambda (acons) (string= word (car acons)))
                                        word&frame-list))
        do (format outstream *frame-description-template-body*
             (make-frame-sentence word frame)))))

(setq *noun-synset-description-temp*
"<wn20schema:NounSynset rdf:about=\"&wnja11instances;~A\"
    rdfs:label=\"~A\">
  <owl:sameAs rdf:resource=\"&wn30instances;~A\"/>
  <wn20schema:synsetId>~D</wn20schema:synsetId>~%")

(setq *verb-synset-description-temp*
"<wn20schema:VerbSynset rdf:about=\"&wnja11instances;~A\"
    rdfs:label=\"~A\">
  <owl:sameAs rdf:resource=\"&wn30instances;~A\"/>
  <wn20schema:synsetId>~D</wn20schema:synsetId>~%")

(setq *adjective-synset-description-temp*
"<wn20schema:AdjectiveSynset rdf:about=\"&wnja11instances;~A\"
    rdfs:label=\"~A\">
  <owl:sameAs rdf:resource=\"&wn30instances;~A\"/>
  <wn20schema:synsetId>~D</wn20schema:synsetId>~%")

(setq *adjective-satellite-synset-description-temp*
"<wn20schema:AdjectiveSatelliteSynset rdf:about=\"&wnja11instances;~A\"
    rdfs:label=\"~A\">
  <owl:sameAs rdf:resource=\"&wn30instances;~A\"/>
  <wn20schema:synsetId>~D</wn20schema:synsetId>~%")

(setq *adverb-synset-description-temp*
"<wn20schema:AdverbSynset rdf:about=\"&wnja11instances;~A\"
    rdfs:label=\"~A\">
  <owl:sameAs rdf:resource=\"&wn30instances;~A\"/>
  <wn20schema:synsetId>~D</wn20schema:synsetId>~%")

(setq *synset-description-template-body* "  <wn20schema:containsWordSense rdf:resource=\"&wnja11instances;~A\"/>~%")


(defun %%synset-header-output (outstream ss_type synset-name word offset)
  (format outstream
      (ecase ss_type
        (:noun *noun-synset-description-temp*)
        (:verb *verb-synset-description-temp*)
        (:adjective *adjective-synset-description-temp*)
        (:adjectivesatellite *adjective-satellite-synset-description-temp*)
        (:adverb *adverb-synset-description-temp*))
    synset-name (underscore2space word) synset-name
    (+ offset
       (ecase ss_type
         (:noun 100000000)
         (:verb 200000000)
         (:adjective 300000000)
         (:adjectivesatellite 300000000)
         (:adverb 400000000)))))
;;; EOF
