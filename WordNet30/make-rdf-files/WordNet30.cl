;;;
;;;; WordNet Dictionary Information Retrieval and OWL Conversion Program
;;; This program is applied to Allegro8.1, 8.2, 9.0 Modern, sbcl, and WordNet.
;;;
;;;
;;; History
;;; -------
;;; 2011.07.25    Copied and modified from WordNet20.cl
;;;

(defpackage wn30instances
  (:use)
  (:documentation "http://www.w3.org/2006/03/wn/wn30/instances/"))

(defpackage wn30
  (:use common-lisp wn20))

;(defpackage wn
;  (:use :common-lisp))
(in-package :wn)

(setf *wn-package* (find-package :wn30))

;;;;;;;;;;;;;;; globals ;;;;;;;;;;;
(eval-when (:execute :load-toplevel :compile-toplevel)
(setq *wnhome*
    (or #+:excl (sys:getenv "WNHOME")             ; it is registered in system but commented for 2.1 and 3.0
        #+:sbcl (sb-ext:posix-getenv "WNHOME")
        #+:mswindows "C:\\WordNet\\WordNet-3.0"
        #+:unix "/usr/local/wordnet/WordNet-3.0"))
(setq *wnsearchdir*
      #+:mswindows (str (or (sys:getenv "WNSEARCHDIR")
                            (str *wnhome* "\\dict"))
                        "\\")
      #+:unix (str (or #+:excl (sys:getenv "WNSEARCHDIR")
                       #+:sbcl (sb-ext:posix-getenv "WNSEARCHDIR")
                       (str *wnhome* "/dict"))
                   "/"))
)
;;;
;;;;
;;;

(setq *doc-decl* "<!DOCTYPE rdf:RDF [
    <!ENTITY rdf 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'>
    <!ENTITY rdfs 'http://www.w3.org/2000/01/rdf-schema#'>
    <!ENTITY wn20schema 'http://www.w3.org/2006/03/wn/wn20/schema/'>
    <!ENTITY wn30instances 'http://www.w3.org/2006/03/wn/wn30/instances/'>
    <!ENTITY wn21schema 'http://www.w3.org/2006/03/wn/wn21/schema/'>
]>~%~%")
(setq *rdf-decl-prolog*
"<rdf:RDF
    xmlns:rdf=\"&rdf;\"
    xmlns:rdfs=\"&rdfs;\"
    xmlns:wn20schema=\"&wn20schema;\"
    xmlns:wn30instances=\"&wn30instances;\"
    xmlns:wn21schema=\"&wn21schema;\"
    xml:lang=\"en-US\">~%")

(setq *noun-synset-description-template* "<wn20schema:NounSynset rdf:about=\"&wn30instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:synsetId>~D</wn20schema:synsetId>
</wn20schema:NounSynset>~%~%")
(setq *verb-synset-description-template* "<wn20schema:VerbSynset rdf:about=\"&wn30instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:synsetId>~D</wn20schema:synsetId>
</wn20schema:VerbSynset>~%~%")
(setq *adjective-synset-description-template* "<wn20schema:AdjectiveSynset rdf:about=\"&wn30instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:synsetId>~D</wn20schema:synsetId>
</wn20schema:AdjectiveSynset>~%~%")
(setq *adjective-satellite-synset-description-template* "<wn20schema:AdjectiveSatelliteSynset rdf:about=\"&wn30instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:synsetId>~D</wn20schema:synsetId>
</wn20schema:AdjectiveSatelliteSynset>~%~%")
(setq *adverb-synset-description-template* "<wn20schema:AdverbSynset rdf:about=\"&wn30instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:synsetId>~D</wn20schema:synsetId>
</wn20schema:AdverbSynset>~%~%")

(setq *noun-synset-description-temp* "<wn20schema:NounSynset rdf:about=\"&wn30instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:synsetId>~D</wn20schema:synsetId>~%")

(setq *verb-synset-description-temp* "<wn20schema:VerbSynset rdf:about=\"&wn30instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:synsetId>~D</wn20schema:synsetId>~%")

(setq *adjective-synset-description-temp* "<wn20schema:AdjectiveSynset rdf:about=\"&wn30instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:synsetId>~D</wn20schema:synsetId>~%")

(setq *adjective-satellite-synset-description-temp* "<wn20schema:AdjectiveSatelliteSynset rdf:about=\"&wn30instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:synsetId>~D</wn20schema:synsetId>~%")

(setq *adverb-synset-description-temp* "<wn20schema:AdverbSynset rdf:about=\"&wn30instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:synsetId>~D</wn20schema:synsetId>~%")


(setq *noun-hyponym-description-template* "<rdf:Description rdf:about=\"&wn30instances;~A\">
~{  <wn20schema:hyponymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}
</rdf:Description>~%~%")
(setq *verb-hyponym-description-template* "<rdf:Description rdf:about=\"&wn30instances;~A\">
~{  <wn20schema:hyponymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *noun-hypernym-description-template* "<rdf:Description rdf:about=\"&wn30instances;~A\">
~{  <wn20schema:hypernymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}
</rdf:Description>~%~%")
(setq *verb-hypernym-description-template* "<rdf:Description rdf:about=\"&wn30instances;~A\">
~{  <wn20schema:hypernymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *derivationallyRelated-description-template* "<rdf:Description rdf:about=\"&wn30instances;~A\">
~{  <wn20schema:derivationallyRelated rdf:resource=\"&wn30instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *memberMeronym-description-template* "<rdf:Description rdf:about=\"&wn30instances;~A\">
~{  <wn20schema:memberMeronymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}
</rdf:Description>~%~%")
(setq *memberHolonym-description-template* "<rdf:Description rdf:about=\"&wn30instances;~A\">
~{  <wn20schema:memberHolonymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *partmeronym-description-template* "<rdf:Description rdf:about=\"&wn30instances;~A\">
~{  <wn20schema:partMeronymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}
</rdf:Description>~%~%")
(setq *partholonym-description-template* "<rdf:Description rdf:about=\"&wn30instances;~A\">
~{  <wn20schema:partHolonymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *substanceMeronym-description-template* "<rdf:Description rdf:about=\"&wn30instances;~A\">
~{  <wn20schema:substanceMeronymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}
</rdf:Description>~%~%")
(setq *substanceHolonym-description-template* "<rdf:Description rdf:about=\"&wn30instances;~A\">
~{  <wn20schema:substanceHolonymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *adjective-pertainsTo-description-template* "<rdf:Description rdf:about=\"&wn30instances;~A\">
~{  <wn20schema:adjectivePertainsTo rdf:resource=\"&wn30instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *adverb-pertainsTo-description-template* "<rdf:Description rdf:about=\"&wn30instances;~A\">
~{  <wn20schema:adverbPertainsTo rdf:resource=\"&wn30instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *classifiedBy-header-template* "<rdf:Description rdf:about=\"&wn30instances;~A\">")
(setq *classifiedByTopic-body-template* 
"~%~{  <wn20schema:classifiedByTopic rdf:resource=\"&wn30instances;~A\"/>~^~%~}")
(setq *classifiedByUsage-body-template* 
"~%~{  <wn20schema:classifiedByUsage rdf:resource=\"&wn30instances;~A\"/>~^~%~}")
(setq *classifiedByRegion-body-template* 
"~%~{  <wn20schema:classifiedByRegion rdf:resource=\"&wn30instances;~A\"/>~^~%~}")



(setq *similarity-description-template* "<rdf:Description rdf:about=\"&wn30instances;~A\">
~{  <wn20schema:similarTo rdf:resource=\"&wn30instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *antonym-description-template* "<rdf:Description rdf:about=\"&wn30instances;~A\">
~{  <wn20schema:antonymOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *sameverbgroupas-description-template* "<rdf:Description rdf:about=\"&wn30instances;~A\">
~{  <wn20schema:sameVerbGroupAs rdf:resource=\"&wn30instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *entailment-description-template* "<rdf:Description rdf:about=\"&wn30instances;~A\">
~{  <wn20schema:entails rdf:resource=\"&wn30instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *causes-description-template* "<rdf:Description rdf:about=\"&wn30instances;~A\">
~{  <wn20schema:causes rdf:resource=\"&wn30instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *participleof-description-template* "<rdf:Description rdf:about=\"&wn30instances;~A\">
~{  <wn20schema:participleOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *attribute-description-template* "<rdf:Description rdf:about=\"&wn30instances;~A\">
~{  <wn20schema:attribute rdf:resource=\"&wn30instances;~A\"/>~^~%~}
</rdf:Description>~%~%")
(setq *attributeOf-description-template* "<rdf:Description rdf:about=\"&wn30instances;~A\">
~{  <wn20schema:attributeOf rdf:resource=\"&wn30instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *seealso-description-template* "<rdf:Description rdf:about=\"&wn30instances;~A\">
~{  <wn20schema:seeAlso rdf:resource=\"&wn30instances;~A\"/>~^~%~}
</rdf:Description>~%~%")

(setq *gloss-description-template* "<rdf:Description rdf:about=\"&wn30instances;~A\">
  <wn20schema:gloss>~A</wn20schema:gloss>
</rdf:Description>~%~%")

(setq *frame-description-template-head* "<rdf:Description rdf:about=\"&wn30instances;~A\">~%")



(setq *word-description-template* "<wn20schema:Word rdf:about=\"&wn30instances;~A\"
    wn20schema:lexicalForm=\"~A\">
~{  <wn20schema:sense rdf:resource=\"&wn30instances;~A\"/>~^~%~}
</wn20schema:Word>~%~%")
(setq *collocation-description-template* "<wn20schema:Collocation rdf:about=\"&wn30instances;~A\"
    wn20schema:lexicalForm=\"~A\">
~{  <wn20schema:sense rdf:resource=\"&wn30instances;~A\"/>~^~%~}
</wn20schema:Collocation>~%~%")

(setq *noun-word-sense-description-template* "<wn20schema:NounWordSense rdf:about=\"&wn30instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:inSynset rdf:resource=\"&wn30instances;~A\"/>
  <wn20schema:word rdf:resource=\"&wn30instances;~A\"/>
</wn20schema:NounWordSense>~%~%" )
(setq *verb-word-sense-description-template* "<wn20schema:VerbWordSense rdf:about=\"&wn30instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:inSynset rdf:resource=\"&wn30instances;~A\"/>
  <wn20schema:word rdf:resource=\"&wn30instances;~A\"/>
</wn20schema:VerbWordSense>~%~%" )
(setq *adjective-word-sense-description-template* "<wn20schema:AdjectiveWordSense rdf:about=\"&wn30instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:inSynset rdf:resource=\"&wn30instances;~A\"/>
  <wn20schema:word rdf:resource=\"&wn30instances;~A\"/>
</wn20schema:AdjectiveWordSense>~%~%" )
(setq *adjectivesatellite-word-sense-description-template* "<wn20schema:AdjectiveSatelliteWordSense rdf:about=\"&wn30instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:word rdf:resource=\"&wn30instances;~A\"/>
  <wn20schema:inSynset rdf:resource=\"&wn30instances;~A\"/>
</wn20schema:AdjectiveSatelliteWordSense>~%~%" )
(setq *adverb-word-sense-description-template* "<wn20schema:AdverbWordSense rdf:about=\"&wn30instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:inSynset rdf:resource=\"&wn30instances;~A\"/>
  <wn20schema:word rdf:resource=\"&wn30instances;~A\"/>
</wn20schema:AdverbWordSense>~%~%" )

(setq *noun-word-sense-description-temp* "<wn20schema:NounWordSense rdf:about=\"&wn30instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:word rdf:resource=\"&wn30instances;~A\"/>
  <wn20schema:inSynset rdf:resource=\"&wn30instances;~A\"/>~%" )
(setq *verb-word-sense-description-temp* "<wn20schema:VerbWordSense rdf:about=\"&wn30instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:word rdf:resource=\"&wn30instances;~A\"/>
  <wn20schema:inSynset rdf:resource=\"&wn30instances;~A\"/>~%" )
(setq *adjective-word-sense-description-temp* "<wn20schema:AdjectiveWordSense rdf:about=\"&wn30instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:word rdf:resource=\"&wn30instances;~A\"/>
  <wn20schema:inSynset rdf:resource=\"&wn30instances;~A\"/>~%" )
(setq *adjectivesatellite-word-sense-description-temp* "<wn20schema:AdjectiveSatelliteWordSense rdf:about=\"&wn30instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:word rdf:resource=\"&wn30instances;~A\"/>
  <wn20schema:inSynset rdf:resource=\"&wn30instances;~A\"/>~%" )
(setq *adverb-word-sense-description-temp* "<wn20schema:AdverbWordSense rdf:about=\"&wn30instances;~A\"
    rdfs:label=\"~A\">
  <wn20schema:tagCount>~A</wn20schema:tagCount>
  <wn20schema:word rdf:resource=\"&wn30instances;~A\"/>
  <wn20schema:inSynset rdf:resource=\"&wn30instances;~A\"/>~%" )


(setq *synset-description-template-head* "<rdf:Description rdf:about=\"&wn30instances;~A\">~%")
(setq *synset-description-template-body* "  <wn20schema:containsWordSense rdf:resource=\"&wn30instances;~A\"/>~%")


(setq *synset-file-path* "WN30:wordnet-synset.rdf")
(setq *hyponym-file-path* "WN30:wordnet-hyponym.rdf")
(setq *hypernym-file-path* "WN30:wordnet-hypernym.rdf")
(setq *derivationallyRelated-file-path* "WN30:wordnet-derivationallyrelated.rdf")
(setq *membermeronym-file-path* "WN30:wordnet-membermeronym.rdf")
(setq *memberholonym-file-path* "WN30:wordnet-membeholonym.rdf")
(setq *partmeronym-file-path* "WN30:wordnet-partmeronym.rdf")
(setq *partholonym-file-path* "WN30:wordnet-partholonym.rdf")
(setq *substancemeronym-file-path* "WN30:wordnet-substancemeronym.rdf")
(setq *substanceholonym-file-path* "WN30:wordnet-substanceholonym.rdf")
(setq *pertainsTo-file-path* "WN30:wordnet-pertainsto.rdf")
(setq *classifiedby-file-path* "WN30:wordnet-classifiedby.rdf")
(setq *similarity-file-path* "WN30:wordnet-similarity.rdf")
(setq *antonym-file-path* "WN30:wordnet-antonym.rdf")
(setq *entailment-file-path* "WN30:wordnet-entailment.rdf")
(setq *causes-file-path* "WN30:wordnet-causes.rdf")
(setq *participleof-file-path* "WN30:wordnet-participleof.rdf")
(setq *sameverbgroupas-file-path* "WN30:wordnet-sameverbgroupas.rdf")
(setq *attribute-file-path* "WN30:wordnet-attribute.rdf")
(setq *seealso-file-path* "WN30:wordnet-seealso.rdf")
(setq *glossary-file-path* "WN30:wordnet-glossary.rdf")
(setq *frame-file-path* "WN30:wordnet-frame.rdf")
(setq *wordsenseandwords-file-path* "WN30:wordnet-wordsenseandwords.rdf")
(setq *one-big-rdf-file-noun-path* "WN30:wordnet-noun.rdf")
(setq *one-big-rdf-file-verb-path* "WN30:wordnet-verb.rdf")
(setq *one-big-rdf-file-adjective-path* "WN30:wordnet-adjective.rdf")
(setq *one-big-rdf-file-adverb-path* "WN30:wordnet-adverb.rdf")
(setq *one-big-rdf-file-path* (merge-pathnames "wordnet.rdf" (user-homedir-pathname)))
;;; EOF
