;;;
;;;; WordNet Dictionary Information Retrieval and OWL Conversion Program
;;;                  http://www.w3.org/TR/2006/WD-wordnet-rdf-20060619/
;;; This program is applied to Allegro8.1, 8.2, 9.0 Modern, sbcl, and WordNet.
;;;
;; History
;; -------
;; 2013/06/28    File created.
;;; ==================================================================================

(in-package :cl-user)

(defpackage wn
  (:use :common-lisp)
  (:export split-seq-using get-synset-number-in-index-entry))

(defpackage wn20
  (:use :common-lisp))

(defpackage wn21
  (:use common-lisp wn20))

(defpackage wn30
  (:use common-lisp wn20))

(defpackage wn20schema
  (:use) ; suppress common-lisp package
  (:documentation "http://www.w3.org/2006/03/wn/wn20/instances/"))

(defpackage wn21instances
  (:use)
  (:documentation "http://www.w3.org/2006/03/wn/wn21/instances/"))

(defpackage wn30instances
  (:use)
  (:documentation "http://www.w3.org/2006/03/wn/wn30/instances/"))