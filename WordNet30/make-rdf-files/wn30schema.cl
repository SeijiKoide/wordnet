;;;
;;; WordNet Schema for W3C RDF/OWL Representation based on W3C Working Draft 19 June 2006,
;;; See http://www.w3.org/TR/2006/WD-wordnet-rdf-20060619/
;;;
;;; This program does not require SWCLOS.

(defpackage wn30schema
  (:use) ; suppress common-lisp package
  )

(defpackage wn30)

(defpackage wn
  (:use :common-lisp))
(in-package :wn)

;;;
;;; Logical Pathname for WordNet30 Home Directory
;;;

(defvar *wordnet30-directory* nil)

(eval-when (:execute :load-toplevel)
  (setq *wordnet30-directory*
        (make-pathname :host (pathname-host *load-truename*)
                       :device (pathname-device *load-truename*)
                       :directory (pathname-directory *load-truename*)))
  (setf (logical-pathname-translations "WN30")
    `(("*.*"
       ,(make-pathname
         :host (pathname-host *wordnet30-directory*)
         :device (pathname-device *wordnet30-directory*)
         :directory (pathname-directory *wordnet30-directory*)
         :name :wild
         :type :wild
         ))))
  ) ; End of eval-when

