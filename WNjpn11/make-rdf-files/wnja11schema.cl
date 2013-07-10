;;;-*- Mode: common-lisp; syntax: common-lisp; package: wn; base: 10 -*-
;;;
;;; Japanese WordNet Schema for W3C RDF/OWL Representation 
;;;

(in-package :wn)

;;;
;;; Note that *wordnet20-directory* and WN20 is the same as *wordnet30-directory* and WN30.
;;;

;;;
;;; No wn30full.rdf
;;;

(defvar *wordnetja11-directory* nil)

(eval-when (:load-toplevel :execute)
  (setq *wordnetja11-directory*
        (make-pathname :host (pathname-host *load-truename*)
                       :device (pathname-device *load-truename*)
                       :directory (pathname-directory *load-truename*)))
  (setf (logical-pathname-translations "WNJA11")
    `(("*.*"
       ,(make-pathname
         :host (pathname-host *wordnetja11-directory*)
         :device (pathname-device *wordnetja11-directory*)
         :directory (pathname-directory *wordnetja11-directory*)
         :name :wild
         :type :wild
         ))))
  ) ; end of eval-when

