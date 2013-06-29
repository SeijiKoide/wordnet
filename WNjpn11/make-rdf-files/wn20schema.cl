;;;
;;; WordNet Schema for W3C RDF/OWL Representation based on W3C Working Draft 19 June 2006,
;;; See http://www.w3.org/TR/2006/WD-wordnet-rdf-20060619/
;;;
;;; This program does not require SWCLOS.

(in-package :wn)

;;;
;;; Logical Pathname for WordNet20 Home Directory
;;;

(defvar *wordnet20-directory* nil)

(eval-when (:execute :load-toplevel)
  (setq *wordnet20-directory*
        (make-pathname :host (pathname-host *load-truename*)
                       :device (pathname-device *load-truename*)
                       :directory (pathname-directory *load-truename*)))
  (setf (logical-pathname-translations "WN20")
    `(("*.*"
       ,(make-pathname
         :host (pathname-host *wordnet20-directory*)
         :device (pathname-device *wordnet20-directory*)
         :directory (pathname-directory *wordnet20-directory*)
         :name :wild
         :type :wild
         ))))
  ) ; End of eval-when

