;;;
;;; WordNet Schema for W3C RDF/OWL Representation based on W3C Working Draft 19 June 2006,
;;; See http://www.w3.org/TR/2006/WD-wordnet-rdf-20060619/
;;;
;;; This program does not require SWCLOS.

(in-package :wn)

;;;
;;; Logical Pathname for WordNet21 Home Directory
;;;

(defvar *wordnet21-directory* nil)

(eval-when (:execute :load-toplevel)
  (setq *wordnet21-directory*
        (make-pathname :host (pathname-host *load-truename*)
                       :device (pathname-device *load-truename*)
                       :directory (pathname-directory *load-truename*)))
  (setf (logical-pathname-translations "WN21")
    `(("*.*"
       ,(make-pathname
         :host (pathname-host *wordnet21-directory*)
         :device (pathname-device *wordnet21-directory*)
         :directory (pathname-directory *wordnet21-directory*)
         :name :wild
         :type :wild
         ))))
  ) ; End of eval-when

