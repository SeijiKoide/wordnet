(in-package :asdf)

(eval-when (:load-toplevel :execute)
  (defparameter *make-rdf-files-directory*
    (make-pathname :host (pathname-host *load-truename*)
                   :device (pathname-device *load-truename*)
                   :directory (pathname-directory *load-truename*)))
  (setf (logical-pathname-translations "MAKE-RDF-FILES")
    `(("*.*"
       ,(make-pathname
         :host (pathname-host *make-rdf-files-directory*)
         :device (pathname-device *make-rdf-files-directory*)
         :directory (pathname-directory *make-rdf-files-directory*)
         :name :wild
         :type :wild
         ))))
  ) ; End of eval-when

(defmethod source-file-type ((c cl-source-file) (s module)) "cl")

(defsystem :wordnet2rdf
  :name "WordNet to RDF Conversion System"
  :author "Seiji Koide <koide@nii.ac.jp>"
  :maintainer "Seiji Koide <koide@nii.ac.jp>"
  :version "1.0.0"
  :licence "MIT"
  :description "Conversion program from WordNet 3.0 to RDF files"
  :long-description "This program makes RDF files of WordNet 3.0 dictionaries according to the guidline by W3C.
See http://www.w3.org/TR/2006/WD-wordnet-rdf-20060619/."
  :pathname #+(and :asdf (not :asdf2)) (translate-logical-pathname "MAKE-RDF-FILES:")
            #+(and :asdf :asdf2)       nil
  :serial t
  :components
  ((:file "package")
   (:file "wn20schema")
   (:file "wn21schema")
   (:file "wn30schema")
   (:file "utilmacro")
   (:file "Utils")
   (:file "WordNet");        :depends-on ("utilmacro" "Utils"))
   (:file "WordNet20");      :depends-on ("utilmacro")); "Utils" "wn20schema"))
   (:file "WordNet21");      :depends-on ("utilmacro" "WordNet20"))
   (:file "WordNet30");      :depends-on ("utilmacro" "WordNet20"))
   (:file "make-rdf-files"); :depends-on ("wn20schema" "wn21schema" "wn30schema" "utilmacro" "Utils" "WordNet" 
                           ;             "WordNet20" "WordNet21" "WordNet30"))
   )
)

(in-package #:cl-user)
(format t "~%=========== System Description ================")
(describe (asdf:find-system :wordnet2rdf))
(format t "===============================================~%")

(format t "~%;; To compile RDF module, execute these forms:~%;; ~s~%"
  '(asdf:operate 'asdf:compile-op :make-rdf-files))

(format t "~%;; To load RDF module, execute these forms:~%;; ~s~%"
  '(asdf:operate 'asdf:load-op :make-rdf-files))

(format t "~%;; To load a whole system, execute these forms:~%;; ~s~%"
  '(asdf:load-system :wordnet2rdf))
