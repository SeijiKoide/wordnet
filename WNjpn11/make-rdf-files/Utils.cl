;;;
;;;; Utility Functions for WordNet
;;; This program is applied to Allegro8.1, 8.2, 9.0 Modern, sbcl, and WordNet.
;;;

(in-package :wn)

;;;
;;;; Utils from SWCLOS
;;;

;;; from AIMA
(defun mklist (x)
  "If <x> is a list, return it; otherwise return a singleton list, (<x>)."
  (if (listp x) x (list x)))

(defun mappend (fn &rest lists)
  "Apply <fn> to respective elements of list(s), and append results."
  (reduce #'append (apply #'mapcar fn lists) :from-end t))

(defun last2 (lst)
  "Return the last two element of a list."
  (let ((inv (reverse lst)))
    (nreverse (list (car inv) (cadr inv)))))

(defun squash (x)
  "flattens a nested list <x> and returns a list that includes only atoms."
  (cond ((consp x) (mappend #'squash x))
        (t (list x))))

(defun iri-delimiter-p (char)
  (or (char= char #\<)
      (char= char #\>)
      (char= char #\#)
      (char= char #\%)
      (char= char #\")))

(defun iri-escape-for-lexform (str)
  "This function performs Percent-Encoding for word and lexform."
  (flet ((escape-p (c)
                   (declare (optimize (speed 3) (safety 1)))
                   (or (char= c #\%)
                       (char= c #\/)
                       (char= c #\?)
                       (char= c #\#)
                       (char= c #\[)
                       (char= c #\])
                       (eq (char-code c) #x20)
                       )))
    (labels ((escape (str)
                     (let ((pos 0))
                       (cond ((setq pos (position-if #'escape-p str)) ; found
                              (let ((c (char str pos)))
                                (concatenate 'cl:string
                                  (subseq str 0 pos)
                                  (format nil "%~X" (char-code c))
                                  (escape (subseq str (1+ pos))))))
                             (t str)))))
      (escape str))))

;;;
;;; Utils for WordNet
;;;

(defun find-schema-package (package)
  "retrieves versioned schema package from <package>, e.g., 
given \"wn20\", returns wn20schema package."
  (let ((package-name (package-name (find-package package))))
    (find-package (concatenate 'string package-name (string 'schema)))))

(defun find-instance-package (package)
  "retrieves versioned instance package from <package>, e.g., 
given \"wn20\", returns wn20instance package."
  (let ((package-name (package-name (find-package package))))
    (find-package (concatenate 'string package-name (string 'instances)))))

(defvar *wn-package* *package*
  "default-wordnet-version-package,
This value is set at each version program.")

(defvar *wnja-package* nil
  "default-japanese-wordnet-version-package,
This value is set at each Japanese version programl.")

(defun make-word-name (str &optional (package *wn-package*))
  (intern (concatenate 'cl:string "word-" (iri-escape-for-lexform str))
          (find-instance-package package)))

(defun make-sense-name (lexform pos n &optional (package *wn-package*))
  (intern (concatenate 'cl:string "wordsense-" (iri-escape-for-lexform lexform) "-" (string-downcase (string pos)) "-" (format cl:nil "~d" n))
          (find-instance-package package)))

(defun make-synset-name-str (lexform pos n)
  (concatenate 'cl:string "synset-" (iri-escape-for-lexform lexform) "-" (string-downcase (string pos)) "-" (format nil "~d" n)))

(defun make-synset-name (lexform pos n &optional (package *wn-package*))
  (intern (concatenate 'cl:string "synset-" (iri-escape-for-lexform lexform) "-" (string-downcase (string pos)) "-" (format nil "~d" n))
          (find-instance-package package)))

;;;
;;;
;;;

(defun space2underscore (string)
  "substitues spaces in <string> to underscores."
  (substitute #\_ #\Space string))

(defun underscore2space (string)
  "substitues underscores in <string> to spaces."
  (substitute #\Space #\_ string))

(defun get-adj-word (string)
  "splits adjective word into a word part and a marker, and returns a word."
  (flet ((marker-p (x) (or (string= x "p)") (string= x "a)") (string= x "ip)"))))
    (let ((parts (split-seq-on string #\()))
      (cond ((marker-p (second parts)) (car parts))
            (t string)))))

(defun get-adj-marker (string)
  "splits adjective word into a word part and a marker, and returns a marker and a word."
  (flet ((marker-p (x) (or (string= x "p)") (string= x "a)") (string= x "ip)"))))
    (let ((parts (split-seq-on string #\()))
      (cond ((marker-p (second parts))
             (values (cdr (assoc (second parts) '(("p)" . :predicate-position)
                                                  ("a)" . :prenominal-position)
                                                  ("ip)" . :postnominal-position))
                                 :test #'string=))
                     (car parts)))
            (t (values nil string))))))

(defun collocated-p (key)
  "If <key> has characters of hyphen or underscore, the word is collocated."
  (find-if #'(lambda (c) (or (char= c #\_) (char= c #\-))) key))

;;;
;;;
;;;

(defvar *wnsearchdir* nil
  "directory where dictionaries of English wordnet are located.
This value should be set in each version of WordNet.")

(defun pos-index-file (pos)
  (make-pathname :host (pathname-host *wnsearchdir*)
                 :device (pathname-device *wnsearchdir*)
                 :directory (pathname-directory *wnsearchdir*)
                 :name "index"
                 :type (pos-filename-affix pos)))

(defun pos-data-file (pos)
  (make-pathname :host (pathname-host *wnsearchdir*)
                 :device (pathname-device *wnsearchdir*)
                 :directory (pathname-directory *wnsearchdir*)
                 :name "data"
                 :type (pos-filename-affix pos)))

(defun pos-morph-file (pos)
  (make-pathname :host (pathname-host *wnsearchdir*)
                 :device (pathname-device *wnsearchdir*)
                 :directory (pathname-directory *wnsearchdir*)
                 :name (pos-filename-affix pos)
                 :type "exc"))

;;;
;;;
;;;

(defun escape-gloss-decls (gloss)
  "escapes characters &amp;, &lt;, and &gt;, and returns escaped <gloss>."
  (let ((pos (position-if #'(lambda (ch) (or (char= ch #\<) (char= ch #\>) (char= ch #\&)))
                          gloss)))
    (if pos
        (let ((ch (char gloss pos)))
          (concatenate 'cl:string
            (subseq gloss 0 pos)
            (cond ((char= ch #\>) "&gt;")
                  ((char= ch #\<) "&lt;")
                  ((char= ch #\&) "&amp;")
                  ((error "Cant happen!")))
            (escape-gloss-decls (subseq gloss (1+ pos)))))
      gloss)))
