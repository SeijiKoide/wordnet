;;;-*- Mode: common-lisp; syntax: common-lisp; package: wn2rdf; base: 10 -*-
;;;
;;;; Utility Macros for WordNet and Miscellaneous Functions
;;; This program is applied to Allegro8.1, 8.2, 9.0 Modern, sbcl, and WordNet.
;;;
;;;
;;; History
;;; -------
;;; 2011.10.24    Main part is separated here from ./make-rdf-files/utilmacro.
;;; 2005.01.21    File obtained from Ruml's web site.
;;;

(in-package :wn2rdf)

;;;======================================================================================

;; an interface to wordnet, a semantic network for words from Princeton.
;; This code is partly from Wheeler Ruml's wordnet lisp APIs.
;; Wheeler Ruml (ruml@eecs.harvard.edu)

;;;======================================================================================

;;;
;;; Utils from Ruml
;;;

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defmacro with-gensyms (syms &body body)
    "set the value of each symbol in SYMS to a unique gensym"
    `(let ,(mapcar #'(lambda (s)
                       `(,s (gensym)))
             syms)
       ,@body))
  ) ; End of eval-when

(defun str (&rest args)
  "makes a string from printed representations of all of its arguments"
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (let ((*print-circle* ()))
    (format nil "~{~A~}" args)))

(defun spaced-str (first &rest args)
  "makes a string from all its arguments, inserting spaces between items"
  (let ((*print-circle* ())
        (*print-level* ())
        (*print-length* ()))
    (format nil "~A~{ ~A~}" first args)))

(defun make-spaced-str (list)
  (let ((*print-circle* ())
        (*print-level* ())
        (*print-length* ()))
    (format nil "~{~A~^ ~}" list)))

(defun first-n-list (list &optional (n 1))
  "returns a new list containing the first <n> elements of <list>."
  (cond ((<= n 0) nil)
        ((endp list) nil)
        (t (cons (car list)
                 (first-n-list (cdr list) (1- n))))))

(defun groups-of (n list)
  "returns a list of subsequences of <list> of length <= <n>"
  (assert (> n 0))
  (when list
    (cons (first-n list n)
          (groups-of n (nthcdr n list)))))

(defun split-seq-on (str &optional (ch #\Space))
  "returns a list of strings formed by breaking <str> at every occurence
of <ch> (which is not included).  Works for any sequence, not just strings,
but optimized for vectors."
  (when str
    (do* ((prev-pos 0 (1+ next-pos))
          (next-pos (position ch str)
                    (position ch str :start prev-pos))
          (stuff (list (subseq str 0 next-pos))
                 (cons (subseq str prev-pos next-pos)
                       stuff)))
         ((null next-pos) (nreverse stuff)))))

(defun skip-line (strm)
  "reads through the next #\Newline."
  (declare (optimize (speed 3) (safety 1) (debug 0))
           (type stream strm))
  (loop
    (let ((ch (read-char strm nil nil)))
      #-:sbcl (declare (dynamic-extent ch))
      (when (eq ch #\Newline)
        (return)))))

(defun first-n (seq &optional (n 1))
  "returns a new sequence containing the first <n> elements of <seq>."
  (etypecase seq
    (vector (subseq seq 0 (min n (length seq))))
    (list (first-n-list seq n))))

(defun seq-starts-as (prefix-str text)
  "returns non-nil iff <text> starts as <str>"
  (let ((diff-pos (mismatch prefix-str text)))
    (or (null diff-pos)
        (>= diff-pos (length prefix-str)))))

(defun split-seq-using (str &optional (ch #\Space))
  "returns a list of strings.  Ignores multiple delimiters."
  (when str
    (do* ((prev-pos (position ch str :test-not #'eql)
                    (position ch str :test-not #'eql :start next-pos))
          (next-pos (when prev-pos (position ch str :start prev-pos))
                    (when prev-pos (position ch str :start prev-pos)))
          (stuff (when prev-pos
                   (list (subseq str prev-pos next-pos)))
                 (if prev-pos
                     (cons (subseq str prev-pos next-pos)
                           stuff)
                   stuff)))
         ((null next-pos) (nreverse stuff)))))

(defun white-space-p (ch)
  (or (char= ch #\Space)
      (char= ch #\Tab)
      (char= ch #\Newline)))

(defun split-seq (str)
  "returns a list of strings.  Ignores multiple delimiters."
  (when str
    (do* ((prev-pos (position-if-not #'white-space-p str)
                    (position-if-not #'white-space-p str :start next-pos))
          (next-pos (when prev-pos (position-if #'white-space-p str :start prev-pos))
                    (when prev-pos (position-if #'white-space-p str :start prev-pos)))
          (stuff (when prev-pos
                   (list (subseq str prev-pos next-pos)))
                 (if prev-pos
                     (cons (subseq str prev-pos next-pos)
                           stuff)
                   stuff)))
         ((null next-pos) (nreverse stuff)))))

(defmacro with-list-split-after (n (first-part second-part) list &body body)
  "executes <body> with <first-part> bound to the first <n> elements of<list>
and <second-part> bound to the rest"
  (with-gensyms (num l)
    `(let* ((,num ,n)
            (,l ,list)
            (,first-part (first-n ,l ,num))
            (,second-part (nthcdr ,num ,l)))
       ,@body)))

;;;======================================================================================
