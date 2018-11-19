;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LET-OVER-LAMBDA; Base: 10 -*- file: let-over-lambda.lisp

;;;; Copyleft Spenser Truex "Equwal" 2018
;;;; This file for automatic gensym reader macros on SBCL, since SBCL doesn't support it otherwise.

;; Antiweb (C) Doug Hoyte

;; This is a "production" version of LOL with bug-fixes
;; and new features in the spirit of the book.

;; See http://letoverlambda.com

;; This is the source code for the book
;; _Let_Over_Lambda_ by Doug Hoyte.
;; This code is (C) 2002-2008, Doug Hoyte.
;;
;; You are free to use, modify, and re-distribute
;; this code however you want, except that any
;; modifications must be clearly indicated before
;; re-distribution. There is no warranty,
;; expressed nor implied.
;;
;; Attribution of this code to me, Doug Hoyte, is
;; appreciated but not necessary. If you find the
;; code useful, or would like documentation,
;; please consider buying the book!

;; Modifications by "the Phoeron" Colin J.E. Lupton, 2012--2014
;; - Support for ASDF/Quicklisp
;; - Cheap hacks to support new Backquote implementation in SBCL v1.2.2

;; Modifications by "Equwal" Spenser Truex 2018-11-17
;; - Reader macros for defmacro/g! and defmacro! since SBCL won't allow otherwise.
;;;; TODO:
;;;; Fix backquote-kludge to not silently remove backquotes from strings.
;;;; Make defmacro/g! do declarations and docstrings like it does in let-over-lambda.lisp

(in-package #:let-over-lambda)


(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (or (string= (symbol-name s)
		    "G!"
		    :start1 0
		    :end1 2)
	   #+sbcl
	   (string= (symbol-name s)
		    ",G!"
		    :start1 0
		    :end1 3))))
(defun flatten (x)
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec
		       (car x)
		       (rec (cdr x) acc))))))
    (rec x nil)))
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))
(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))
(defun o!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
		"O!"
		:start1 0
		:end1 2)))

(defun o!-symbol-to-g!-symbol (s)
  (intern (concatenate 'string
		       "G!"
		       (subseq (remove #\, (symbol-name s)) 2))))
#+sbcl
(defun prepare (str)
  (concatenate 'string (string #\() str (string #\))))
#+sbcl
(defun enclose (char)
  (cond ((char= char #\{) #\})
	((char= char #\[) #\[)
	((char= char #\() #\))
	((char= char #\<) #\>)
	(t char)))
#+sbcl
(defun termp (opening tester)
  (char= (enclose opening) tester))
#+sbcl
(defun backquote-kludge (str)
  (remove #\` str))
#+sbcl
(defmacro once-only ((&rest names) &body body)
  "A macro-writing utility for evaluating code only once."
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
	  ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
	     ,@body)))))
#+sbcl
(defmacro with-gensyms (symbols &body body)
  "Create gensyms for those symbols."
  `(let (,@(mapcar #'(lambda (sym)
		       `(,sym ',(gensym))) symbols))
     ,@body))
#+sbcl
(defun push-on (elt stack)
  (vector-push-extend elt stack) stack)
#+sbcl
(defmacro with-macro-fn (char new-fn &body body)
  (once-only (char new-fn)
    (with-gensyms (old)
      `(let ((,old (get-macro-character ,char)))
	 (progn (set-macro-character ,char ,new-fn)
		(prog1 (progn ,@body) (set-macro-character ,char ,old)))))))
#+sbcl
(defun read-atoms (str)
  (with-macro-fn #\, nil
    (flatten (read-from-string (backquote-kludge (prepare str)) nil nil))))
#+sbcl
(defun read-to-string (stream terminating-char &optional (acc (make-array 0 :adjustable t :fill-pointer 0)))
  (let ((ch (read-char stream nil nil)))
    (if (and ch (not (char= terminating-char ch)))
	(read-to-string stream terminating-char (push-on ch acc))
	(coerce acc 'string))))
#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun defmacro!-reader (stream char numarg)
    (declare (ignore char numarg))
    (let* ((str (prepare (read-to-string stream (enclose (read-char stream)))))
	   (code (read-from-string str nil))
	   (atoms (read-atoms str))
	   (os (remove-if-not #'o!-symbol-p (remove-duplicates atoms)))	   
	   (gs (mapcar #'o!-symbol-to-g!-symbol os))
	   (syms (remove-duplicates (mapcar #'(lambda (x) (intern (remove #\, (symbol-name x))))
					    (remove-if-not #'g!-symbol-p atoms))
				    :test #'(lambda (x y)
					      (string-equal (symbol-name x)
							    (symbol-name y))))))
      (let ((body (cddr code)))
	(multiple-value-bind (body declarations docstring)
	    (parse-body body :documentation t)
	  `(defmacro ,(car code) ,(cadr code)
	     ,@(when docstring
		 (list docstring))
	     ,@declarations
	     (let ,(mapcar
		    (lambda (s)
		      `(,s (gensym ,(subseq
				     (symbol-name s)
				     2))))
		    syms)
	       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
		  ,(progn ,@body)))))))))
#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-autogensym-reader (form stream)
    (let* ((str (prepare (read-to-string stream (enclose (read-char stream)))))
	   (code (read-from-string str nil))
	   (syms (remove-duplicates (mapcar #'(lambda (x) (intern (remove #\, (symbol-name x))))
					    (remove-if-not #'g!-symbol-p (read-atoms str)))
				    :test #'(lambda (x y)
					      (string-equal (symbol-name x)
							    (symbol-name y))))))
      (let ((body (cddr code)))
	(multiple-value-bind (body declarations docstring)
	    (parse-body body :documentation t)
	  `(,form ,(car code) ,(cadr code)
		  ,@(when docstring
		      (list docstring))
		  ,@declarations
		  (let ,(mapcar
			 (lambda (s)
			   `(,s (gensym ,(subseq
					  (symbol-name s)
					  2))))
			 syms)
		    ,@body)))))))
(defreadtable lol-syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\g #'(lambda (stream char numarg)
				    (declare (ignore char numarg))
				    (make-autogensym-reader 'defmacro stream)))
  (:dispatch-macro-char #\# #\n #'(lambda (stream char numarg)
					  (declare (ignore char numarg))
					  (make-autogensym-reader 'defun stream)))
  (:dispatch-macro-char #\# #\d #'defmacro!-reader))
