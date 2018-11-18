;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LET-OVER-LAMBDA; Base: 10 -*- file: let-over-lambda.lisp

;;;; Copyleft Spenser Truex "Equwal" 2018
;;;; This file for automatic gensym reader macros on SBCL, since SBCL doesn't support it otherwise.

;;;; TODO:
;;;; Fix backquote-kludge to not silently remove backquotes from strings.
;;;; Make defmacro/g! do declarations and docstrings like it does in let-over-lambda.lisp

(in-package #:let-over-lambda)
#+sbcl
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun prepare (str)
    (concatenate 'string (string #\() str (string #\))))
  (defun enclose (char)
    (cond ((char= char #\{) #\})
	  ((char= char #\[) #\[)
	  ((char= char #\() #\))
	  ((char= char #\<) #\>)
	  (t char)))
  (defun termp (opening tester)
    (char= (enclose opening) tester))
  (defun backquote-kludge (str)
    (remove #\` str))
  (defmacro once-only ((&rest names) &body body)
    "A macro-writing utility for evaluating code only once."
    (let ((gensyms (loop for n in names collect (gensym))))
      `(let (,@(loop for g in gensyms collect `(,g (gensym))))
	 `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
	    ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
	       ,@body)))))
  (defmacro with-gensyms (symbols &body body)
    "Create gensyms for those symbols."
    `(let (,@(mapcar #'(lambda (sym)
			 `(,sym ',(gensym))) symbols))
       ,@body))
  (defun push-on (elt stack)
    (vector-push-extend elt stack) stack)
  (defmacro with-macro-fn (char new-fn &body body)
    (once-only (char new-fn)
      (with-gensyms (old)
	`(let ((,old (get-macro-character ,char)))
	   (progn (set-macro-character ,char ,new-fn)
		  (prog1 (progn ,@body) (set-macro-character ,char ,old)))))))
  (defun read-atoms (str)
    (with-macro-fn #\, nil
      (flatten (read-from-string (backquote-kludge (prepare str)) nil nil))))
  (defun read-to-string (stream terminating-char &optional (acc (make-array 0 :adjustable t :fill-pointer 0)))
    (let ((ch (read-char stream nil nil)))
      (if (and ch (not (char= terminating-char ch)))
	  (read-to-string stream terminating-char (push-on ch acc))
	  (concatenate 'string acc))))
  (defun defmacro/g!-reader (stream char numarg)
    (declare (ignore char numarg))
    (let* ((str (prepare (read-to-string stream (enclose (read-char stream)))))
	   (code (read-from-string str nil))
	   (syms (remove-duplicates (mapcar #'(lambda (x) (intern (remove #\, (symbol-name x))))
					    (remove-if-not #'g!-symbol-p (read-atoms str)))
				    :test #'(lambda (x y)
					      (string-equal (symbol-name x)
							    (symbol-name y))))))
      (let ((name (car code))
	    (args (cadr code))
	    (body (cddr code)))
	`(defmacro ,name ,args
	   (let (,@(loop for x in syms
		      collect (list x '(gensym))))
	     ,@body))))))
  ;; (multiple-value-bind (body declarations docstring)
  ;;     (parse-body ,body :documentation t)
  ;;   `(defmacro ,name ,args
  ;;      ,@(when docstring
  ;; 	   (list docstring))
  ;;      ,@declarations
  ;;      (let ,(mapcar
  ;; 	      (lambda (s)
  ;; 		`(,s (gensym ,(subseq
  ;; 			       (symbol-name s)
  ;; 			       2))))
  ;; 	      ,',syms)
  ;; ,@body))))))))
#+sbcl
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defmacro defmacro! (name args &rest body)
    (let* ((os (remove-if-not #'o!-symbol-p (flatten args)))
	   (gs (mapcar #'o!-symbol-to-g!-symbol os)))
      (multiple-value-bind (body declarations docstring)
	  (parse-body body :documentation t)
	`#g{,name ,args
	,@(when docstring
	    (list docstring))
	,@declarations
	`(let ,(mapcar #'list (list ,@gs) (list ,@os))
	   ,(progn ,@body))}))))
#+sbcl
(set-dispatch-macro-character #\# #\g #'defmacro/g!-reader)
  ;; Expansion
  ;; (defmacro name (y)
  ;;   (let ((g!x (gensym)))
  ;;      `(let ((,g!x ,y))
  ;; 	(list ,g!x ,g!x))))
  ;; Clean test case
					;#g{name (z) `(let ((,g!y ,z)) (list ,g!y ,g!y))}
					;(set-macro-character #\d #'g! t)
  ;; (defmacro defmacro/g! (name args &rest body)
  ;;   (let ((syms (remove-duplicates
  ;; 	       (remove-if-not #'g!-symbol-p
  ;; 			      (flatten body)))))
  ;;     `(defmacro ,name ,args
  ;;        (let ,(mapcar
  ;; 	      (lambda (s)
  ;; 		`(,s (gensym ,(subseq
  ;; 			       (symbol-name s)
  ;; 			       2))))
  ;; 	      syms)
  ;;          ,@body))))
  ;; (set-dispatch-character)
  ;; (defmacro/g! test () `(list ,g!a))
