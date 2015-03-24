(defpackage let-over-lambda.readtable
  (:use cl)
  (:nicknames lol.rt)
  (:import-from defmacro-enhance
                defmacro!)
  (:import-from named-readtables
                defreadtable)
  (:import-from on-lisp
                symb)
  (:export lol-syntax
           |#"-reader|
           segment-reader
           match-mode-ppcre-lambda-form
           subst-mode-ppcre-lambda-form
           |#~-reader|
           |#`-reader|
           |#f-reader|))
(in-package lol.rt)

;; Nestable suggestion from Daniel Herring
(defun |#"-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let (chars (state 'normal) (depth 1))
    (loop do
         (let ((curr (read-char stream)))
           (cond ((eq state 'normal)
                  (cond ((char= curr #\#)
                         (push #\# chars)
                         (setq state 'read-sharp))
                        ((char= curr #\")
                         (setq state 'read-quote))
                        (t
                         (push curr chars))))
                 ((eq state 'read-sharp)
                  (cond ((char= curr #\")
                         (push #\" chars)
                         (incf depth)
                         (setq state 'normal))
                        (t
                         (push curr chars)
                         (setq state 'normal))))
                 ((eq state 'read-quote)
                  (cond ((char= curr #\#)
                         (decf depth)
                         (if (zerop depth) (return))
                         (push #\" chars)
                         (push #\# chars)
                         (setq state 'normal))
                        (t
                         (push #\" chars)
                         (if (char= curr #\")
                             (setq state 'read-quote)
                             (progn
                               (push curr chars)
                               (setq state 'normal)))))))))
    (coerce (nreverse chars) 'string)))

(defun segment-reader (stream ch n)
  (if (> n 0)
    (let ((chars))
      (do ((curr (read-char stream)
                 (read-char stream)))
          ((char= ch curr))
        (push curr chars))
      (cons (coerce (nreverse chars) 'string)
            (segment-reader stream ch (- n 1))))))

(defmacro! match-mode-ppcre-lambda-form (o!-args o!-mods)
 ``(lambda (,',g!-str)
     (ppcre:scan-to-strings
       ,(if (zerop (length ,o!-mods))
          (car ,o!-args)
          (format nil "(?~a)~a" ,o!-mods (car ,o!-args)))
       ,',g!-str)))

(defmacro! subst-mode-ppcre-lambda-form (o!-args)
 ``(lambda (,',g!-str)
     (ppcre:regex-replace-all
       ,(car ,o!-args)
       ,',g!-str
       ,(cadr ,o!-args))))

(defparameter matching-delimiters
  '((#\( . #\)) (#\[ . #\]) (#\{ . #\}) (#\< . #\>)))

(defun get-pair (char)
  (or (car (rassoc char matching-delimiters))
      (cdr (assoc char matching-delimiters))
      char))

(defun |#~-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((mode-char (read-char stream)))
    (cond
      ((char= mode-char #\m)
       (match-mode-ppcre-lambda-form
        (segment-reader stream
                        (get-pair (read-char stream))
                        1)
        (coerce (loop for c = (read-char stream)
                   while (alpha-char-p c)
                   collect c
                   finally (unread-char c stream))
                'string)))
      ((char= mode-char #\s)
       (subst-mode-ppcre-lambda-form
        (segment-reader stream
                        (read-char stream)
                        2)))
      (t (error "Unknown #~~ mode character")))))

(defun |#`-reader| (stream sub-char numarg)
  (declare (ignore sub-char))
  (unless numarg (setq numarg 1))
  `(lambda ,(loop for i from 1 to numarg
               collect (symb 'a i))
     ,(funcall
       (get-macro-character #\`) stream nil)))

(defun |#f-reader| (stream sub-char numarg)
  (declare (ignore stream sub-char))
  (setq numarg (or numarg 3))
  (unless (<= numarg 3)
    (error "Bad value for #f: ~a" numarg))
  `(declare (optimize (speed ,numarg)
                      (safety ,(- 3 numarg)))))

(defreadtable lol-syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\" #'|#"-reader|)
  (:dispatch-macro-char #\# #\> #'cl-heredoc:read-heredoc)
  (:dispatch-macro-char #\# #\~ #'|#~-reader|)
  (:dispatch-macro-char #\# #\` #'|#`-reader|)
  (:dispatch-macro-char #\# #\` #'|#`-reader|)
  (:dispatch-macro-char #\# #\f #'|#f-reader|))
