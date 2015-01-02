;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LET-OVER-LAMBDA; Base: 10 -*- file: let-over-lambda.lisp

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

;; Safety feature for SBCL>=v1.2.2

(in-package let-over-lambda)
(in-readtable lol.rt:lol-syntax)

(defmacro! dlambda (&rest ds)
  `(lambda (&rest ,g!-args)
     (case (car ,g!-args)
       ,@(mapcar
           (lambda (d)
             `(,(if (eq t (car d))
                  t
                  (list (car d)))
               (apply (lambda ,@(cdr d))
                      ,(if (eq t (car d))
                         g!-args
                         `(cdr ,g!-args)))))
           ds))))

(defmacro alet% (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     this))

(defmacro alet (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))

(defun let-binding-transform (bs)
  (if bs
    (cons
      (cond ((symbolp (car bs))
              (list (car bs)))
            ((consp (car bs))
              (car bs))
            (t
              (error "Bad let bindings")))
      (let-binding-transform (cdr bs)))))

(defmacro pandoriclet (letargs &rest body)
  (let ((letargs (cons
                   '(this)
                   (let-binding-transform
                     letargs))))
    `(let (,@letargs)
       (setq this ,@(last body))
       ,@(butlast body)
       (dlambda
         (:pandoric-get (sym)
           ,(pandoriclet-get letargs))
         (:pandoric-set (sym val)
           ,(pandoriclet-set letargs))
         (t (&rest args)
           (apply this args))))))

(defun pandoriclet-get (letargs)
  `(case sym
     ,@(mapcar #`((,(car a1)) ,(car a1))
               letargs)
     (t (error
          "Unknown pandoric get: ~a"
          sym))))

(defun pandoriclet-set (letargs)
  `(case sym
     ,@(mapcar #`((,(car a1))
                   (setq ,(car a1) val))
               letargs)
     (t (error
          "Unknown pandoric set: ~a"
          sym))))

(declaim (inline get-pandoric))

(defun get-pandoric (box sym)
  (funcall box :pandoric-get sym))

(defsetf get-pandoric (box sym) (val)
  `(progn
     (funcall ,box :pandoric-set ,sym ,val)
     ,val))

(defmacro with-pandoric (syms box &rest body)
  (let ((g!-box (gensym "box")))
    `(let ((,g!-box ,box))
       (declare (ignorable ,g!-box))
       (symbol-macrolet
         (,@(mapcar #`(,a1 (get-pandoric ,g!-box ',a1))
                    syms))
         ,@body))))

(defun pandoric-hotpatch (box new)
  (with-pandoric (this) box
    (setq this new)))

(defmacro pandoric-recode (vars box new)
  `(with-pandoric (this ,@vars) ,box
     (setq this ,new)))

(defmacro plambda (largs pargs &rest body)
  (let ((pargs (mapcar #'list pargs)))
    `(let (this self)
       (setq
         this (lambda ,largs ,@body)
         self (dlambda
                (:pandoric-get (sym)
                  ,(pandoriclet-get pargs))
                (:pandoric-set (sym val)
                  ,(pandoriclet-set pargs))
                (t (&rest args)
                  (apply this args)))))))

(defvar pandoric-eval-tunnel)

(defmacro pandoric-eval (vars expr)
  `(let ((pandoric-eval-tunnel
           (plambda () ,vars t)))
     (eval `(with-pandoric
              ,',vars pandoric-eval-tunnel
              ,,expr))))

;; Chapter 7


(defmacro fast-progn (&rest body)
  `(locally #f ,@body))

(defmacro safe-progn (&rest body)
  `(locally #0f ,@body))

(defun fformat (&rest all)
  (apply #'format all))

(define-compiler-macro fformat
                       (&whole form
                        stream fmt &rest args)
  (if (constantp fmt)
    (if stream
      `(funcall (formatter ,fmt)
         ,stream ,@args)
      (let ((g!-stream (gensym "stream")))
        `(with-output-to-string (,g!-stream)
           (funcall (formatter ,fmt)
             ,g!-stream ,@args))))
    form))

(declaim (inline make-tlist tlist-left
                 tlist-right tlist-empty-p))

(defun make-tlist () (cons nil nil))
(defun tlist-left (tl) (caar tl))
(defun tlist-right (tl) (cadr tl))
(defun tlist-empty-p (tl) (null (car tl)))

(declaim (inline tlist-add-left
                 tlist-add-right))

(defun tlist-add-left (tl it)
  (let ((x (cons it (car tl))))
    (if (tlist-empty-p tl)
      (setf (cdr tl) x))
    (setf (car tl) x)))

(defun tlist-add-right (tl it)
  (let ((x (cons it nil)))
    (if (tlist-empty-p tl)
      (setf (car tl) x)
      (setf (cddr tl) x))
    (setf (cdr tl) x)))

(declaim (inline tlist-rem-left))

(defun tlist-rem-left (tl)
  (if (tlist-empty-p tl)
    (error "Remove from empty tlist")
    (let ((x (car tl)))
      (setf (car tl) (cdar tl))
      (if (tlist-empty-p tl)
        (setf (cdr tl) nil)) ;; For gc
      (car x))))

(declaim (inline tlist-update))

(defun tlist-update (tl)
  (setf (cdr tl) (last (car tl))))

(defun build-batcher-sn (n)
  (let* (network
         (tee (ceiling (log n 2)))
         (p (ash 1 (- tee 1))))
    (loop while (> p 0) do
      (let ((q (ash 1 (- tee 1)))
            (r 0)
            (d p))
        (loop while (> d 0) do
          (loop for i from 0 to (- n d 1) do
            (if (= (logand i p) r)
              (push (list i (+ i d))
                    network)))
          (setf d (- q p)
                q (ash q -1)
                r p)))
      (setf p (ash p -1)))
    (nreverse network)))

(defmacro! sortf (comparator &rest places)
  (if places
    `(tagbody
       ,@(mapcar
           #`(let ((,g!-a #1=,(nth (car a1) places))
                   (,g!-b #2=,(nth (cadr a1) places)))
               (if (,comparator ,g!-b ,g!-a)
                 (setf #1# ,g!-b
                       #2# ,g!-a)))
           (build-batcher-sn (length places))))))

;;;;;; NEW CODE FOR ANTIWEB
(defun dollar-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 1)
       (string= (symbol-name s)
                "$"
                :start1 0
                :end1 1)
       (parse-integer (subseq (symbol-name s) 1) :junk-allowed t)))

(defun prune-if-match-bodies-from-sub-lexical-scope (tree)
  (if (consp tree)
      (if (or (eq (car tree) 'if-match)
              (eq (car tree) 'when-match))
          (cddr tree)
          (cons (prune-if-match-bodies-from-sub-lexical-scope (car tree))
                (prune-if-match-bodies-from-sub-lexical-scope (cdr tree))))
      tree))

;; WARNING: Not %100 correct. Removes forms like (... if-match ...) from the
;; sub-lexical scope even though this isn't an invocation of the macro.
(defmacro! if-match ((test o!-str) conseq &optional altern)
  (let ((dollars (remove-duplicates
                   (remove-if-not
                    #'dollar-symbol-p
                    (flatten conseq
                     ;;(prune-if-match-bodies-from-sub-lexical-scope conseq)
                     )))))
    (let ((top (or (car (sort (mapcar #'dollar-symbol-p dollars) #'>)) 0)))
      `(multiple-value-bind (,g!-s ,g!-e ,g!-ms ,g!-me) (,test ,o!-str)
        (declare (ignorable ,g!-e ,g!-me))
        (if ,g!-s
            (if (< (length ,g!-ms) ,top)
                (error "ifmatch: too few matches")
                (let ,(mapcar #`(,(symb "$" a1) (subseq ,o!-str
                                                        (aref ,g!-ms ,(1- a1))
                                                        (aref ,g!-me ,(1- a1))))
                              (loop for i from 1 to top collect i))
                  ,conseq))
            ,altern)))))

(defmacro when-match ((test str) conseq &rest more-conseq)
  `(if-match (,test ,str)
     (progn ,conseq ,@more-conseq)))
(in-package ut)

(lol:if-match (#~m/(a)bc/ "abc")
          (progn
            (p "um" $1)
            (lol:if-match (#~m/abc/ "abc")
                      (p "dois" $1)
                      'foi))
          'foi)


(defmacro! ifmatch ((test str) then &optional else)
  "Checks for the existence of group-capturing regex (in /for(bar)baz/, bar is capured) in the TEST and bind $1, $2, $n vars to the captured regex. Obviously, doesn't work with runtime regexes"
  (let* ((regexp (second (third test)))
         (how-many-$-vars (when (stringp regexp)
                            (let ((regex-paretheses
                                   (ppcre:all-matches-as-strings "\\((.*?)\\)"
                                                                 regexp)))
                              (print regex-paretheses)
                              (length regex-paretheses))))
         ($-vars-let-form
          (append `((|$`| (first ,g!-match-list))
                    ($&   (second ,g!-match-list))
                    (|$'| (third ,g!-match-list)))
                  (when how-many-$-vars
                    (mapcar (lambda (var-num)
                              `(,(symbolicate "$"
                                              (write-to-string var-num))
                                 (aref ,g!-arr ,(1- var-num))))
                            (loop for i from 1 to how-many-$-vars collect i))))))
    `(multiple-value-bind (,g!-matches ,g!-arr) (,test ,str)
       (if (plusp (length ,g!-matches))
           (let* ((,g!-match-list (ppcre:split (format nil "(~a)" ,g!-matches)
                                           ,str :with-registers-p t :limit 3))
                  ,@$-vars-let-form)

             (declare (ignorable ,@(mapcar #'car $-vars-let-form)))
             ,then)
           ,else))))

;; EOF
