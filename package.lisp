;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LET-OVER-LAMBDA; Base: 10 -*- file: package.lisp

(defpackage let-over-lambda
  (:nicknames lol)
  (:use cl cl-user cl-ppcre)
  (:import-from #:alexandria
                #:parse-body)
  (:import-from #:named-readtables
                #:defreadtable
                #:in-readtable)
  (:export #:lol-syntax
           #:mkstr
           #:symb
           #:group
           #:flatten
           #:fact
           #:choose
           #:g!-symbol-p
           #:defmacro/g!
           #:o!-symbol-p
           #:o!-symbol-to-g!-symbol
           #:defmacro!
           #:defun!
           #:|#"-reader|
           #:segment-reader
           #:match-mode-ppcre-lambda-form
           #:subst-mode-ppcre-lambda-form
           #:|#~-reader|
           #:dlambda
           #:alambda
           #:aif
           #:|#`-reader|
           #:|#f-reader|
           #:nlet-tail
           #:alet%
           #:alet
           #:it
           #:this
           #:self
           #:let-binding-transform
           #:pandoriclet
           #:pandoriclet-get
           #:pandoriclet-set
           #:get-pandoric
           #:with-pandoric
           #:pandoric-hotpatch
           #:pandoric-recode
           #:plambda
           #:pandoric-eval
           #:fast-progn
           #:safe-progn
           #:fformat
           #:make-tlist
           #:tlist-left
           #:tlist-right
           #:tlist-empty-p
           #:tlist-add-left
           #:tlist-add-right
           #:tlist-rem-left
           #:tlist-update
           #:build-batcher-sn
           #:sortf
           #:dollar-symbol-p
           #:prune-if-match-bodies-from-sub-lexical-scope
           #:if-match
           #:when-match))

;; EOF
