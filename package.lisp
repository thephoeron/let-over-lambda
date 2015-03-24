;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LET-OVER-LAMBDA; Base: 10 -*- file: package.lisp

(defpackage #:let-over-lambda
  (:nicknames #:lol)
  (:use #:cl #:cl-user #:cl-ppcre)
  (:import-from #:named-readtables
                #:defreadtable
                #:in-readtable)
  (:import-from #:on-lisp
                #:symb
                #:flatten)
  (:import-from #:defmacro-enhance
                #:defmacro!)
  (:export #:dlambda
           #:alet
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
           #:if-match
           #:when-match))

(in-package #:let-over-lambda)

(cl-reexport:reexport-from :lol.rt)
