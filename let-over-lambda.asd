;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LET-OVER-LAMBDA; Base: 10 -*- file: let-over-lambda.asd

(in-package :cl-user)

(defpackage let-over-lambda-asd
  (:use :cl :asdf))

(in-package :let-over-lambda-asd)

(defvar *lol-version* "1.0"
  "A string denoting the current version of LET-OVER-LAMBDA.  Used for diagnostic output.")

(export '*lol-version*)

(defsystem #:let-over-lambda
  :serial t
  :description "The Production version code from letoverlambda.com, conveniently wrapped in an ASDF System for Quicklisp."
  :version #.*lol-version*
  :author "Doug Hoyte <doug@hoytech.com>"
  :maintainer "\"the Phoeron\" <sysop@thephoeron.com>"
  :license "BSD Simplified"
  :depends-on (#:cl-ppcre)
  :components ((:file "package")
               (:file "let-over-lambda")))

;; EOF
