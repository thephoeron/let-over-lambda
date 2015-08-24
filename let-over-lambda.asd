;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LET-OVER-LAMBDA; Base: 10 -*- file: let-over-lambda.asd

(in-package :cl-user)

(defpackage let-over-lambda-asd
  (:use :cl :asdf)
  (:export #:*lol-version*))

(in-package :let-over-lambda-asd)

(defparameter *lol-version* "1.0.1"
  "A string denoting the current version of LET-OVER-LAMBDA.  Used for diagnostic output.")

(defsystem #:let-over-lambda
  :serial t
  :description "The Production version code from letoverlambda.com, conveniently wrapped in an ASDF System for Quicklisp."
  :version #.*lol-version*
  :author "Doug Hoyte <doug@hoytech.com>"
  :maintainer "\"the Phoeron\" Colin J.E. Lupton <sysop@thephoeron.com>"
  :license "BSD Simplified"
  :depends-on (#:alexandria
               #:cl-ppcre
               #:named-readtables)
  :components ((:file "package")
               (:file "let-over-lambda"))
  :in-order-to ((test-op (test-op let-over-lambda-test))))

;; EOF
