;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LET-OVER-LAMBDA; Base: 10 -*- file: let-over-lambda.asd

(in-package :cl-user)

(defpackage let-over-lambda/asdf
  (:nicknames let-over-lambda/sys lol/sys)
  (:use cl asdf uiop)
  (:export #:*lol-version*))

(in-package :let-over-lambda/asdf)

(defsystem let-over-lambda
  :description "The Production version code from letoverlambda.com, conveniently wrapped in an ASDF System for Quicklisp."
  :author "Doug Hoyte <doug@hoytech.com>"
  :maintainer "\"the Phoeron\" Colin J.E. Lupton <sysop@thephoeron.com>"
  :homepage "https://thephoeron.github.io/let-over-lambda/"
  :source-control (:git "https://github.com/thephoeron/let-over-lambda.git")
  :bug-tracker "https://github.com/thephoeron/let-over-lambda/issues"
  :version (:read-file-form "VERSION")
  :license "BSD Simplified"
  :depends-on (alexandria
               cl-ppcre
               named-readtables
               fare-quasiquote-extras)
  :serial t
  :components ((:file "package")
               (:file "let-over-lambda"))
  :in-order-to ((test-op (test-op let-over-lambda-test))))

(defparameter *lol-version* (asdf:component-version (asdf:find-system :let-over-lambda))
  "A string denoting the current version of LET-OVER-LAMBDA.  Used for diagnostic output.")

;; EOF
