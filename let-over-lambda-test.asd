;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LET-OVER-LAMBDA; Base: 10 -*- file: let-over-lambda.asd

(in-package :cl-user)

(defpackage let-over-lambda-test-asd
  (:use :cl :asdf))

(in-package :let-over-lambda-test-asd)

(defsystem #:let-over-lambda-test
  :serial t
  :description "The test code for Let Over Lambda."
  :version #.*lol-version*
  :license "BSD Simplified"
  :depends-on (#:let-over-lambda
               #:prove)
  :components ((:module "t"
                :components
                ((:test-file "let-over-lambda"))))

  :defsystem-depends-on (prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
