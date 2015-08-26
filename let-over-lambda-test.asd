;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LET-OVER-LAMBDA-TEST-ASD; Base: 10 -*-
;;;; file: let-over-lambda-test.asd

(in-package :cl-user)

(defpackage let-over-lambda-test-asd
  (:use :cl :asdf))

(in-package :let-over-lambda-test-asd)

(defsystem #:let-over-lambda-test
  :serial t
  :version "1.0.1"
  :description "The test code for Let Over Lambda."
  :author "André Miranda <andremiramor@gmail.com>"
  :maintainer "\"the Phoeron\" Colin J.E. Lupton <sysop@thephoeron.com>"
  :license "BSD Simplified"
  :depends-on (#:let-over-lambda
               #:prove
               #:named-readtables)
  :components ((:module "t"
                :components
                ((:test-file "let-over-lambda"))))

  :defsystem-depends-on (prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))

;; EOF
