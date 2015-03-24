(in-package cl-user)
(defpackage let-over-lambda-test
  (:use cl let-over-lambda prove)
  (:import-from named-readtables
                in-readtable))
(in-package let-over-lambda-test)
(in-readtable lol-syntax)

;; NOTE: To run this test file, execute `(asdf:test-system :let-over-lambda)' in your Lisp.

(plan 5)

(deftest |test-#""#-read-macro|
  (is  #"Contains " and \."#
   "Contains \" and \\." "SHARP-QUOTE read macro works as expected."   ))

(defparameter heredoc-string #>END
I can put anything here: ", , "# and ># are
no problem. The only thing that will terminate
the reading of this string is...END)

(deftest heredoc-read-macro-test
  (is heredoc-string
      "I can put anything here: \", , \"# and ># are
no problem. The only thing that will terminate
the reading of this string is..."
      "SHARP-GREATER-THEN read macro works as expected."))

(deftest pilfered-perl-regex-syntax-test
  (is-expand '#~m|\w+tp://|
             '(lambda ($str) (cl-ppcre:scan "\\w+tp://" $str))
             "#~m expands correctly.")
  (is-expand '#~s/abc/def/
             '(lambda ($str) (cl-ppcre:regex-replace-all "abc" $str "def"))
             "#~s expands correctly.")
  (is-values (#~m/abc/ "123abc")
             '(3 6 #() #())
             "#~m runs correctly."
             :test #'equalp)
  (is (#~s/abc/def/ "Testing abc testing abc")
      "Testing def testing def"
      "#~s runs correctly."))

(deftest read-anaphor-sharp-backquote-test
  (is '#`((,a1))
      '(lambda (a1) `((,a1))
        :test #'equalp)
      "SHARP-BACKQUOTE expands correctly.")
  (is-expand #.(#3`(((,@a2)) ,a3 (,a1 ,a1))
                (gensym)
                '(a b c)
                'hello)
             (((a b c)) hello ($g $g))
             "SHARP-BACKQUOTE runs correctly, respecting order, gensyms, nesting, numarg, etc."))

(deftest sharp-f-test
  (is '#f
      '(declare (optimize (speed 3) (safety 0)))
      "Default numarg SHARP-F expands correctly.")
  (is '#0f
      '(declare (optimize (speed 0) (safety 3)))
      "Numarg = 3 SHARP-F expands correctly.")
  (is '(#1f #2f)
      '((declare (optimize (speed 1) (safety 2)))
        (declare (optimize (speed 2) (safety 1))))
      "SHARP-F correctly expands into rarely used compiler options."))

(run-test-all)
