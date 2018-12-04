(in-package cl-user)

(defpackage let-over-lambda-test
  (:use cl let-over-lambda prove)
  (:import-from named-readtables
                in-readtable))

(in-package let-over-lambda-test)

(in-readtable lol-syntax)

;; NOTE: To run this test file, execute `(asdf:test-system :let-over-lambda)' in your Lisp.

(plan 8)

#-sbcl
(defun! fn! ()
  `(let ((,g!test 123))
     ,g!test))
#+sbcl
#n{fn! ()
  `(let ((,g!test 123))
     ,g!test)}
(defmacro fn-macro ()
  (fn!))

(deftest defun!-test
  (is-expand (fn-macro)
             (LET (($TEST 123))
               $TEST)))

(defparameter flatten-list `(D (E (F ,'(G)))))

(deftest flatten-test
  (is (flatten '((A . B) (C D (E) (F (G)))))
      '(A B C D E F G)
      "FLATTEN function works as expected.")
  #-sbcl
  (is (flatten `(A B C ,flatten-list))
      '(A B C D E F G)
      "FLATTEN on quasiquotes works as expected."))

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
             '(lambda ($str) (cl-ppcre:scan-to-strings "\\w+tp://" $str))
             "#~m expands correctly.")
  (is-expand '#~s/abc/def/
             '(lambda ($str) (cl-ppcre:regex-replace-all "abc" $str "def"))
             "#~s expands correctly.")
  (is-values (#~m/abc/ "123abc")
             '("abc" #())
             "#~m runs correctly."
             :test #'equalp)
  (is (#~s/abc/def/ "Testing abc testing abc")
      "Testing def testing def"
      "#~s runs correctly."))

(deftest read-anaphor-sharp-backquote-test
  (is '#`((,a1))
      '(lambda (a1) `((,a1)))
      "SHARP-BACKQUOTE expands correctly."
      :test #'equalp)
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

(deftest |test-#""#-read-macro|
  (is #"Contains " and \."#
      "Contains \" and \\."
      "SHARP-QUOTE read macro works as expected."))

(deftest if-match-test
  (is (if-match (#~m_a(b)c_ "abc")
          $1)
      "b"
      "IF-MATCH correctly returns the single capture.")
  (is-error (if-match (#~m_a(b)c_ "abc")
                $2)
            'simple-error
            "IF-MATCH throws an error when $2 is unbound.")
  (is (if-match (#~m_a(b)c_ "def")
          $1
          :else)
      :else
      "When IF-MATCH test is false it goes to the else body.")
  (is (if-match (#~m_a(b)c_ "abc")
          (if-match (#~m_(d)(e)f_ "def")
              (list $1 $2)
              :no-second-match)
          $1)
      '("d" "e")
      "IF-MATCH works with nested IF-MATCHs.")
  (is (if-match (#~m_a(b)c_ "abc")
          (if-match (#~m_(d)(e)f_ "d ef")
              (list $1 $2)
              :no-second-match)
          $1)
      :no-second-match
      "IF-MATCH works with nested IF-MATCHs.")
  (is-error (if-match (#~m_a(b)c_ "ab c")
                (if-match (#~m_(d)(e)f_ "d ef")
                    (list $1 $2)
                    :no-second-match)
                $1)
            'simple-error
            "IF-MATCH throws an error, even when nested.")
  (is-error (if-match (#~m_a(b)c_ "ab c")
                (if-match (#~m_(d)(e)f_ "d ef")
                    (list $1 $2)
                    :no-second-match)
                $2)
            'simple-error
            "IF-MATCH throws an error, even when nested."))

(run-test-all)
