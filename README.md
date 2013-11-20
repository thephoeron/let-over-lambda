let-over-lambda
===============

Doug Hoyte's "Production" version of macros from Let Over Lambda, ready for ASDF and Quicklisp.

Read more about the book and code at: http://letoverlambda.com

Includes minor modifications by Jorge Gajon <gajon@gajon.org>.

Tested with SBCL 1.1.7+ on Linux and OS X.

Usage
-----

Clone into `~/quicklisp/local-projects/`

    * (ql:quickload "let-over-lambda")
    
    * (lol:flatten '((A . B) (C . D) (E . (F G H (I . J) . K))))
    
    (A B C D E F G H I J K)

UPDATE 11/19/2013
-----------------

Added request for inclusion in `quicklisp-projects`.

See https://github.com/quicklisp/quicklisp-projects/issues/598 for status updates, etc
