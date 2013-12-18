let-over-lambda
===============

Doug Hoyte's "Production" version of macros from Let Over Lambda, ready for ASDF and Quicklisp.

Read more about the book and code at: http://letoverlambda.com

Includes minor modifications by Jorge Gajon <gajon@gajon.org>.

Tested with SBCL 1.1.7+ on Linux and OS X.

Usage
-----

Make sure you have the latest Quicklisp distribution, then include it as a dependency in your system definition, or from the REPL evaluate `(ql:quickload "let-over-lambda")`.

    * (ql:quickload "let-over-lambda")
    
    * (lol:flatten '((A . B) (C . D) (E . (F G H (I . J) . K))))
    
    (A B C D E F G H I J K)

UPDATE 12/18/2013
-----------------

Now available in the December 2013 distribution of Quicklisp
