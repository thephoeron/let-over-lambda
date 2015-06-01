# LET-OVER-LAMBDA

[![Build Status](https://travis-ci.org/thephoeron/let-over-lambda.svg?branch=master)](https://travis-ci.org/thephoeron/let-over-lambda)

Doug Hoyte's "Production" version of macros from Let Over Lambda, including community updates; available from Quicklisp.

Read more about the book and code at: http://letoverlambda.com

## News &amp; Updates

##### 3/19/2015

Add symbols for anaphoric macro internals, `IT`, `THIS`, and `SELF` to package exports for better end-user experience.  Will be available in April 2015 release of Quicklisp.

##### 8/14/2014

Issue with incompatible change to backquote syntax in SBCL 1.2.2 resolved; tested against and builds on SBCL 1.2.0-1 and 1.2.2.  Will be available in the August release of Quicklisp.

##### 12/18/2013

Now available in the December 2013 distribution of Quicklisp

## Usage

Make sure you have the latest Quicklisp distribution, then include it as a dependency in your system definition, or from the REPL evaluate `(ql:quickload "let-over-lambda")`.

```lisp
(ql:quickload "let-over-lambda")
(lol:flatten '((A . B) (C . D) (E . (F G H (I . J) . K))))
=> (A B C D E F G H I J K)
```

LET-OVER-LAMBDA now uses the `named-readtables` library instead of modifying the global readtable. To use LOL reader macros in your Lisp source files, you will have to add both `let-over-lambda` and `named-readtables` to your project dependencies, and the following line after your call to `in-package`, in every source file you wish to use LOL syntax:

```lisp
(named-readtables:in-readtable lol:lol-syntax)
```

## Contributors

- [Doug Hoyte](https://github.com/hoytech)
- ["the Phoeron" Colin J.E. Lupton](https://github.com/thephoeron)
- [Jorge Gajon](https://github.com/gajon)
- [André Miranda](https://github.com/EuAndreh/)
