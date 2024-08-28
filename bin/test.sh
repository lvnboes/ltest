#!/usr/bin/sbcl --script

(load "./src/colour.lisp")
(load "./src/res-out.lisp")
(load "./src/ltest.lisp")
(load "./test/test.lisp")
(test:test)