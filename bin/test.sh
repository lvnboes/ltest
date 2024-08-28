#!/bin/bash sbcl --script

(load "./src/colour.lisp")
(load "./src/out.lisp")
(load "./src/ltest.lisp")
(load "./test/test.lisp")
(test:test)