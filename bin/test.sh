#!/usr/local/bin/sbcl --script

(load "./lib/ltest/init.lisp")
(load "./test/init.lisp")
(ltest-test-suite)
