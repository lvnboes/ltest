#!/usr/bin/sbcl --script

(load "./test/init.lisp")
(unit-tests:ltest-test-suite)
