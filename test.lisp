(defpackage :test
    (:use :cl :ltest)
    (:export :test))

(in-package :test)

(defun test ()
    (ltest:test-set
        "test-set-name"
        (ltest:assert-is-v 1 2 '< "comp-test-01")
        (ltest:assert-is-v 1 2 '= "comp-test-02")
        (ltest:assert-is-v 1 "abc" '< "comp-test-03")
        (ltest:assert-is 1 2 '< "comp-test-04")
        (ltest:assert-is 1 2 '= "comp-test-05")
        (ltest:assert-is 1 "abc" '< "comp-test-06")))
