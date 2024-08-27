(defpackage :test
    (:use :cl :ltest)
    (:export :test))

(in-package :test)

(defun test ()
    (ltest:test-suite
        (ltest:test-set
            "Single comparison tests"
            (ltest:assert-is-v 1 2 '< "s-comp-test-01")
            (ltest:assert-is-v 1 2 '= "s-comp-test-02")
            (ltest:assert-is-v 1 "abc" '< "s-comp-test-03")
            (ltest:assert-is 1 2 '< "s-comp-test-04")
            (ltest:assert-is 1 2 '= "s-comp-test-05")
            (ltest:assert-is 1 "abc" '< "s-comp-test-06")
            (ltest:assert-not-v 1 2 '< "s-comp-test-07")
            (ltest:assert-not-v 1 2 '= "s-comp-test-08")
            (ltest:assert-not-v 1 "abc" '< "s-comp-test-09")
            (ltest:assert-not 1 2 '< "s-comp-test-10")
            (ltest:assert-not 1 2 '= "s-comp-test-11")
            (ltest:assert-not 1 "abc" '< "s-comp-test-12"))
        (ltest:test-set
            "Multiple comparison tests"
            (ltest:assert-any-v 1 2 (list '< '=) "m-comp-test-01")
            (ltest:assert-any-v 1 2 (list '> '=) "m-comp-test-02")
            (ltest:assert-any-v 1 "abc" (list '< '=) "m-comp-test-03")
            (ltest:assert-any 1 2 (list '< '=) "m-comp-test-04")
            (ltest:assert-any 1 2 (list '> '=) "m-comp-test-05")
            (ltest:assert-any 1 "abc" (list '< '=) "m-comp-test-06")
            (ltest:assert-all-v 1 2 (list '< '=) "m-comp-test-07")
            (ltest:assert-all-v 1 2 (list '> '=) "m-comp-test-08")
            (ltest:assert-all-v 1 "abc" (list '< '=) "m-comp-test-09")
            (ltest:assert-all 1 2 (list '< '=) "m-comp-test-10")
            (ltest:assert-all 1 2 (list '> '=) "m-comp-test-11")
            (ltest:assert-all 1 "abc" (list '< '=) "m-comp-test-12")
            (ltest:assert-none-v 1 2 (list '< '=) "m-comp-test-13")
            (ltest:assert-none-v 1 2 (list '> '=) "m-comp-test-14")
            (ltest:assert-none-v 1 "abc" (list '< '=) "m-comp-test-15")
            (ltest:assert-none 1 2 (list '< '=) "m-comp-test-16")
            (ltest:assert-none 1 2 (list '> '=) "m-comp-test-17")
            (ltest:assert-none 1 "abc" (list '< '=) "m-comp-test-18"))
    ))
