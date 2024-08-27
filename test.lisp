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
        (ltest:test-set
            "Equal-less-greater operators tests"
            (ltest:assert-=-v 1 2 "eq-op-test-01")
            (ltest:assert-=-v 1 1 "eq-op-test-02")
            (ltest:assert-=-v 1 "abc" "eq-op-test-03")
            (ltest:assert-= 1 2 "eq-op-test-04")
            (ltest:assert-= 1 1 "eq-op-test-05")
            (ltest:assert-= 1 "abc" "eq-op-test-06")
            (ltest:assert-/=-v 1 2 "eq-op-test-07")
            (ltest:assert-/=-v 1 1 "eq-op-test-08")
            (ltest:assert-/=-v 1 "abc" "eq-op-test-09")
            (ltest:assert-/= 1 2 "eq-op-test-10")
            (ltest:assert-/= 1 1 "eq-op-test-11")
            (ltest:assert-/= 1 "abc" "eq-op-test-12")
            (ltest:assert->-v 1 2 "eq-op-test-13")
            (ltest:assert->-v 2 1 "eq-op-test-14")
            (ltest:assert->-v 1 "abc" "eq-op-test-15")
            (ltest:assert-> 1 2 "eq-op-test-16")
            (ltest:assert-> 2 1 "eq-op-test-17")
            (ltest:assert-> 1 "abc" "eq-op-test-18")
            (ltest:assert->=-v 1 2 "eq-op-test-19")
            (ltest:assert->=-v 2 1 "eq-op-test-20")
            (ltest:assert->=-v 1 "abc" "eq-op-test-21")
            (ltest:assert->= 1 2 "eq-op-test-22")
            (ltest:assert->= 2 1 "eq-op-test-23")
            (ltest:assert->= 1 "abc" "eq-op-test-24")
            (ltest:assert-<-v 1 2 "eq-op-test-25")
            (ltest:assert-<-v 2 1 "eq-op-test-26")
            (ltest:assert-<-v 1 "abc" "eq-op-test-27")
            (ltest:assert-< 1 2 "eq-op-test-28")
            (ltest:assert-< 2 1 "eq-op-test-29")
            (ltest:assert-< 1 "abc" "eq-op-test-30")
            (ltest:assert-<=-v 1 2 "eq-op-test-31")
            (ltest:assert-<=-v 2 1 "eq-op-test-32")
            (ltest:assert-<=-v 1 "abc" "eq-op-test-33")
            (ltest:assert-<= 1 2 "eq-op-test-34")
            (ltest:assert-<= 2 1 "eq-op-test-35")
            (ltest:assert-<= 1 "abc" "eq-op-test-36")
            (ltest:assert-eq-v 1 2 "eq-op-test-37")
            (ltest:assert-eq-v 1 1 "eq-op-test-38")
            (ltest:assert-eq-v 1 "abc" "eq-op-test-39")
            (ltest:assert-eq 1 2 "eq-op-test-40")
            (ltest:assert-eq 1 1 "eq-op-test-41")
            (ltest:assert-eq 1 "abc" "eq-op-test-42")
            (ltest:assert-not-eq-v 1 2 "eq-op-test-43")
            (ltest:assert-not-eq-v 1 1 "eq-op-test-44")
            (ltest:assert-not-eq-v 1 "abc" "eq-op-test-45")
            (ltest:assert-not-eq 1 2 "eq-op-test-46")
            (ltest:assert-not-eq 1 1 "eq-op-test-47")
            (ltest:assert-not-eq 1 "abc" "eq-op-test-48")
            (ltest:assert-eql-v 1 2 "eq-op-test-49")
            (ltest:assert-eql-v 1 1 "eq-op-test-50")
            (ltest:assert-eql-v 1 "abc" "eq-op-test-51")
            (ltest:assert-eql 1 2 "eq-op-test-52")
            (ltest:assert-eql 1 1 "eq-op-test-53")
            (ltest:assert-eql 1 "abc" "eq-op-test-54")
            (ltest:assert-not-eql-v 1 2 "eq-op-test-55")
            (ltest:assert-not-eql-v 1 1 "eq-op-test-56")
            (ltest:assert-not-eql-v 1 "abc" "eq-op-test-57")
            (ltest:assert-not-eql 1 2 "eq-op-test-58")
            (ltest:assert-not-eql 1 1 "eq-op-test-59")
            (ltest:assert-not-eql 1 "abc" "eq-op-test-60")
            (ltest:assert-equal-v 1 2 "eq-op-test-61")
            (ltest:assert-equal-v 1 1 "eq-op-test-62")
            (ltest:assert-equal-v 1 "abc" "eq-op-test-63")
            (ltest:assert-equal 1 2 "eq-op-test-64")
            (ltest:assert-equal 1 1 "eq-op-test-65")
            (ltest:assert-equal 1 "abc" "eq-op-test-66")
            (ltest:assert-not-equal-v 1 2 "eq-op-test-67")
            (ltest:assert-not-equal-v 1 1 "eq-op-test-68")
            (ltest:assert-not-equal-v 1 "abc" "eq-op-test-69")
            (ltest:assert-not-equal 1 2 "eq-op-test-70")
            (ltest:assert-not-equal 1 1 "eq-op-test-71")
            (ltest:assert-not-equal 1 "abc" "eq-op-test-72")
            (ltest:assert-equalp-v 1 2 "eq-op-test-73")
            (ltest:assert-equalp-v 1 1 "eq-op-test-74")
            (ltest:assert-equalp-v 1 "abc" "eq-op-test-75")
            (ltest:assert-equalp 1 2 "eq-op-test-76")
            (ltest:assert-equalp 1 1 "eq-op-test-77")
            (ltest:assert-equalp 1 "abc" "eq-op-test-78")
            (ltest:assert-not-equalp-v 1 2 "eq-op-test-79")
            (ltest:assert-not-equalp-v 1 1 "eq-op-test-80")
            (ltest:assert-not-equalp-v 1 "abc" "eq-op-test-81")
            (ltest:assert-not-equalp 1 2 "eq-op-test-82")
            (ltest:assert-not-equalp 1 1 "eq-op-test-83")
            (ltest:assert-not-equalp 1 "abc" "eq-op-test-84"))))
