(defpackage :test-ltest
    (:use :cl :ltest)
    (:export :test-simple-checks))

(in-package :test-ltest)

(defun test-simple-checks ()
    (ltest:test-set
        :name "Simple one on one checks"
        :tests (list
            (test-check-true)
            (test-check-false))))

(defun test-check-true () 
    (ltest:test
        :name "Test check-true"
        :assertions (list
            (ltest:assertion
                :check #'ltest:check-true
                :pred #'eq
                :val (ltest:check-true '= 1 1)
                :exp t)
            (ltest:assertion
                :check #'ltest:check-true
                :pred #'eq
                :val (ltest:check-true '= 1 2)
                :exp nil))))

(defun test-check-false () 
    (ltest:test
        :name "Test check-false"
        :assertions (list
            (ltest:assertion
                :check #'ltest:check-true
                :pred #'eq
                :val (ltest:check-false '= 1 1)
                :exp nil)
            (ltest:assertion
                :check #'ltest:check-true
                :pred #'eq
                :val (ltest:check-false '= 1 2)
                :exp t))))