(defpackage :test-ltest
    (:use :cl :ltest)
    (:export :test-simple-checks))

(in-package :test-ltest)

#|
Test sets
 |#

(defun test-simple-checks ()
    (ltest:test-set
        :name "Simple one on one checks"
        :tests (list
            (test-check-true)
            (test-check-false))))

(defun test-iterative-checks ()
    (ltest:test-set
        :name "Many on one or one on many checks"
        :tests (list
            (test-check-all-v)
            (test-check-no-v)
            (test-check-some-v)
            (test-check-some-not-v)
            (test-check-all-p)
            (test-check-no-p)
            (test-check-some-p)
            (test-check-some-not-p))))

#|
Individual tests
 |#


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

(defun test-check-all-v ()
    (ltest:test
        :name "Test check all values"
        :assertions (list
            (ltest:assertion
                :check #'ltest:check-true
                :pred #'eq
                :val (ltest:check-all-v '< (list 1 2 3) 4)
                :exp t)
            (ltest:assertion
                :check #'ltest:check-true
                :pred #'eq
                :val (ltest:check-all-v '< (list 1 2 3) 2)
                :exp nil))))

(defun test-check-no-v ()
    (ltest:test
        :name "Test check no values"
        :assertions (list
            (ltest:assertion
                :check #'ltest:check-true
                :pred #'eq
                :val (ltest:check-no-v '> (list 1 2 3) 4)
                :exp t)
            (ltest:assertion
                :check #'ltest:check-true
                :pred #'eq
                :val (ltest:check-no-v '< (list 1 2 3) 2)
                :exp nil))))

(defun test-check-some-v ()
    (ltest:test
        :name "Test check some values"
        :assertions (list
            (ltest:assertion
                :check #'ltest:check-true
                :pred #'eq
                :val (ltest:check-no-v '> (list 1 2 3) 4)
                :exp t)
            (ltest:assertion
                :check #'ltest:check-true
                :pred #'eq
                :val (ltest:check-no-v '< (list 1 2 3) 2)
                :exp nil))))

(defun test-check-some-not-v ()
    (ltest:test
        :name "Test check some values not"
        :assertions (list
            (ltest:assertion
                :check
                :pred
                :val
                :exp t))))
(defun test-check-all-p ()
    (ltest:test
        :name "Test check all predicates"
        :assertions (list
            (ltest:assertion
                :check
                :pred
                :val
                :exp t))))
(defun test-check-no-p ()
    (ltest:test
        :name "Test check no predicates"
        :assertions (list
            (ltest:assertion
                :check
                :pred
                :val
                :exp t))))
(defun test-check-some-p ()
    (ltest:test
        :name "Test check some predicates"
        :assertions (list
            (ltest:assertion
                :check
                :pred
                :val
                :exp t))))
(defun test-check-some-not-p ()
    (ltest:test
        :name "Test check some predicates not"
        :assertions (list
            (ltest:assertion
                :check
                :pred
                :val
                :exp t))))