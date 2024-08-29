(defpackage :test-ltest
    (:use :cl :ltest)
    (:export :test-simple-checks
        :test-iterative-checks))

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

;;simple checks

(defun test-check-true () 
    (ltest:test
        :name "Test check true"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-true '= 1 1))
            (ltest:assertion
                :val (ltest:check-true '= 1 2)
                :exp nil))))

(defun test-check-false () 
    (ltest:test
        :name "Test check false"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-false '= 1 1)
                :exp nil)
            (ltest:assertion
                :val (ltest:check-false '= 1 2)))))

;;iterative checks

(defun test-check-all-v ()
    (ltest:test
        :name "Test check all values"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-all-v '< (list 1 2 3) 4))
            (ltest:assertion
                :val (ltest:check-all-v '< (list 1 2 3) 2)
                :exp nil))))

(defun test-check-no-v ()
    (ltest:test
        :name "Test check no values"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-no-v '> (list 1 2 3) 4))
            (ltest:assertion
                :val (ltest:check-no-v '< (list 1 2 3) 2)
                :exp nil))))

(defun test-check-some-v ()
    (ltest:test
        :name "Test check some values"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-some-v '< (list 1 2 3) 2))
            (ltest:assertion
                :val (ltest:check-some-v '< (list 1 2 3) 0)
                :exp nil))))

(defun test-check-some-not-v ()
    (ltest:test
        :name "Test check some values not"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-some-not-v '< (list 1 2 3) 2))
            (ltest:assertion
                :val (ltest:check-some-not-v '< (list 1 2 3) 4)
                :exp nil))))

(defun test-check-all-p ()
    (ltest:test
        :name "Test check all predicates"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-all-p (list 'eq '=) 2 2))
            (ltest:assertion
                :val (ltest:check-all-p (list 'eq '=) 2 2.0)
                :exp nil))))

(defun test-check-no-p ()
    (ltest:test
        :name "Test check no predicates"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-no-p (list 'eq '=) 2 3))
            (ltest:assertion
                :val (ltest:check-no-p (list 'eq '=) 2 2.0)
                :exp nil))))

(defun test-check-some-p ()
    (ltest:test
        :name "Test check some predicates"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-some-p (list 'eq '=) 2 2.0))
            (ltest:assertion
                :val (ltest:check-some-p (list 'eq '=) 2 3)
                :exp nil))))

(defun test-check-some-not-p ()
    (ltest:test
        :name "Test check some predicates not"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-some-not-p (list 'eq '=) 2 2.0))
            (ltest:assertion
                :val (ltest:check-some-not-p (list 'eq '=) 2 2)
                :exp nil))))