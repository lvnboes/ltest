(defpackage :test-ltest
    (:use :cl :ltest)
    (:export :test-simple-checks
        :test-iterative-checks
        :test-double-iterative-checks))

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

(defun test-double-iterative-checks ()
    (ltest:test-set
        :name "Many on many checks"
        :tests (list
            (test-check-all-v-all-p)
            (test-check-all-v-no-p)
            (test-check-all-v-some-p)
            (test-check-all-v-some-not-p)
            (test-check-no-v-all-p)
            (test-check-no-v-no-p)
            (test-check-no-v-some-p)
            (test-check-no-v-some-not-p)
            (test-check-some-v-all-p)
            (test-check-some-v-no-p)
            (test-check-some-v-some-p)
            (test-check-some-v-some-not-p)
            (test-check-some-not-v-all-p)
            (test-check-some-not-v-no-p)
            (test-check-some-not-v-some-p)
            (test-check-some-not-v-some-not-p))))

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

(defun test-check-all-v-all-p ()
    (ltest:test
        :name "Test all v all p"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-all-v-all-p (list 'equalp 'equal 'eq '=) (list 2 2 2) 2))
            (ltest:assertion
                :val (ltest:check-all-v-all-p (list 'equalp 'equal 'eq '=) (list 2 2 2) 2.0)
                :exp nil)
            (ltest:assertion
                :val (ltest:check-all-v-all-p (list 'equalp 'equal 'eq '=) (list 2 3 2) 2)
                :exp nil))))

(defun test-check-all-v-no-p ()
    (ltest:test
        :name "Test all v no p"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-all-v-no-p (list 'equalp 'equal 'eq '=) (list 2 2 2) 3))
            (ltest:assertion
                :val (ltest:check-all-v-no-p (list 'equalp 'equal 'eq '=) (list 2 2 2) 2.0)
                :exp nil)
            (ltest:assertion
                :val (ltest:check-all-v-no-p (list 'equalp 'equal 'eq '=) (list 2 3 2) 2)
                :exp nil))))

(defun test-check-all-v-some-p ()
    (ltest:test
        :name "Test all v some p"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-all-v-some-p (list 'equalp 'equal 'eq '=) (list 2 2 2) 2.0))
            (ltest:assertion
                :val (ltest:check-all-v-some-p (list 'equalp 'equal 'eq '=) (list 2 2 2) 1.0)
                :exp nil)
            (ltest:assertion
                :val (ltest:check-all-v-some-p (list 'equalp 'equal 'eq '=) (list 2 3 2) 2)
                :exp nil))))

(defun test-check-all-v-some-not-p ()
    (ltest:test
        :name "Test all v some not p"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-all-v-some-not-p (list 'equalp 'equal 'eq '=) (list 2 2 2) 2.0))
            (ltest:assertion
                :val (ltest:check-all-v-some-not-p (list 'equalp 'equal 'eq '=) (list 2 3 3) 2)
                :exp nil)
            (ltest:assertion
                :val (ltest:check-all-v-some-not-p (list 'equalp 'equal 'eq '=) (list 2 2 2) 2)
                :exp nil))))

(defun test-check-no-v-all-p ()
    (ltest:test
        :name "Test no v all p"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-no-v-all-p (list 'equalp 'equal 'eq '=) (list 2.0 3 1) 2))
            (ltest:assertion
                :val (ltest:check-no-v-all-p (list 'equalp 'equal 'eq '=) (list 2 3 4) 2)
                :exp nil)
            (ltest:assertion
                :val (ltest:check-no-v-all-p (list 'equalp 'equal 'eq '=) (list 2.0 2.0 2) 2)
                :exp nil))))

(defun test-check-no-v-no-p ()
    (ltest:test
        :name "Test no v no p"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-no-v-no-p (list 'equalp 'equal 'eq '=) (list 2 2 2) 2))
            (ltest:assertion
                :val (ltest:check-no-v-no-p (list 'equalp 'equal 'eq '=) (list 3 4 5) 2.0)
                :exp nil)
            (ltest:assertion
                :val (ltest:check-no-v-no-p (list 'equalp 'equal 'eq '=) (list 2 3 2) 2)
                :exp nil))))

(defun test-check-no-v-some-p ()
    (ltest:test
        :name "Test no v some p"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-no-v-some-p (list 'equalp 'equal 'eq '=) (list 2 2 2) 3))
            (ltest:assertion
                :val (ltest:check-no-v-some-p (list 'equalp 'equal 'eq '=) (list 2 2 2) 2.0)
                :exp nil)
            (ltest:assertion
                :val (ltest:check-no-v-some-p (list 'equalp 'equal 'eq '=) (list 2.0 3 4) 2)
                :exp nil))))

(defun test-check-no-v-some-not-p ()
    (ltest:test
        :name "Test no v some not p"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-no-v-some-not-p (list 'equalp 'equal 'eq '=) (list 2 2 2) 2))
            (ltest:assertion
                :val (ltest:check-no-v-some-not-p (list 'equalp 'equal 'eq '=) (list 2 3 4) 2)
                :exp nil)
            (ltest:assertion
                :val (ltest:check-no-v-some-not-p (list 'equalp 'equal 'eq '=) (list 2 2 2) 2.0)
                :exp nil))))

(defun test-check-some-v-all-p ()
    (ltest:test
        :name "Test some v all p"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-some-v-all-p (list 'equalp 'equal 'eq '=) (list 2.0 3 2) 2))
            (ltest:assertion
                :val (ltest:check-some-v-all-p (list 'equalp 'equal 'eq '=) (list 2.0 3 4) 2)
                :exp nil)
            (ltest:assertion
                :val (ltest:check-some-v-all-p (list 'equalp 'equal 'eq '=) (list 3 4 5) 2)
                :exp nil))))

(defun test-check-some-v-no-p ()
    (ltest:test
        :name "Test some v no p"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-some-v-no-p (list 'equalp 'equal 'eq '=) (list 3 2 2) 2))
            (ltest:assertion
                :val (ltest:check-some-v-no-p (list 'equalp 'equal 'eq '=) (list 2 2 2.0) 2.0)
                :exp nil)
            (ltest:assertion
                :val (ltest:check-some-v-no-p (list 'equalp 'equal 'eq '=) (list 2 2 2) 2)
                :exp nil))))

(defun test-check-some-v-some-p ()
    (ltest:test
        :name "Test some v some p"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-some-v-some-p (list 'equalp 'equal 'eq '=) (list 2 2 2) 2))
            (ltest:assertion
                :val (ltest:check-some-v-some-p (list 'equalp 'equal 'eq '=) (list 2 2 2) 3)
                :exp nil)
            (ltest:assertion
                :val (ltest:check-some-v-some-p (list 'equalp 'equal 'eq '=) (list 3 4 5) 2)
                :exp nil))))

(defun test-check-some-v-some-not-p ()
    (ltest:test
        :name "Test some v some not p"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-some-v-some-not-p (list 'equalp 'equal 'eq '=) (list 2 2 2.0) 2))
            (ltest:assertion
                :val (ltest:check-some-v-some-not-p (list 'equalp 'equal 'eq '=) (list 2 2 2) 2)
                :exp nil)
            (ltest:assertion
                :val (ltest:check-some-v-some-not-p (list 'equalp 'equal 'eq '=) (list 2 2 2) 2.0)
                :exp t))))




(defun test-check-some-not-v-all-p ()
    (ltest:test
        :name "Test some not v all p"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-some-not-v-all-p (list 'equalp 'equal 'eq '=) (list 2.0 3 2) 2))
            (ltest:assertion
                :val (ltest:check-some-not-v-all-p (list 'equalp 'equal 'eq '=) (list 2 2 2) 2)
                :exp nil)
            (ltest:assertion
                :val (ltest:check-some-not-v-all-p (list 'equalp 'equal 'eq '=) (list 3 4 5) 2)))))

(defun test-check-some-not-v-no-p ()
    (ltest:test
        :name "Test some not v no p"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-some-not-v-no-p (list 'equalp 'equal 'eq '=) (list 3 4 2) 2))
            (ltest:assertion
                :val (ltest:check-some-not-v-no-p (list 'equalp 'equal 'eq '=) (list 3 4 5) 2)
                :exp nil)
            (ltest:assertion
                :val (ltest:check-some-not-v-no-p (list 'equalp 'equal 'eq '=) (list 2 2 2.0) 2)))))

(defun test-check-some-not-v-some-p ()
    (ltest:test
        :name "Test some not v some p"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-some-not-v-some-p (list 'equalp 'equal 'eq '=) (list 3 4 5) 2))
            (ltest:assertion
                :val (ltest:check-some-not-v-some-p (list 'equalp 'equal 'eq '=) (list 2 4 5) 2))
            (ltest:assertion
                :val (ltest:check-some-not-v-some-p (list 'equalp 'equal 'eq '=) (list 2 2 2) 2)
                :exp nil))))

(defun test-check-some-not-v-some-not-p ()
    (ltest:test
        :name "Test some-not v some not p"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-some-not-v-some-not-p (list 'equalp 'equal 'eq '=) (list 2 2 3) 2))
            (ltest:assertion
                :val (ltest:check-some-not-v-some-not-p (list 'equalp 'equal 'eq '=) (list 2 2 2) 2))
            (ltest:assertion
                :val (ltest:check-some-not-v-some-not-p (list 'equalp 'equal 'eq '=) (list 3 4 5) 2)
                :exp nil))))