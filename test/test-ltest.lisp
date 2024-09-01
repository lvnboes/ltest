(defpackage :test-ltest
    (:use :cl :ltest)
    (:export :test-simple-checks
        :test-iterative-checks
        :test-double-iterative-checks
        :test-testing-levels))

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

(defun test-testing-levels ()
    (ltest:test-set
        :name "Test testing levels"
        :tests (list
            (test-assertion)
            (test-to-test-result-table))))

#|
Individual tests
 |#

;;simple checks

(defun test-check-true () 
    (ltest:test
        :name "Test check-true"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-true '= 1 1))
            (ltest:assertion
                :val (ltest:check-true '= 1 2)
                :exp nil))))

(defun test-check-false () 
    (ltest:test
        :name "Test check-false"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-false '= 1 1)
                :exp nil)
            (ltest:assertion
                :val (ltest:check-false '= 1 2)))))

;;iterative checks

(defun test-check-all-v ()
    (ltest:test
        :name "Test check-all-v"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-all-v '< (list 1 2 3) 4))
            (ltest:assertion
                :val (ltest:check-all-v '< (list 1 2 3) 2)
                :exp nil))))

(defun test-check-no-v ()
    (ltest:test
        :name "Test check-no-v"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-no-v '> (list 1 2 3) 4))
            (ltest:assertion
                :val (ltest:check-no-v '< (list 1 2 3) 2)
                :exp nil))))

(defun test-check-some-v ()
    (ltest:test
        :name "Test check-some-v"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-some-v '< (list 1 2 3) 2))
            (ltest:assertion
                :val (ltest:check-some-v '< (list 1 2 3) 0)
                :exp nil))))

(defun test-check-some-not-v ()
    (ltest:test
        :name "Test check-some-not-v"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-some-not-v '< (list 1 2 3) 2))
            (ltest:assertion
                :val (ltest:check-some-not-v '< (list 1 2 3) 4)
                :exp nil))))

(defun test-check-all-p ()
    (ltest:test
        :name "Test check-all-p"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-all-p (list 'eq '=) 2 2))
            (ltest:assertion
                :val (ltest:check-all-p (list 'eq '=) 2 2.0)
                :exp nil))))

(defun test-check-no-p ()
    (ltest:test
        :name "Test check-no-p"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-no-p (list 'eq '=) 2 3))
            (ltest:assertion
                :val (ltest:check-no-p (list 'eq '=) 2 2.0)
                :exp nil))))

(defun test-check-some-p ()
    (ltest:test
        :name "Test check-some-p"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-some-p (list 'eq '=) 2 2.0))
            (ltest:assertion
                :val (ltest:check-some-p (list 'eq '=) 2 3)
                :exp nil))))

(defun test-check-some-not-p ()
    (ltest:test
        :name "Test check-some-not-p"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-some-not-p (list 'eq '=) 2 2.0))
            (ltest:assertion
                :val (ltest:check-some-not-p (list 'eq '=) 2 2)
                :exp nil))))

(defun test-check-all-v-all-p ()
    (ltest:test
        :name "Test check-all-v-all-p"
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
        :name "check-all-v-no-p"
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
        :name "Test check-all-v-some-p"
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
        :name "Test check-all-v-some-not-p"
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
        :name "Test check-no-v-all-p"
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
        :name "Test check-no-v-no-p"
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
        :name "Test check-no-v-some-p"
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
        :name "Test check-no-v-some-not-p"
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
        :name "Test check-some-v-all-p"
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
        :name "Test check-some-v-no-p"
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
        :name "Test check-some-v-some-p"
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
        :name "Test check-some-v-some-not-p"
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
        :name "Test check-some-not-v-all-p"
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
        :name "Test check-some-not-v-no-p"
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
        :name "Test check-some-not-v-some-p"
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
        :name "Test check-some-not-v-some-not-p"
        :assertions (list
            (ltest:assertion
                :val (ltest:check-some-not-v-some-not-p (list 'equalp 'equal 'eq '=) (list 2 2 3) 2))
            (ltest:assertion
                :val (ltest:check-some-not-v-some-not-p (list 'equalp 'equal 'eq '=) (list 2 2 2) 2))
            (ltest:assertion
                :val (ltest:check-some-not-v-some-not-p (list 'equalp 'equal 'eq '=) (list 3 4 5) 2)
                :exp nil))))

(defun test-assertion ()
    (let ((pass (ltest:assertion :val t))
            (fail (ltest:assertion :val nil))
            (invalid (ltest:assertion :pred '= :val 1 :exp "test")))
        (ltest:test
            :name "Test assertion"
            :assertions (list
                (ltest:assertion
                    :check #'ltest:check-all-v
                    :val (list (listp pass)
                        (listp fail)
                        (listp invalid)))
                (ltest:assertion
                    :val (getf pass :result)
                    :exp :pass)
                (ltest:assertion
                    :val (getf fail :result)
                    :exp :fail)
                (ltest:assertion
                    :val (getf invalid :result)
                    :exp :invalid)))))

(defun test-to-test-result-table ()
    (let* ((pass-a (ltest:assertion :val t))
            (fail-a (ltest:assertion :val nil))
            (invalid-a (ltest:assertion :pred '= :val 1 :exp "test"))
            (name "name")
            (pass-t-result (make-hash-table))
            (fail-t-result (make-hash-table))
            (invalid-t-result (make-hash-table)))
        (setf (gethash :name pass-t-result) "test")
        (setf (gethash :result pass-t-result) :pass)
        (setf (gethash :pass pass-t-result) 1)
        (setf (gethash :fail pass-t-result) 0)
        (setf (gethash :invalid pass-t-result) 0)
        (setf (gethash :assertions pass-t-result) (list pass-a))
        (setf (gethash :name fail-t-result) "test")
        (setf (gethash :result fail-t-result) :fail)
        (setf (gethash :pass fail-t-result) 1)
        (setf (gethash :fail fail-t-result) 1)
        (setf (gethash :invalid fail-t-result) 1)
        (setf (gethash :assertions fail-t-result) (list pass-a fail-a invalid-a))
        (setf (gethash :name invalid-t-result) "test")
        (setf (gethash :result invalid-t-result) :invalid)
        (setf (gethash :pass invalid-t-result) 1)
        (setf (gethash :fail invalid-t-result) 0)
        (setf (gethash :invalid invalid-t-result) 1)
        (setf (gethash :assertions invalid-t-result) (list pass-a invalid-a))
        (ltest:test
            :name "Test to-test-result-table"
            :assertions (list
                (ltest:assertion
                    :pred #'lpred:hash-table-equal-p
                    :val (ltest::to-test-result-table
                        (list pass-a)
                        "test")
                    :exp pass-t-result)
                (ltest:assertion
                    :pred #'lpred:hash-table-equal-p
                    :val (ltest::to-test-result-table
                        (list pass-a fail-a invalid-a)
                        "test")
                    :exp fail-t-result)
                (ltest:assertion
                    :pred #'lpred:hash-table-equal-p
                    :val (ltest::to-test-result-table
                        (list pass-a invalid-a)
                        "test")
                    :exp invalid-t-result)))))