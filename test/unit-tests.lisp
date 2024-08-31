(defpackage :unit-tests
    (:use :cl :test-colour :test-ltest)
    (:export :ltest-test-suite))

(in-package :unit-tests)


(defun ltest-test-suite ()
    (ltest:test-suite
        :name "Ltest self test"
        :test-sets (list
            (test-colour:test-colours)
            (test-lpred:test-custom-p)
            (test-ltest:test-simple-checks)
            (test-ltest:test-iterative-checks)
            (test-ltest:test-double-iterative-checks))))