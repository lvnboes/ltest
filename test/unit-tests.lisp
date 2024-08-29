(load "./lib/ltest/ltest-load.lisp")
(load "./test/test-colour.lisp")
(load "./test/test-ltest.lisp")

(defun ltest-test-suite ()
    (ltest:test-suite
        :name "Ltest self test"
        :test-sets (list
            (test-colour:test-colours)
            (test-ltest:test-simple-checks)
            (test-ltest:test-iterative-checks))))

(ltest-test-suite)

#|
Wrap (ltest:to-file) around any test suite, test set or 
    individual test to write its output to a file

(ltest:to-file
    :file-name "test.txt"
    :ltests #'ltest-test-suite)
 |#