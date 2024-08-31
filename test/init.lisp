(load "./lib/ltest/init.lisp")
(load "./test/test-colour.lisp")
(load "./test/test-lpred.lisp")
(load "./test/test-ltest.lisp")
(load "./test/unit-tests.lisp")


(unit-tests:ltest-test-suite)
(ltest:to-file
    :file-name "test-results.txt"
    :ltest-fn #'unit-tests:ltest-test-suite)