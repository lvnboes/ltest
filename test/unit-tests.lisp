(load "./lib/ltest/ltest-load.lisp")
(load "./test/test-colour.lisp")
(load "./test/test-ltest.lisp")

(ltest:test-suite
    :name "Ltest self test"
    :test-sets (list
        (test-colour:test-colours)
        ))