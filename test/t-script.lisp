(ltest:test
    :name "test-1"
    :assertions (list
        (ltest:assertion
            :check #'ltest:check-true
            :pred '> :val 2 :exp 1)
        (ltest:assertion
            :check #'ltest:check-true
            :pred '> :val 2 :exp 1
        )
        (ltest:assertion
            :check #'ltest:check-true
            :pred '> :val 2 :exp 1)))
(ltest:test
    :name "test-2"
    :assertions (list
        (ltest:assertion
            :check #'ltest:check-true
            :pred '> :val 2 :exp 1)
        (ltest:assertion
            :check #'ltest:check-true
            :pred '> :val 2 :exp 1
        )
        (ltest:assertion
            :check #'ltest:check-true
            :pred '> :val 2 :exp 1)))
(ltest:test
    :name "test-3"
    :assertions (list
        (ltest:assertion
            :check #'ltest:check-true
            :pred '> :val 2 :exp 1)
        (ltest:assertion
            :check #'ltest:check-true
            :pred '> :val 2 :exp 1
        )
        (ltest:assertion
            :check #'ltest:check-true
            :pred '> :val 2 :exp 1)))
(ltest:test
    :name "test-4"
    :assertions (list
        (ltest:assertion
            :check #'ltest:check-true
            :pred '> :val 2 :exp 1)
        (ltest:assertion
            :check #'ltest:check-true
            :pred '> :val 1 :exp 2
        )
        (ltest:assertion
            :check #'ltest:check-true
            :pred '> :val 2 :exp "abc")))
(ltest:test
    :name "test-5"
    :assertions (list
        (ltest:assertion
            :check #'ltest:check-true
            :pred '> :val 2 :exp 1)
        (ltest:assertion
            :check #'ltest:check-true
            :pred '> :val 2 :exp 1
        )
        (ltest:assertion
            :check #'ltest:check-true
            :pred '> :val 2 :exp 1)))
(ltest:test
    :name "test-6"
    :assertions (list
        (ltest:assertion
            :check #'ltest:check-true
            :pred '> :val 2 :exp 1)
        (ltest:assertion
            :check #'ltest:check-true
            :pred '> :val 2 :exp 1
        )
        (ltest:assertion
            :check #'ltest:check-true
            :pred '> :val 2 :exp "abc")))
(ltest:test
    :name "test-7"
    :assertions (list
        (ltest:assertion
            :check #'ltest:check-true
            :pred '> :val 2 :exp 1)
        (ltest:assertion
            :check #'ltest:check-true
            :pred '> :val 2 :exp 1
        )
        (ltest:assertion
            :check #'ltest:check-true
            :pred '> :val 2 :exp 1)))
