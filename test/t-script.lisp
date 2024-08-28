(ltest:test
    :name "test"
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
    :name "test"
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
    :name "test"
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
