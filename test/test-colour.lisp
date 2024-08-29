(defpackage :test-colour
    (:use :cl :ltest :colour)
    (:export :test-colours))

(in-package :test-colour)

(defun test-colours ()
    (ltest:test-set
        :name "Test colour add colour escape chars"
        :tests (list
            (test-black)
            (test-red)
            (test-green)
            (test-yellow)
            (test-blue)
            (test-magenta)
            (test-cyan)
            (test-white)
            (test-gray)
            (test-bright-red)
            (test-bright-green)
            (test-bright-yellow)
            (test-bright-blue)
            (test-bright-magenta)
            (test-bright-cyan)
            (test-bright-white))))

(defun test-black ()
    (ltest:test
        :name "Test colour black"
        :assertions (list
            (ltest:assertion
                :check #'ltest:check-true
                :pred '>
                :val (length (colour:black "teststring" t))
                :exp (length "teststring")
            )
            (ltest:assertion
                :check #'ltest:check-true
                :pred '=
                :val (length (colour:black "teststring" nil))
                :exp (length "teststring")))))

(defun test-red ()
    (ltest:test
        :name "Test colour red"
        :assertions (list
            (ltest:assertion
                :check #'ltest:check-true
                :pred '>
                :val (length (colour:red "teststring" t))
                :exp (length "teststring")
            )
            (ltest:assertion
                :check #'ltest:check-true
                :pred '=
                :val (length (colour:red "teststring" nil))
                :exp (length "teststring")))))

(defun test-green ()
    (ltest:test
        :name "Test colour green"
        :assertions (list
            (ltest:assertion
                :check #'ltest:check-true
                :pred '>
                :val (length (colour:green "teststring" t))
                :exp (length "teststring")
            )
            (ltest:assertion
                :check #'ltest:check-true
                :pred '=
                :val (length (colour:green "teststring" nil))
                :exp (length "teststring")))))

(defun test-yellow ()
    (ltest:test
        :name "Test colour yellow"
        :assertions (list
            (ltest:assertion
                :check #'ltest:check-true
                :pred '>
                :val (length (colour:yellow "teststring" t))
                :exp (length "teststring")
            )
            (ltest:assertion
                :check #'ltest:check-true
                :pred '=
                :val (length (colour:yellow "teststring" nil))
                :exp (length "teststring")))))

(defun test-blue ()
    (ltest:test
        :name "Test colour blue"
        :assertions (list
            (ltest:assertion
                :check #'ltest:check-true
                :pred '>
                :val (length (colour:blue "teststring" t))
                :exp (length "teststring")
            )
            (ltest:assertion
                :check #'ltest:check-true
                :pred '=
                :val (length (colour:blue "teststring" nil))
                :exp (length "teststring")))))

(defun test-magenta ()
    (ltest:test
        :name "Test colour magenta"
        :assertions (list
            (ltest:assertion
                :check #'ltest:check-true
                :pred '>
                :val (length (colour:magenta "teststring" t))
                :exp (length "teststring")
            )
            (ltest:assertion
                :check #'ltest:check-true
                :pred '=
                :val (length (colour:magenta "teststring" nil))
                :exp (length "teststring")))))

(defun test-cyan ()
    (ltest:test
        :name "Test colour cyan"
        :assertions (list
            (ltest:assertion
                :check #'ltest:check-true
                :pred '>
                :val (length (colour:cyan "teststring" t))
                :exp (length "teststring")
            )
            (ltest:assertion
                :check #'ltest:check-true
                :pred '=
                :val (length (colour:cyan "teststring" nil))
                :exp (length "teststring")))))

(defun test-white ()
    (ltest:test
        :name "Test colour white"
        :assertions (list
            (ltest:assertion
                :check #'ltest:check-true
                :pred '>
                :val (length (colour:white "teststring" t))
                :exp (length "teststring")
            )
            (ltest:assertion
                :check #'ltest:check-true
                :pred '=
                :val (length (colour:white "teststring" nil))
                :exp (length "teststring")))))

(defun test-gray ()
    (ltest:test
        :name "Test colour gray"
        :assertions (list
            (ltest:assertion
                :check #'ltest:check-true
                :pred '>
                :val (length (colour:gray "teststring" t))
                :exp (length "teststring")
            )
            (ltest:assertion
                :check #'ltest:check-true
                :pred '=
                :val (length (colour:gray "teststring" nil))
                :exp (length "teststring")))))

(defun test-bright-red ()
    (ltest:test
        :name "Test colour bright-red"
        :assertions (list
            (ltest:assertion
                :check #'ltest:check-true
                :pred '>
                :val (length (colour:bright-red "teststring" t))
                :exp (length "teststring")
            )
            (ltest:assertion
                :check #'ltest:check-true
                :pred '=
                :val (length (colour:bright-red "teststring" nil))
                :exp (length "teststring")))))

(defun test-bright-green ()
    (ltest:test
        :name "Test colour bright-green"
        :assertions (list
            (ltest:assertion
                :check #'ltest:check-true
                :pred '>
                :val (length (colour:bright-green "teststring" t))
                :exp (length "teststring")
            )
            (ltest:assertion
                :check #'ltest:check-true
                :pred '=
                :val (length (colour:bright-green "teststring" nil))
                :exp (length "teststring")))))

(defun test-bright-yellow ()
    (ltest:test
        :name "Test colour bright-yellow"
        :assertions (list
            (ltest:assertion
                :check #'ltest:check-true
                :pred '>
                :val (length (colour:bright-yellow "teststring" t))
                :exp (length "teststring")
            )
            (ltest:assertion
                :check #'ltest:check-true
                :pred '=
                :val (length (colour:bright-yellow "teststring" nil))
                :exp (length "teststring")))))

(defun test-bright-blue ()
    (ltest:test
        :name "Test colour bright-blue"
        :assertions (list
            (ltest:assertion
                :check #'ltest:check-true
                :pred '>
                :val (length (colour:bright-blue "teststring" t))
                :exp (length "teststring")
            )
            (ltest:assertion
                :check #'ltest:check-true
                :pred '=
                :val (length (colour:bright-blue "teststring" nil))
                :exp (length "teststring")))))

(defun test-bright-magenta ()
    (ltest:test
        :name "Test colour bright-magenta"
        :assertions (list
            (ltest:assertion
                :check #'ltest:check-true
                :pred '>
                :val (length (colour:bright-magenta "teststring" t))
                :exp (length "teststring")
            )
            (ltest:assertion
                :check #'ltest:check-true
                :pred '=
                :val (length (colour:bright-magenta "teststring" nil))
                :exp (length "teststring")))))

(defun test-bright-cyan ()
    (ltest:test
        :name "Test colour bright-cyan"
        :assertions (list
            (ltest:assertion
                :check #'ltest:check-true
                :pred '>
                :val (length (colour:bright-cyan "teststring" t))
                :exp (length "teststring")
            )
            (ltest:assertion
                :check #'ltest:check-true
                :pred '=
                :val (length (colour:bright-cyan "teststring" nil))
                :exp (length "teststring")))))

(defun test-bright-white ()
    (ltest:test
        :name "Test colour bright-white"
        :assertions (list
            (ltest:assertion
                :check #'ltest:check-true
                :pred '>
                :val (length (colour:bright-white "teststring" t))
                :exp (length "teststring")
            )
            (ltest:assertion
                :check #'ltest:check-true
                :pred '=
                :val (length (colour:bright-white "teststring" nil))
                :exp (length "teststring")))))