(defpackage :test-lpred
    (:use :cl :ltest :lpred)
    (:export :test-custom-p))

(in-package :test-lpred)

(defun test-custom-p ()
    (ltest:test-set
        :name "Test custom predicates"
        :tests (list
            (test-hash-table-equal-p)
            (test-hash-table-not-equal-p)
        )
    ))

(defun test-hash-table-equal-p ()
    (let ((hash-table-1 (make-hash-table))
            (hash-table-2 (make-hash-table))
            (hash-table-3 (make-hash-table))
            (hash-table-4 (make-hash-table))
            (hash-table-5 (make-hash-table)))
        (setf (gethash :value-1 hash-table-1) 1)
        (setf (gethash :value-2 hash-table-1) :an-atom)
        (setf (gethash :value-3 hash-table-1) "A string")
        (setf (gethash :value-1 hash-table-2) 1)
        (setf (gethash :value-2 hash-table-2) :an-atom)
        (setf (gethash :value-3 hash-table-2) "A string")
        (setf (gethash :value-1 hash-table-3) :an-atom)
        (setf (gethash :value-2 hash-table-3) "A string")
        (setf (gethash :value-3 hash-table-3) 1)
        (setf (gethash :value-1 hash-table-4) 2)
        (setf (gethash :value-2 hash-table-4) :an-other-atom)
        (setf (gethash :value-3 hash-table-4) "An other string")
        (setf (gethash :value-a hash-table-5) 1)
        (setf (gethash :value-b hash-table-5) :an-atom)
        (setf (gethash :value-c hash-table-5) "A string")
        (ltest:test
            :name "Test hash-table-equal-p"
            :assertions (list 
                (ltest:assertion
                    :val (lpred:hash-table-equal-p hash-table-1 hash-table-2))
                (ltest:assertion
                    :val (lpred:hash-table-equal-p hash-table-1 hash-table-3)
                    :exp nil)
                (ltest:assertion
                    :val (lpred:hash-table-equal-p hash-table-1 hash-table-4)
                    :exp nil)
                (ltest:assertion
                    :val (lpred:hash-table-equal-p hash-table-1 hash-table-5)
                    :exp nil)))))

(defun test-hash-table-not-equal-p ()
    (let ((hash-table-1 (make-hash-table))
            (hash-table-2 (make-hash-table))
            (hash-table-3 (make-hash-table))
            (hash-table-4 (make-hash-table))
            (hash-table-5 (make-hash-table)))
        (setf (gethash :value-1 hash-table-1) 1)
        (setf (gethash :value-2 hash-table-1) :an-atom)
        (setf (gethash :value-3 hash-table-1) "A string")
        (setf (gethash :value-1 hash-table-2) 1)
        (setf (gethash :value-2 hash-table-2) :an-atom)
        (setf (gethash :value-3 hash-table-2) "A string")
        (setf (gethash :value-1 hash-table-3) :an-atom)
        (setf (gethash :value-2 hash-table-3) "A string")
        (setf (gethash :value-3 hash-table-3) 1)
        (setf (gethash :value-1 hash-table-4) 2)
        (setf (gethash :value-2 hash-table-4) :an-other-atom)
        (setf (gethash :value-3 hash-table-4) "An other string")
        (setf (gethash :value-a hash-table-5) 1)
        (setf (gethash :value-b hash-table-5) :an-atom)
        (setf (gethash :value-c hash-table-5) "A string")
        (ltest:test
            :name "Test hash-table-not-equal-p"
            :assertions (list 
                (ltest:assertion
                    :val (lpred:hash-table-not-equal-p hash-table-1 hash-table-2)
                    :exp nil)
                (ltest:assertion
                    :val (lpred:hash-table-not-equal-p hash-table-1 hash-table-3))
                (ltest:assertion
                    :val (lpred:hash-table-not-equal-p hash-table-1 hash-table-4))
                (ltest:assertion
                    :val (lpred:hash-table-not-equal-p hash-table-1 hash-table-5))))))
    