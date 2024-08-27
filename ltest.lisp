(defpackage :ltest
    (:use :cl :colour)
    (:export :test-set :test-suite :to-output
        :compare-v :compare-each-v :compare-some-v :compare-no-v
        :compare :compare-each :compare-some :compare-no
        :compare-not-v :compare-each-not-v :compare-some-not-v :compare-no-not-v
        :compare-not :compare-each-not :compare-some-not :compare-no-not
        :compare-t-v :compare-each-t-v :compare-some-t-v :compare-no-t-v
        :compare-t :compare-each-t :compare-some-t :compare-no-t
        :compare-nil-v :compare-each-nil-v :compare-some-nil-v :compare-no-nil-v
        :compare-nil :compare-each-nil :compare-some-nil :compare-no-nil
        :compare-any-v :compare-each-any-v :compare-some-any-v :compare-no-any-v
        :compare-any :compare-each-any :compare-some-any :compare-no-any
        :compare-any-not-v :compare-each-any-not-v :compare-some-any-not-v :compare-no-any-not-v
        :compare-any-not :compare-each-any-not :compare-some-any-not :compare-no-any-not
        :compare-any-t-v :compare-each-any-t-v :compare-some-any-t-v :compare-no-any-t-v
        :compare-any-t :compare-each-any-t :compare-some-any-t :compare-no-any-t
        :compare-any-nil-v :compare-each-any-nil-v :compare-some-any-nil-v :compare-no-any-nil-v
        :compare-any-nil :compare-each-any-nil :compare-some-any-nil :compare-no-any-nil

        :compare-all-v 
        :compare-all 

        :compare-none-v 
        :compare-none

        :assert-is-v :assert-is :assert-not-v :assert-not :assert-any-v 
        :assert-any :assert-all-v :assert-all :assert-none-v :assert-none
        :assert-=-v :assert-= :assert-/=-v :assert-/= :assert->-v :assert-> 
        :assert->=-v :assert->= :assert-<-v :assert-< :assert-<=-v :assert-<=
        :assert-eq-v :assert-eq :assert-not-eq-v :assert-not-eq
        :assert-eql-v :assert-eql :assert-not-eql-v :assert-not-eql
        :assert-equal-v :assert-equal :assert-not-equal-v :assert-not-equal
        :assert-equalp-v :assert-equalp :assert-not-equalp-v :assert-not-equalp
        :assert-string=-v :assert-string= :assert-string/=-v :assert-string/=
        :assert-string>-v :assert-string> :assert-string>=-v :assert-string>=
        :assert-string<-v :assert-string< :assert-string<=-v :assert-string<=
        :assert-string-equal-v :assert-string-equal :assert-string-not-equal-v 
        :assert-string-not-equal :assert-string-greaterp-v :assert-string-greaterp 
        :assert-string-greaterorequalp-v :ssert-string-greaterorequalp
        :assert-string-lessp-v :assert-string-lessp :assert-string-lessorequalp-v 
        :assert-string-lessorequalp
        :assert-char=-v :assert-char= :assert-char/=-v :assert-char/=
        :assert-char>-v :assert-char> :assert-char>=-v :assert-char>=
        :assert-char<-v :assert-char< :assert-char<=-v :assert-char<=
        :assert-char-equal-v :assert-char-equal :assert-char-not-equal-v 
        :assert-char-not-equal :assert-char-greaterp-v :assert-char-greaterp 
        :assert-char-greaterorequalp-v :ssert-char-greaterorequalp :assert-char-lessp-v 
        :assert-char-lessp :assert-char-lessorequalp-v :assert-char-lessorequalp))

(in-package :ltest)

(defparameter *output-file* nil)

#|
Wrapper to optonally write to a specific output file
 |#

(defun to-output (&key test-suite output-file)
    (let ((result nil))
        (if output-file
            (progn
                (setf *output-file* output-file)
                (with-open-file (stream *output-file*
                    :direction :output
                    :if-exists :overwrite
                    :if-does-not-exist :create)
                    (let ((*standard-output* stream))
                        (setf result (eval test-suite)))))
        (setf result (eval test-suite)))
        result
    ))

#|
Helper functions to print out test results
 |#

(defun insert-padding (str1 str2 &optional (len 100) (pad-char #\.))
    (let* ((len1 (length str1))
            (len2 (length str2))
            (len-padding (max (- len (+ len1 len2)) 0))
            (padding-str (make-string len-padding :element-type 'character :initial-element pad-char)))
        (format nil "~a~a~a" str1 padding-str str2)))
 
(defun pass (f-name var1 var2 &key (predicates nil) (test-name nil)) 
    (let* ((pred-str (if predicates (format nil " ~{~a~^ ~} |" predicates) ""))
            (test-name-str (if test-name (format nil "~a | " test-name) ""))
            (result-str (format nil
                "~%~a~a |~a ~a ~a | ~a ~a "
                test-name-str f-name pred-str
                (type-of var1) var1 (type-of var2) var2)))
        (format t (colour:bright-green 
            (insert-padding result-str " [PASS]") 
            *output-file*))
        :pass))

(defun fail (f-name var1 var2 &key (predicates nil) (test-name nil)) 
    (let* ((pred-str (if predicates (format nil " ~{~a~^ ~} |" predicates) ""))
            (test-name-str (if test-name (format nil "~a | " test-name) ""))
            (result-str (format nil
                "~%~a~a |~a ~a ~a | ~a ~a "
                test-name-str f-name pred-str
                (type-of var1) var1 (type-of var2) var2)))
        (format t (colour:bright-red 
            (insert-padding result-str " [FAIL]")
            *output-file*))
        :fail))
    

(defun invalid (f-name var1 var2 &key (predicates nil) (test-name nil)) 
    (let* ((pred-str (if predicates (format nil " ~{~a~^ ~} |" predicates) ""))
            (test-name-str (if test-name (format nil "~a | " test-name) ""))
            (result-str (format nil
                "~%~a~a |~a ~a ~a | ~a ~a "
                test-name-str f-name pred-str
                (type-of var1) var1 (type-of var2) var2)))
        (format t (colour:bright-yellow 
            (insert-padding result-str " [INVD]")
            *output-file*))
        :invalid))

#|
Helper functions to execute tests as a set and print out the set result
 |#

(defun show-test-set-result (name result-hash-table)
    (let* ((passed (gethash :pass result-hash-table 0))
            (failed (gethash :fail result-hash-table 0))
            (invalid (gethash :invalid result-hash-table 0))
            (total (+ failed passed invalid)))
        (format t 
            "~%Results of test set: ~a~%~\t~\tPASSED: ~a~\t~\tFAILED: ~a~\t~\tINVALID: ~a~%~\t~\tTOTAL: ~a TESTS~%~%"
            name passed failed invalid total)
        result-hash-table))

(defun test-set-execute (tests result-acc)
    (if (null tests)
        result-acc
        (progn 
            (incf (gethash (car tests) result-acc 0))
            (test-set-execute (cdr tests) result-acc))))

(defun test-set (&key (name "Unnamed test set") (tests '()))
    (let ((results (test-set-execute tests (make-hash-table))))
        (show-test-set-result name results)))

#|
Helper functions to execute test-sets as part of a test-suite
 |#

 (defun show-test-suite-result (result-hash-table) 
    (let* ((passed (gethash :pass result-hash-table 0))
            (failed (gethash :fail result-hash-table 0))
            (invalid (gethash :invalid result-hash-table 0))
            (total (+ failed passed invalid))
            (sets (gethash :sets result-hash-table 0))
            (all-passed? (eq (+ failed invalid) 0))
            (result-str
                (format nil
                    "~%FINAL RESULT OF ~a TEST SETS IN SUITE~%~\t~\tPASSED: ~a~\t~\tFAILED: ~a~\t~\tINVALID: ~a~%~\t~\tTOTAL: ~a TESTS~%~%"
                    sets passed failed invalid total)))
        (format t (if all-passed? 
            (colour:bright-green 
                result-str 
                *output-file*) 
            (colour:bright-red 
                result-str 
                *output-file*)))
        all-passed?))

 (defun test-suite-execute (test-sets result-acc)
    (if (null test-sets) 
        result-acc
        (let* ((set (car test-sets))
                (set-pass (gethash :pass set 0))
                (set-fail (gethash :fail set 0))
                (set-invalid (gethash :invalid set 0))
                (suite-pass (gethash :pass result-acc 0))
                (suite-fail (gethash :fail result-acc 0))
                (suite-invalid (gethash :invalid result-acc 0)))
            (progn 
                (setf (gethash :pass result-acc) (+ suite-pass set-pass))
                (setf (gethash :fail result-acc) (+ suite-fail set-fail))
                (setf (gethash :invalid result-acc) (+ suite-invalid set-invalid))
                (incf (gethash :sets result-acc 0))
                (test-suite-execute (cdr test-sets) result-acc)))))

 (defun test-suite (&key test-sets) 
    (show-test-suite-result (test-suite-execute test-sets (make-hash-table))))

#|
Basic comparison functions with and without validity check
    With -v: results in INVALID if value doesn't match predicate
    Without -v: results in FAIL if value doesn't match predicate
 |#

 ;; iteration helpers

 (defun do-each (var-list exp pred)
    (cond ((null var-list) t)
        ((funcall pred (car var-list) exp) (do-each (cdr var-list) exp pred))
        (t nil)))

 (defun do-some (var-list exp pred)
    (cond ((null var-list) nil)
        ((funcall pred (car var-list) exp) t)
        (t (do-each (cdr var-list) exp pred))))

 (defun do-some-not (var-list exp pred)
    (cond ((null var-list) nil)
        ((funcall pred (car var-list) exp) (do-some-not (cdr var-list) exp pred))
        (t t)))

 (defun do-no (var-list exp pred)
    (cond ((null var-list) t)
        ((funcall pred (car var-list) exp) nil)
        (t (do-no (cdr var-list) exp pred))))

;; comparison functions

(defun compare-v (val1 val2 pred)
    (handler-case
        (if (funcall pred val1 val2) 'pass 'fail)
        (error () 'invalid)))

(defun compare-each-v (var-list val2 pred)
    (handler-case
        (if (do-each var-list val2 pred) 'pass 'fail)
        (error () 'invalid)))

(defun compare-some-v (var-list val2 pred)
    (handler-case
        (if (do-some var-list val2 pred) 'pass 'fail)
        (error () 'invalid)))

(defun compare-no-v (var-list val2 pred)
    (handler-case
        (if (do-no var-list val2 pred) 'pass 'fail)
        (error () 'invalid)))

(defun compare (val1 val2 pred)
    (handler-case
        (if (funcall pred val1 val2) 'pass 'fail)
        (error () 'fail)))

(defun compare-each (var-list val2 pred)
    (handler-case
        (if (do-each var-list val2 pred) 'pass 'fail)
        (error () 'fail)))

(defun compare-some (var-list val2 pred)
    (handler-case
        (if (do-some var-list val2 pred) 'pass 'fail)
        (error () 'fail)))

(defun compare-no (var-list val2 pred)
    (handler-case
        (if (do-no var-list val2 pred) 'pass 'fail)
        (error () 'fail)))

(defun compare-not-v (val1 val2 pred)
    (handler-case
        (if (funcall pred val1 val2) 'fail 'pass)
        (error () 'invalid)))

(defun compare-each-not-v (var-list val2 pred)
    (handler-case
        (if (do-no var-list val2 pred) 'pass 'fail)
        (error () 'invalid)))

(defun compare-some-not-v (var-list val2 pred)
    (handler-case
        (if (do-some-not var-list val2 pred) 'pass 'fail)
        (error () 'invalid)))

(defun compare-no-not-v (var-list val2 pred)
    (handler-case
        (if (do-each var-list val2 pred) 'pass 'fail)
        (error () 'invalid)))

(defun compare-not (val1 val2 pred)
    (handler-case
        (if (funcall pred val1 val2) 'fail 'pass)
        (error () 'fail)))

(defun compare-each-not (var-list val2 pred)
    (handler-case
        (if (do-no var-list val2 pred) 'pass 'fail)
        (error () 'fail)))

(defun compare-some-not (var-list val2 pred)
    (handler-case
        (if (do-some-not var-list val2 pred) 'pass 'fail)
        (error () 'fail)))

(defun compare-no-not (var-list val2 pred)
    (handler-case
        (if (do-each var-list val2 pred) 'pass 'fail)
        (error () 'fail)))

(defun compare-t-v (val pred)
    (compare-v val t pred))

(defun compare-each-t-v (var-list pred)
    (compare-each-v var-list t pred))

(defun compare-some-t-v (var-list pred)
    (compare-some-v var-list t pred))

(defun compare-no-t-v (var-list pred)
    (compare-no-v var-list t pred))

(defun compare-t (val pred)
    (compare-v val t pred))

(defun compare-each-t (var-list pred)
    (compare-each var-list t pred))

(defun compare-some-t (var-list pred)
    (compare-some var-list t pred))

(defun compare-no-t (var-list pred)
    (compare-no var-list t pred))

(defun compare-nil-v (val pred)
    (compare-v val nil pred))

(defun compare-each-nil-v (var-list pred)
    (compare-each-v var-list t pred))

(defun compare-some-nil-v (var-list pred)
    (compare-some-v var-list t pred))

(defun compare-no-nil-v (var-list pred)
    (compare-no-v var-list t pred))

(defun compare-nil (val pred)
    (compare val nil pred))

(defun compare-each-nil (var-list pred)
    (compare-each var-list nil pred))

(defun compare-some-nil (var-list pred)
    (compare-some var-list nil pred))

(defun compare-no-nil (var-list pred)
    (compare-no var-list nil pred))

(defun compare-any-v (val1 val2 p-list)
    (handler-case
        (cond ((null p-list) 'fail)
            ((funcall (car p-list) val1 val2) 'pass)
            (t (compare-any-v val1 val2 (cdr p-list))))
        (error () 'invalid)))

(defun compare-each-any-v (var-list val2 p-list)
    (handler-case
        (cond ((null p-list) 'fail)
            ((do-each var-list val2 (car p-list)) 'pass)
            (t (compare-each-any-v var-list val2 (cdr p-list))))
        (error () 'invalid)))

(defun compare-some-any-v (var-list val2 p-list)
    (handler-case
        (cond ((null p-list) 'fail)
            ((do-some var-list val2 (car p-list)) 'pass)
            (t (compare-some-any-v var-list val2 (cdr p-list))))
        (error () 'invalid)))

(defun compare-no-any-v (var-list val2 p-list)
    (handler-case
        (cond ((null p-list) 'fail)
            ((do-no var-list val2 (car p-list)) 'pass)
            (t (compare-no-any-v var-list val2 (cdr p-list))))
        (error () 'invalid)))

(defun compare-any (val1 val2 p-list)
    (handler-case
        (cond ((null p-list) 'fail)
            ((funcall (car p-list) val1 val2) 'pass)
            (t (compare-any val1 val2 (cdr p-list))))
        (error () (compare-any val1 val2 (cdr p-list)))))

(defun compare-each-any (var-list val2 p-list)
    (handler-case
        (cond ((null p-list) 'fail)
            ((do-each var-list val2 (car p-list)) 'pass)
            (t (compare-each-any var-list val2 (cdr p-list))))
        (error () 'fail)))

(defun compare-some-any (var-list val2 p-list)
    (handler-case
        (cond ((null p-list) 'fail)
            ((do-some var-list val2 (car p-list)) 'pass)
            (t (compare-some-any var-list val2 (cdr p-list))))
        (error () 'fail)))

(defun compare-no-any (var-list val2 p-list)
    (handler-case
        (cond ((null p-list) 'fail)
            ((do-no var-list val2 (car p-list)) 'pass)
            (t (compare-no-any var-list val2 (cdr p-list))))
        (error () 'fail)))

(defun compare-any-not-v (val1 val2 p-list)
    (handler-case
        (cond ((null p-list) 'fail)
            ((funcall (car p-list) val1 val2) (compare-any-not-v val1 val2 (cdr p-list)))
            (t 'pass))
        (error () 'invalid)))

(defun compare-each-any-not-v (val1 val2 p-list)
    (handler-case
        (cond ((null p-list) 'fail)
            ((do-each val1 val2 (car p-list)) (compare-each-any-not-v val1 val2 (cdr p-list)))
            (t 'pass))
        (error () 'invalid)))

(defun compare-some-any-not-v (val1 val2 p-list)
    (handler-case
        (cond ((null p-list) 'fail)
            ((do-some val1 val2 (car p-list)) (compare-some-any-not-v val1 val2 (cdr p-list)))
            (t 'pass))
        (error () 'invalid)))

(defun compare-no-any-not-v (val1 val2 p-list)
    (handler-case
        (cond ((null p-list) 'fail)
            ((do-no val1 val2 (car p-list)) (compare-no-any-not-v val1 val2 (cdr p-list)))
            (t 'pass))
        (error () 'invalid)))

(defun compare-any-not (val1 val2 p-list)
    (handler-case
        (cond ((null p-list) 'fail)
            ((funcall (car p-list) val1 val2) (compare-any-not val1 val2 (cdr p-list)))
            (t 'pass))
        (error () 'fail)))

(defun compare-each-any-not (val1 val2 p-list)
    (handler-case
        (cond ((null p-list) 'fail)
            ((do-each val1 val2 (car p-list)) (compare-each-any-not val1 val2 (cdr p-list)))
            (t 'pass))
        (error () 'fail)))

(defun compare-some-any-not (val1 val2 p-list)
    (handler-case
        (cond ((null p-list) 'fail)
            ((do-some val1 val2 (car p-list)) (compare-some-any-not val1 val2 (cdr p-list)))
            (t 'pass))
        (error () 'fail)))

(defun compare-no-any-not (val1 val2 p-list)
    (handler-case
        (cond ((null p-list) 'fail)
            ((do-no val1 val2 (car p-list)) (compare-no-any-not val1 val2 (cdr p-list)))
            (t 'pass))
        (error () 'fail)))

(defun compare-any-t-v (val pred)
    (compare-any-v val t pred))

(defun compare-each-any-t-v (var-list pred)
    (compare-each-any-v var-list t pred))

(defun compare-some-any-t-v (var-list pred)
    (compare-some-any-v var-list t pred))

(defun compare-no-any-t-v (var-list pred)
    (compare-no-any-v var-list t pred))

(defun compare-any-t (val pred)
    (compare-any val t pred))

(defun compare-each-any-t (var-list pred)
    (compare-each-any var-list t pred))

(defun compare-some-any-t (var-list pred)
    (compare-some-any var-list t pred))

(defun compare-no-any-t (var-list pred)
    (compare-no-any var-list t pred))

(defun compare-any-nil-v (val pred)
    (compare-any-v val nil pred))

(defun compare-each-any-nil-v (var-list pred)
    (compare-each-any-v var-list nil pred))

(defun compare-some-any-nil-v (var-list pred)
    (compare-some-any-v var-list nil pred))

(defun compare-no-any-nil-v (var-list pred)
    (compare-no-any-v var-list nil pred))

(defun compare-any-nil (val pred)
    (compare-any val nil pred))

(defun compare-each-any-nil (var-list pred)
    (compare-each-any var-list nil pred))

(defun compare-some-any-nil (var-list pred)
    (compare-some-any var-list nil pred))

(defun compare-no-any-nil (var-list pred)
    (compare-no-any var-list nil pred))

(defun compare-all-v (val1 val2 p-list)
    (handler-case
        (cond ((null p-list) 'pass)
            ((funcall (car p-list) val1 val2) 
                (compare-all-v val1 val2 (cdr p-list)))
            (t 'fail))
        (error () 'invalid)))

(defun compare-all (val1 val2 p-list)
    (handler-case
        (cond ((null p-list) 'pass)
            ((funcall (car p-list) val1 val2) 
                (compare-all val1 val2 (cdr p-list)))
            (t 'fail))
        (error () 'fail)))

(defun compare-all-t-v (val pred)
    (compare-all-v val t pred))

(defun compare-all-t (val pred)
    (compare-all val t pred))

(defun compare-all-nil-v (val pred)
    (compare-all-v val nil pred))

(defun compare-all-nil (val pred)
    (compare-all val nil pred))

(defun compare-none-v (val1 val2 p-list)
    (handler-case
        (cond ((null p-list) 'pass)
            ((funcall (car p-list) val1 val2) 'fail)
            (t (compare-none-v val1 val2 (cdr p-list))))
        (error () 'invalid)))

(defun compare-none (val1 val2 p-list)
    (handler-case
        (cond ((null p-list) 'pass)
            ((funcall (car p-list) val1 val2) 'fail)
            (t (compare-none val1 val2 (cdr p-list))))
        (error () (compare-none val1 val2 (cdr p-list)))))

(defun compare-none-t-v (val pred)
    (compare-none-v val t pred))

(defun compare-none-t (val pred)
    (compare-none val t pred))

(defun compare-none-nil-v (val pred)
    (compare-none-v val nil pred))

(defun compare-none-nil (val pred)
    (compare-none val nil pred))

#|
Test functions derived from the basic comparison functions
 |#

;; validate with predicates

(defun assert-is-v (value expected predicate &optional test-name)
    (funcall (compare-v value expected predicate) 
        'assert-is-v value expected :predicates (list predicate) :test-name test-name))

(defun assert-is (value expected predicate &optional test-name)
    (funcall (compare value expected predicate) 
        'assert-is value expected :predicates (list predicate) :test-name test-name))

(defun assert-not-v (value expected predicate &optional test-name)
    (funcall (compare-not-v value expected predicate) 
        'assert-not-v value expected :predicates (list predicate) :test-name test-name))

(defun assert-not (value expected predicate &optional test-name)
    (funcall (compare-not value expected predicate) 
        'assert-not value expected :predicates (list predicate) :test-name test-name))

(defun assert-t-v (value predicate &optional test-name)
    (funcall (compare-t-v value predicate)
        'assert-t-v value :predicates (list predicate) :test-name test-name))

(defun assert-t (value predicate &optional test-name)
    (funcall (compare-t value predicate)
        'assert-t value :predicates (list predicate) :test-name test-name))

(defun assert-nil-v (value predicate &optional test-name)
    (funcall (compare-nil-v value predicate)
        'assert-nil-v value :predicates (list predicate) :test-name test-name))

(defun assert-nil (value predicate &optional test-name)
    (funcall (compare-nil value predicate)
        'assert-nil value :predicates (list predicate) :test-name test-name))

(defun assert-any-v (value expected predicates &optional test-name)
    (funcall (compare-any-v value expected predicates) 
        'assert-any-v value expected :predicates predicates :test-name test-name))

(defun assert-any (value expected predicates &optional test-name)
    (funcall (compare-any value expected predicates) 
        'assert-any value expected :predicates predicates :test-name test-name))

(defun assert-any-t-v (value predicate &optional test-name)
    (funcall (compare-any-t-v value predicate)
        'assert-any-t-v value :predicates (list predicate) :test-name test-name))

(defun assert-any-t (value predicate &optional test-name)
    (funcall (compare-any-t value predicate)
        'assert-any-t value :predicates (list predicate) :test-name test-name))

(defun assert-any-nil-v (value predicate &optional test-name)
    (funcall (compare-any-nil-v value predicate)
        'assert-any-nil-v value :predicates (list predicate) :test-name test-name))

(defun assert-any-nil (value predicate &optional test-name)
    (funcall (compare-any-nil value predicate)
        'assert-any-nil value :predicates (list predicate) :test-name test-name))

(defun assert-all-v (value expected predicates &optional test-name)
    (funcall (compare-all-v value expected predicates) 
        'assert-all-v value expected :predicates predicates :test-name test-name))

(defun assert-all (value expected predicates &optional test-name)
    (funcall (compare-all value expected predicates) 
        'assert-all value expected :predicates predicates :test-name test-name))

(defun assert-all-t-v (value predicate &optional test-name)
    (funcall (compare-all-t-v value predicate)
        'assert-all-t-v value :predicates (list predicate) :test-name test-name))

(defun assert-all-t (value predicate &optional test-name)
    (funcall (compare-all-t value predicate)
        'assert-all-t value :predicates (list predicate) :test-name test-name))

(defun assert-all-nil-v (value predicate &optional test-name)
    (funcall (compare-all-nil-v value predicate)
        'assert-all-nil-v value :predicates (list predicate) :test-name test-name))

(defun assert-all-nil (value predicate &optional test-name)
    (funcall (compare-all-nil value predicate)
        'assert-all-nil value :predicates (list predicate) :test-name test-name))

(defun assert-none-v (value expected predicates &optional test-name)
    (funcall (compare-none-v value expected predicates) 
        'assert-none-v value expected :predicates predicates :test-name test-name))

(defun assert-none (value expected predicates &optional test-name)
    (funcall (compare-none value expected predicates) 
        'assert-none value expected :predicates predicates :test-name test-name))

(defun assert-none-t-v (value predicate &optional test-name)
    (funcall (compare-none-t-v value predicate)
        'assert-none-t-v value :predicates (list predicate) :test-name test-name))

(defun assert-none-t (value predicate &optional test-name)
    (funcall (compare-none-t value predicate)
        'assert-none-t value :predicates (list predicate) :test-name test-name))

(defun assert-none-nil-v (value predicate &optional test-name)
    (funcall (compare-none-nil-v value predicate)
        'assert-none-nil-v value :predicates (list predicate) :test-name test-name))

(defun assert-none-nil (value predicate &optional test-name)
    (funcall (compare-none-nil value predicate)
        'assert-none-nil value :predicates (list predicate) :test-name test-name))

;; comparison of two values

(defun assert-=-v (value expected &optional test-name)
    (funcall (compare-v value expected '=) 
        'assert-=-v value expected :test-name test-name))

(defun assert-= (value expected &optional test-name)
    (funcall (compare value expected '=) 
        'assert-= value expected :test-name test-name))

(defun assert-/=-v (value expected &optional test-name)
    (funcall (compare-v value expected '/=) 
        'assert-/=-v value expected :test-name test-name))

(defun assert-/= (value expected &optional test-name)
    (funcall (compare value expected '/=) 
        'assert-/= value expected :test-name test-name))

(defun assert->-v (value expected &optional test-name)
    (funcall (compare-v value expected '>) 
        'assert->-v value expected :test-name test-name))

(defun assert-> (value expected &optional test-name)
    (funcall (compare value expected '>) 
        'assert-> value expected :test-name test-name))

(defun assert->=-v (value expected &optional test-name)
    (funcall (compare-v value expected '>=) 
        'assert->=-v value expected :test-name test-name))

(defun assert->= (value expected &optional test-name)
    (funcall (compare value expected '>=) 
        'assert->= value expected :test-name test-name))

(defun assert-<-v (value expected &optional test-name)
    (funcall (compare-v value expected '<) 
        'assert-<-v value expected :test-name test-name))

(defun assert-< (value expected &optional test-name)
    (funcall (compare value expected '<) 
        'assert-< value expected :test-name test-name))

(defun assert-<=-v (value expected &optional test-name)
    (funcall (compare-v value expected '<=) 
        'assert-<=-v value expected :test-name test-name))

(defun assert-<= (value expected &optional test-name)
    (funcall (compare value expected '<=) 
        'assert-<= value expected :test-name test-name))

(defun assert-eq-v (value expected &optional test-name)
    (funcall (compare-v value expected 'eq) 
        'assert-eq-v value expected :test-name test-name))

(defun assert-eq (value expected &optional test-name)
    (funcall (compare value expected 'eq) 
        'assert-eq value expected :test-name test-name))

(defun assert-not-eq-v (value expected &optional test-name)
    (funcall (compare-not-v value expected 'eq) 
        'assert-not-eq-v value expected :test-name test-name))

(defun assert-not-eq (value expected &optional test-name)
    (funcall (compare-not value expected 'eq) 
        'assert-not-eq value expected :test-name test-name))

(defun assert-eql-v (value expected &optional test-name)
    (funcall (compare-v value expected 'eql) 
        'assert-eql-v value expected :test-name test-name))

(defun assert-eql (value expected &optional test-name)
    (funcall (compare value expected 'eql) 
        'assert-eql value expected :test-name test-name))

(defun assert-not-eql-v (value expected &optional test-name)
    (funcall (compare-not-v value expected 'eql) 
        'assert-not-eql-v value expected :test-name test-name))

(defun assert-not-eql (value expected &optional test-name)
    (funcall (compare-not value expected 'eql) 
        'assert-not-eql value expected :test-name test-name))

(defun assert-equal-v (value expected &optional test-name)
    (funcall (compare-v value expected 'equal) 
        'assert-equal-v value expected :test-name test-name))

(defun assert-equal (value expected &optional test-name)
    (funcall (compare value expected 'equal) 
        'assert-equal value expected :test-name test-name))

(defun assert-not-equal-v (value expected &optional test-name)
    (funcall (compare-not-v value expected 'equal) 
        'assert-not-equal-v value expected :test-name test-name))

(defun assert-not-equal (value expected &optional test-name)
    (funcall (compare-not value expected 'equal) 
        'assert-not-equal value expected :test-name test-name))

(defun assert-equalp-v (value expected &optional test-name)
    (funcall (compare-v value expected 'equalp) 
        'assert-equalp-v value expected :test-name test-name))

(defun assert-equalp (value expected &optional test-name)
    (funcall (compare value expected 'equalp) 
        'assert-equalp value expected :test-name test-name))

(defun assert-not-equalp-v (value expected &optional test-name)
    (funcall (compare-not-v value expected 'equalp) 
        'assert-not-equalp-v value expected :test-name test-name))

(defun assert-not-equalp (value expected &optional test-name)
    (funcall (compare-not value expected 'equalp) 
        'assert-not-equalp value expected :test-name test-name))

;; comparison of two strings

(defun assert-string=-v (value expected &optional test-name)
    (funcall (compare-v value expected 'string=) 
        'assert-string=-v value expected :test-name test-name))

(defun assert-string= (value expected &optional test-name)
    (funcall (compare value expected 'string=) 
        'assert-string= value expected :test-name test-name))

(defun assert-string/=-v (value expected &optional test-name)
    (funcall (compare-v value expected 'string/=) 
        'assert-string/=-v value expected :test-name test-name))

(defun assert-string/= (value expected &optional test-name)
    (funcall (compare value expected 'string/=) 
        'assert-string/= value expected :test-name test-name))

(defun assert-string>-v (value expected &optional test-name)
    (funcall (compare-v value expected 'string>) 
        'assert-string>-v value expected :test-name test-name))

(defun assert-string> (value expected &optional test-name)
    (funcall (compare value expected 'string>) 
        'assert-string> value expected :test-name test-name))

(defun assert-string>=-v (value expected &optional test-name)
    (funcall (compare-v value expected 'string>=) 
        'assert-string>=-v value expected :test-name test-name))

(defun assert-string>= (value expected &optional test-name)
    (funcall (compare value expected 'string>=) 
        'assert-string>= value expected :test-name test-name))

(defun assert-string<-v (value expected &optional test-name)
    (funcall (compare-v value expected 'string<) 
        'assert-string<-v value expected :test-name test-name))

(defun assert-string< (value expected &optional test-name)
    (funcall (compare value expected 'string<) 
        'assert-string< value expected :test-name test-name))

(defun assert-string<=-v (value expected &optional test-name)
    (funcall (compare-v value expected 'string<=) 
        'assert-string<=-v value expected :test-name test-name))

(defun assert-string<= (value expected &optional test-name)
    (funcall (compare value expected 'string<=) 
        'assert-string<= value expected :test-name test-name))

(defun assert-string-equal-v (value expected &optional test-name)
    (funcall (compare-v value expected 'string-equal) 
        'assert-string-equal-v value expected :test-name test-name))

(defun assert-string-equal (value expected &optional test-name)
    (funcall (compare value expected 'string-equal) 
        'assert-string-equal value expected :test-name test-name))

(defun assert-string-not-equal-v (value expected &optional test-name)
    (funcall (compare-v value expected 'string-not-equal) 
        'assert-string-not-equal-v value expected :test-name test-name))

(defun assert-string-not-equal (value expected &optional test-name)
    (funcall (compare value expected 'string-not-equal) 
        'assert-string-not-equal value expected :test-name test-name))

(defun assert-string-greaterp-v (value expected &optional test-name)
    (funcall (compare-v value expected 'string-greaterp) 
        'assert-string-greaterp-v value expected :test-name test-name))

(defun assert-string-greaterp (value expected &optional test-name)
    (funcall (compare value expected 'string-greaterp) 
        'assert-string-greaterp value expected :test-name test-name))

(defun assert-string-greaterorequalp-v (value expected &optional test-name)
    (funcall (compare-any-v value expected (list 'string-equal 'string-greaterp)) 
        'assert-string-greaterorequalp-v value expected :test-name test-name))

(defun assert-string-greaterorequalp (value expected &optional test-name)
    (funcall (compare-any value expected (list 'string-equal 'string-greaterp))
        'assert-string-greaterorequalp value expected :test-name test-name))

(defun assert-string-lessp-v (value expected &optional test-name)
    (funcall (compare-v value expected 'string-lessp) 
        'assert-string-lessp-v value expected :test-name test-name))

(defun assert-string-lessp (value expected &optional test-name)
    (funcall (compare value expected 'string-lessp) 
        'assert-string-lessp value expected :test-name test-name))

(defun assert-string-lessorequalp-v (value expected &optional test-name)
    (funcall (compare-any-v value expected (list 'string-equal 'string-lessp)) 
        'assert-string-lessorequalp-v value expected :test-name test-name))

(defun assert-string-lessorequalp (value expected &optional test-name)
    (funcall (compare-any value expected (list 'string-equal 'string-lessp)) 
        'assert-string-lessorequalp value expected :test-name test-name))

;;comparison of two chars

(defun assert-char=-v (value expected &optional test-name)
    (funcall (compare-v value expected 'char=) 
        'assert-char=-v value expected :test-name test-name))

(defun assert-char= (value expected &optional test-name)
    (funcall (compare value expected 'char=) 
        'assert-char= value expected :test-name test-name))

(defun assert-char/=-v (value expected &optional test-name)
    (funcall (compare-v value expected 'char/=) 
        'assert-char/=-v value expected :test-name test-name))
(defun assert-char/= (value expected &optional test-name)
    (funcall (compare value expected 'char/=) 
        'assert-char/= value expected :test-name test-name))

(defun assert-char>-v (value expected &optional test-name)
    (funcall (compare-v value expected 'char>) 
        'assert-char>-v value expected :test-name test-name))

(defun assert-char> (value expected &optional test-name)
    (funcall (compare value expected 'char>) 
        'assert-char> value expected :test-name test-name))

(defun assert-char>=-v (value expected &optional test-name)
    (funcall (compare-v value expected 'char>=) 
        'assert-char>=-v value expected :test-name test-name))

(defun assert-char>= (value expected &optional test-name)
    (funcall (compare value expected 'char>=) 
        'assert-char>= value expected :test-name test-name))

(defun assert-char<-v (value expected &optional test-name)
    (funcall (compare-v value expected 'char<) 
        'assert-char<-v value expected :test-name test-name))

(defun assert-char< (value expected &optional test-name)
    (funcall (compare value expected 'char<) 
        'assert-char< value expected :test-name test-name))

(defun assert-char<=-v (value expected &optional test-name)
    (funcall (compare-v value expected 'char<=) 
        'assert-char<=-v value expected :test-name test-name))

(defun assert-char<= (value expected &optional test-name)
    (funcall (compare value expected 'char<=) 
        'assert-char<= value expected :test-name test-name))

(defun assert-char-equal-v (value expected &optional test-name)
    (funcall (compare-v value expected 'char-equal) 
        'assert-char-equal-v value expected :test-name test-name))

(defun assert-char-equal (value expected &optional test-name)
    (funcall (compare value expected 'char-equal) 
        'assert-char-equal value expected :test-name test-name))

(defun assert-char-not-equal-v (value expected &optional test-name)
    (funcall (compare-v value expected 'char-not-equal) 
        'assert-char-not-equal-v value expected :test-name test-name))

(defun assert-char-not-equal (value expected &optional test-name)
    (funcall (compare value expected 'char-not-equal) 
        'assert-char-not-equal value expected :test-name test-name))

(defun assert-char-greaterp-v (value expected &optional test-name)
    (funcall (compare-v value expected 'char-greaterp) 
        'assert-char-greaterp-v value expected :test-name test-name))

(defun assert-char-greaterp (value expected &optional test-name)
    (funcall (compare value expected 'char-greaterp) 
        'assert-char-greaterp value expected :test-name test-name))

(defun assert-char-greaterorequalp-v (value expected &optional test-name)
    (funcall (compare-any-v value expected (list 'char-equal 'char-greaterp)) 
        'assert-char-greaterorequalp-v value expected :test-name test-name))

(defun assert-char-greaterorequalp (value expected &optional test-name)
    (funcall (compare-any value expected (list 'char-equal 'char-greaterp)) 
        'assert-char-greaterorequalp value expected :test-name test-name))

(defun assert-char-lessp-v (value expected &optional test-name)
    (funcall (compare-v value expected 'char-lessp) 
        'assert-char-lessp-v value expected :test-name test-name))

(defun assert-char-lessp (value expected &optional test-name)
    (funcall (compare value expected 'char-lessp) 
        'assert-char-lessp value expected :test-name test-name))

(defun assert-char-lessorequalp-v (value expected &optional test-name)
    (funcall (compare-any-v value expected (list 'char-equal 'char-lessp))
        'assert-char-lessorequalp-v value expected :test-name test-name))

(defun assert-char-lessorequalp (value expected &optional test-name)
    (funcall (compare-any value expected (list 'char-equal 'char-lessp)) 
        'assert-char-lessorequalp value expected :test-name test-name))
