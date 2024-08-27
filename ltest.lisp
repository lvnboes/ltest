(defpackage :ltest
    (:use :cl)
    (:export :test-set
        :compare-v :compare :compare-not-v :compare-not :compare-any-v 
        :compare-any :compare-all-v :compare-all :compare-none-v :compare-none
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
        :assert-char-lessp :assert-char-lessorequalp-v :assert-char-lessorequalp
        
        :green :yellow :red))

(in-package :ltest)

#|
Helper functions to print out test results
 |#

(defun green (str) (format nil "~c[32m~a~c[0m" #\esc str #\esc))

(defun yellow (str) (format nil "~c[33m~a~c[0m" #\esc str #\esc))

(defun red (str) (format nil "~c[31m~a~c[0m" #\esc str #\esc))

(defun pass (f-name var1 var2 &optional (predicates nil pred-p) (test-name nil test-name-p)) 
    (let ((pred-str (if pred-p (format nil " ~{~a~^ ~} |" predicates) ""))
            (test-name-str (if test-name-p (format nil "~a | " test-name) "")))
        (format T
            (green "~%~a~a |~a ~a ~a | ~a ~a => PASS...")
            test-name-str f-name pred-str
            (type-of var1) var1 (type-of var2) var2)
        :pass))

(defun fail (f-name var1 var2 &optional (predicates nil pred-p) (test-name nil test-name-p)) 
    (let ((pred-str (if pred-p (format nil " ~{~a~^ ~} |" predicates) ""))
            (test-name-str (if test-name-p (format nil "~a | " test-name) "")))
        (format T
            (red "~%~a~a |~a ~a ~a | ~a ~a => FAIL!!!")
            test-name-str f-name pred-str
            (type-of var1) var1 (type-of var2) var2)
        :fail))
    

(defun invalid (f-name var1 var2 &optional (predicates nil pred-p) (test-name nil test-name-p)) 
    (let ((pred-str (if pred-p (format nil " ~{~a~^ ~} |" predicates) ""))
            (test-name-str (if test-name-p (format nil "~a | " test-name) "")))
        (format T
            (yellow "~%~a~a |~a ~a ~a | ~a ~a => INVALID???")
            test-name-str f-name pred-str
            (type-of var1) var1 (type-of var2) var2)
        :invalid))

#|
Helper functions to execute tests as a set and print out the set result
 |#

(defun show-test-set-result (name result-hash-table)
    (let* ((passed (gethash :pass result-hash-table 0))
            (failed (gethash :fail result-hash-table 0))
            (invalid (gethash :invalid result-hash-table 0))
            (total (+ failed passed invalid)))
        (format T 
            "~%Results of test-set ~a~%~\t~\tPASSED: ~a~\t~\tFAILED~a~\t~\tINVALID~a~%~\t~\tTOTAL~a~%~%"
            name passed failed invalid total)
        result-hash-table))

(defun test-set-execute (tests result-acc)
    (if (eq (list-length tests) 0)
        result-acc
        (progn 
            (incf (gethash (car tests) result-acc 0))
            (test-set-execute (cdr tests) result-acc))))

(defun test-set (name &rest tests)
    (let ((results (test-set-execute tests (make-hash-table))))
        (show-test-set-result name results)))

#|
Basic comparison functions with and without validity check
    With -v: results in INVALID if value doesn't match predicate
    Without -v: results in FAIL if value doesn't match predicate
 |#

(defun compare-v (val1 val2 pred)
    (handler-case
        (if (funcall pred val1 val2) 'pass 'fail)
        (error () 'invalid)))

(defun compare (val1 val2 pred)
    (handler-case
        (if (funcall pred val1 val2) 'pass 'fail)
        (error () 'fail)))

(defun compare-not-v (val1 val2 pred)
    (handler-case
        (if (funcall pred val1 val2) 'fail 'pass)
        (error () 'invalid)))

(defun compare-not (val1 val2 pred)
    (handler-case
        (if (funcall pred val1 val2) 'fail 'pass)
        (error () 'fail)))

(defun compare-any-v (val1 val2 p-list)
    (handler-case
        (cond ((= (list-length p-list) 0) 'fail)
            ((funcall (car p-list) val1 val2) 'pass)
            (t (compare-any-v val1 val2 (cdr p-list))))
        (error () 'invalid)))

(defun compare-any (val1 val2 p-list)
    (handler-case
        (cond ((= (list-length p-list) 0) 'fail)
            ((funcall (car p-list) val1 val2) 'pass)
            (t (compare-any val1 val2 (cdr p-list))))
        (error () (compare-any val1 val2 (cdr p-list)))))

(defun compare-all-v (val1 val2 p-list)
    (handler-case
        (cond ((= (list-length p-list) 0) 'pass)
            ((funcall (car p-list) val1 val2) 
                (compare-all-v val1 val2 (cdr p-list)))
            (t 'fail))
        (error () 'invalid)))

(defun compare-all (val1 val2 p-list)
    (handler-case
        (cond ((= (list-length p-list) 0) 'pass)
            ((funcall (car p-list) val1 val2) 
                (compare-all val1 val2 (cdr p-list)))
            (t 'fail))
        (error () 'fail)))

(defun compare-none-v (val1 val2 p-list)
    (handler-case
        (cond ((= (list-length p-list) 0) 'pass)
            ((funcall (car p-list) val1 val2) 'fail)
            (t (compare-none-v val1 val2 (cdr p-list))))
        (error () 'invalid)))

(defun compare-none (val1 val2 p-list)
    (handler-case
        (cond ((= (list-length p-list) 0) 'pass)
            ((funcall (car p-list) val1 val2) 'fail)
            (t (compare-none val1 val2 (cdr p-list))))
        (error () (compare-none val1 val2 (cdr p-list)))))

#|
Test functions derived from the basic comparison functions
 |#

;; check predicates on two value

(defun assert-is-v (value expected predicate &optional test-name)
    (funcall (compare-v value expected predicate) 
        'assert-is-v value expected (list predicate) test-name))

(defun assert-is (value expected predicate &optional test-name)
    (funcall (compare value expected predicate) 
        'assert-is value expected (list predicate) test-name))

(defun assert-not-v (value expected predicate &optional test-name)
    (funcall (compare-not-v value expected predicate) 
        'assert-not-v value expected (list predicate) test-name))

(defun assert-not (value expected predicate &optional test-name)
    (funcall (compare-not value expected predicate) 
        'assert-not value expected (list predicate) test-name))

(defun assert-any-v (value expected predicates &optional test-name)
    (funcall (compare-any-v value expected predicates) 
        'assert-any-v value expected predicates test-name))

(defun assert-any (value expected predicates &optional test-name)
    (funcall (compare-any value expected predicates) 
        'assert-any value expected predicates test-name))

(defun assert-all-v (value expected predicates &optional test-name)
    (funcall (compare-all-v value expected predicates) 
        'assert-all-v value expected predicates test-name))

(defun assert-all (value expected predicates &optional test-name)
    (funcall (compare-all value expected predicates) 
        'assert-all value expected predicates test-name))

(defun assert-none-v (value expected predicates &optional test-name)
    (funcall (compare-none-v value expected predicates) 
        'assert-none-v value expected predicates test-name))

(defun assert-none (value expected predicates &optional test-name)
    (funcall (compare-none value expected predicates) 
        'assert-none value expected predicates test-name))

;; comparison of two values

(defun assert-=-v (value expected &optional test-name)
    (funcall (compare-v value expected '=) 
        'assert-=-v value expected test-name))

(defun assert-= (value expected &optional test-name)
    (funcall (compare value expected '=) 
        'assert-= value expected test-name))

(defun assert-/=-v (value expected &optional test-name)
    (funcall (compare-v value expected '/=) 
        'assert-/=-v value expected test-name))

(defun assert-/= (value expected &optional test-name)
    (funcall (compare value expected '/=) 
        'assert-/= value expected test-name))

(defun assert->-v (value expected &optional test-name)
    (funcall (compare-v value expected '>) 
        'assert->-v value expected test-name))

(defun assert-> (value expected &optional test-name)
    (funcall (compare value expected '>) 
        'assert-> value expected test-name))

(defun assert->=-v (value expected &optional test-name)
    (funcall (compare-v value expected '>=) 
        'assert->=-v value expected test-name))

(defun assert->= (value expected &optional test-name)
    (funcall (compare value expected '>=) 
        'assert->= value expected test-name))

(defun assert-<-v (value expected &optional test-name)
    (funcall (compare-v value expected '<) 
        'assert-<-v value expected test-name))

(defun assert-< (value expected &optional test-name)
    (funcall (compare value expected '<) 
        'assert-< value expected test-name))

(defun assert-<=-v (value expected &optional test-name)
    (funcall (compare-v value expected '<=) 
        'assert-<=-v value expected test-name))

(defun assert-<= (value expected &optional test-name)
    (funcall (compare value expected '<=) 
        'assert-<= value expected test-name))

(defun assert-eq-v (value expected &optional test-name)
    (funcall (compare-v value expected 'eq) 
        'assert-eq-v value expected test-name))

(defun assert-eq (value expected &optional test-name)
    (funcall (compare value expected 'eq) 
        'assert-eq value expected test-name))

(defun assert-not-eq-v (value expected &optional test-name)
    (funcall (compare-not-v value expected 'eq) 
        'assert-not-eq-v value expected test-name))

(defun assert-not-eq (value expected &optional test-name)
    (funcall (compare-not value expected 'eq) 
        'assert-not-eq value expected test-name))

(defun assert-eql-v (value expected &optional test-name)
    (funcall (compare-v value expected 'eql) 
        'assert-eql-v value expected test-name))

(defun assert-eql (value expected &optional test-name)
    (funcall (compare value expected 'eql) 
        'assert-eql value expected test-name))

(defun assert-not-eql-v (value expected &optional test-name)
    (funcall (compare-not-v value expected 'eql) 
        'assert-not-eql-v value expected test-name))

(defun assert-not-eql (value expected &optional test-name)
    (funcall (compare-not value expected 'eql) 
        'assert-not-eql value expected test-name))

(defun assert-equal-v (value expected &optional test-name)
    (funcall (compare-v value expected 'equal) 
        'assert-equal-v value expected test-name))

(defun assert-equal (value expected &optional test-name)
    (funcall (compare value expected 'equal) 
        'assert-equal value expected test-name))

(defun assert-not-equal-v (value expected &optional test-name)
    (funcall (compare-not-v value expected 'equal) 
        'assert-not-equal-v value expected test-name))

(defun assert-not-equal (value expected &optional test-name)
    (funcall (compare-not value expected 'equal) 
        'assert-not-equal value expected test-name))

(defun assert-equalp-v (value expected &optional test-name)
    (funcall (compare-v value expected 'equalp) 
        'assert-equalp-v value expected test-name))

(defun assert-equalp (value expected &optional test-name)
    (funcall (compare value expected 'equalp) 
        'assert-equalp value expected test-name))

(defun assert-not-equalp-v (value expected &optional test-name)
    (funcall (compare-not-v value expected 'equalp) 
        'assert-not-equalp-v value expected test-name))

(defun assert-not-equalp (value expected &optional test-name)
    (funcall (compare-not value expected 'equalp) 
        'assert-not-equalp value expected test-name))

;; comparison of two strings

(defun assert-string=-v (value expected &optional test-name)
    (funcall (compare-v value expected 'string=) 
        'assert-string=-v value expected test-name))

(defun assert-string= (value expected &optional test-name)
    (funcall (compare value expected 'string=) 
        'assert-string= value expected test-name))

(defun assert-string/=-v (value expected &optional test-name)
    (funcall (compare-v value expected 'string/=) 
        'assert-string/=-v value expected test-name))

(defun assert-string/= (value expected &optional test-name)
    (funcall (compare value expected 'string/=) 
        'assert-string/= value expected test-name))

(defun assert-string>-v (value expected &optional test-name)
    (funcall (compare-v value expected 'string>) 
        'assert-string>-v value expected test-name))

(defun assert-string> (value expected &optional test-name)
    (funcall (compare value expected 'string>) 
        'assert-string> value expected test-name))

(defun assert-string>=-v (value expected &optional test-name)
    (funcall (compare-v value expected 'string>=) 
        'assert-string>=-v value expected test-name))

(defun assert-string>= (value expected &optional test-name)
    (funcall (compare value expected 'string>=) 
        'assert-string>= value expected test-name))

(defun assert-string<-v (value expected &optional test-name)
    (funcall (compare-v value expected 'string<) 
        'assert-string<-v value expected test-name))

(defun assert-string< (value expected &optional test-name)
    (funcall (compare value expected 'string<) 
        'assert-string< value expected test-name))

(defun assert-string<=-v (value expected &optional test-name)
    (funcall (compare-v value expected 'string<=) 
        'assert-string<=-v value expected test-name))

(defun assert-string<= (value expected &optional test-name)
    (funcall (compare value expected 'string<=) 
        'assert-string<= value expected test-name))

(defun assert-string-equal-v (value expected &optional test-name)
    (funcall (compare-v value expected 'string-equal) 
        'assert-string-equal-v value expected test-name))

(defun assert-string-equal (value expected &optional test-name)
    (funcall (compare value expected 'string-equal) 
        'assert-string-equal value expected test-name))

(defun assert-string-not-equal-v (value expected &optional test-name)
    (funcall (compare-v value expected 'string-not-equal) 
        'assert-string-not-equal-v value expected test-name))

(defun assert-string-not-equal (value expected &optional test-name)
    (funcall (compare value expected 'string-not-equal) 
        'assert-string-not-equal value expected test-name))

(defun assert-string-greaterp-v (value expected &optional test-name)
    (funcall (compare-v value expected 'string-greaterp) 
        'assert-string-greaterp-v value expected test-name))

(defun assert-string-greaterp (value expected &optional test-name)
    (funcall (compare value expected 'string-greaterp) 
        'assert-string-greaterp value expected test-name))

(defun assert-string-greaterorequalp-v (value expected &optional test-name)
    (funcall (compare-any-v value expected (list 'string-equal 'string-greaterp)) 
        'assert-string-greaterorequalp-v value expected test-name))

(defun assert-string-greaterorequalp (value expected &optional test-name)
    (funcall (compare-any value expected (list 'string-equal 'string-greaterp))
        'assert-string-greaterorequalp value expected test-name))

(defun assert-string-lessp-v (value expected &optional test-name)
    (funcall (compare-v value expected 'string-lessp) 
        'assert-string-lessp-v value expected test-name))

(defun assert-string-lessp (value expected &optional test-name)
    (funcall (compare value expected 'string-lessp) 
        'assert-string-lessp value expected test-name))

(defun assert-string-lessorequalp-v (value expected &optional test-name)
    (funcall (compare-any-v value expected (list 'string-equal 'string-lessp)) 
        'assert-string-lessorequalp-v value expected test-name))

(defun assert-string-lessorequalp (value expected &optional test-name)
    (funcall (compare-any value expected (list 'string-equal 'string-lessp)) 
        'assert-string-lessorequalp value expected test-name))

;;comparison of two chars

(defun assert-char=-v (value expected &optional test-name)
    (funcall (compare-v value expected 'char=) 
        'assert-char=-v value expected test-name))

(defun assert-char= (value expected &optional test-name)
    (funcall (compare value expected 'char=) 
        'assert-char= value expected test-name))

(defun assert-char/=-v (value expected &optional test-name)
    (funcall (compare-v value expected 'char/=) 
        'assert-char/=-v value expected test-name))
(defun assert-char/= (value expected &optional test-name)
    (funcall (compare value expected 'char/=) 
        'assert-char/= value expected test-name))

(defun assert-char>-v (value expected &optional test-name)
    (funcall (compare-v value expected 'char>) 
        'assert-char>-v value expected test-name))

(defun assert-char> (value expected &optional test-name)
    (funcall (compare value expected 'char>) 
        'assert-char> value expected test-name))

(defun assert-char>=-v (value expected &optional test-name)
    (funcall (compare-v value expected 'char>=) 
        'assert-char>=-v value expected test-name))

(defun assert-char>= (value expected &optional test-name)
    (funcall (compare value expected 'char>=) 
        'assert-char>= value expected test-name))

(defun assert-char<-v (value expected &optional test-name)
    (funcall (compare-v value expected 'char<) 
        'assert-char<-v value expected test-name))

(defun assert-char< (value expected &optional test-name)
    (funcall (compare value expected 'char<) 
        'assert-char< value expected test-name))

(defun assert-char<=-v (value expected &optional test-name)
    (funcall (compare-v value expected 'char<=) 
        'assert-char<=-v value expected test-name))

(defun assert-char<= (value expected &optional test-name)
    (funcall (compare value expected 'char<=) 
        'assert-char<= value expected test-name))

(defun assert-char-equal-v (value expected &optional test-name)
    (funcall (compare-v value expected 'char-equal) 
        'assert-char-equal-v value expected test-name))

(defun assert-char-equal (value expected &optional test-name)
    (funcall (compare value expected 'char-equal) 
        'assert-char-equal value expected test-name))

(defun assert-char-not-equal-v (value expected &optional test-name)
    (funcall (compare-v value expected 'char-not-equal) 
        'assert-char-not-equal-v value expected test-name))

(defun assert-char-not-equal (value expected &optional test-name)
    (funcall (compare value expected 'char-not-equal) 
        'assert-char-not-equal value expected test-name))

(defun assert-char-greaterp-v (value expected &optional test-name)
    (funcall (compare-v value expected 'char-greaterp) 
        'assert-char-greaterp-v value expected test-name))

(defun assert-char-greaterp (value expected &optional test-name)
    (funcall (compare value expected 'char-greaterp) 
        'assert-char-greaterp value expected test-name))

(defun assert-char-greaterorequalp-v (value expected &optional test-name)
    (funcall (compare-any-v value expected (list 'char-equal 'char-greaterp)) 
        'assert-char-greaterorequalp-v value expected test-name))

(defun assert-char-greaterorequalp (value expected &optional test-name)
    (funcall (compare-any value expected (list 'char-equal 'char-greaterp)) 
        'assert-char-greaterorequalp value expected test-name))

(defun assert-char-lessp-v (value expected &optional test-name)
    (funcall (compare-v value expected 'char-lessp) 
        'assert-char-lessp-v value expected test-name))

(defun assert-char-lessp (value expected &optional test-name)
    (funcall (compare value expected 'char-lessp) 
        'assert-char-lessp value expected test-name))

(defun assert-char-lessorequalp-v (value expected &optional test-name)
    (funcall (compare-any-v value expected (list 'char-equal 'char-lessp))
        'assert-char-lessorequalp-v value expected test-name))

(defun assert-char-lessorequalp (value expected &optional test-name)
    (funcall (compare-any value expected (list 'char-equal 'char-lessp)) 
        'assert-char-lessorequalp value expected test-name))