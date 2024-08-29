(defpackage :ltest
    (:use :cl :liter :out)
    (:export 
        :check-true :check-false
        :check-all-v :check-no-v :check-some-v :check-some-not-v
        :check-all-p :check-no-p :check-some-p :check-some-not-p
        :check-all-v-all-p :check-all-v-no-p :check-all-v-some-p 
        :check-all-v-some-not-p :check-no-v-all-p :check-no-v-no-p 
        :check-no-v-some-p :check-no-v-some-not-p :check-some-v-all-p 
        :check-some-v-no-p :check-some-v-some-p :check-some-v-some-not-p
        :check-some-not-v-all-p :check-some-not-v-no-p :check-some-not-v-some-p 
        :check-some-not-v-some-not-p
        :assertion :test :test-set :test-suite))

(in-package :ltest)

#|
Checks
 |#

;;Simple checks

(defun check-true (pred val exp) 
    "Apply predicate to value"
    (funcall pred val exp))

(defun check-false (pred val exp)
    "Apply predicate to value 
        and return opposite"
    (not (funcall pred val exp)))

;;Iterative checks

(defun check-all-v (pred vals exp) 
    "Apply predicate to values and 
        check if all evaluate to t"
    (liter:all-val pred vals exp))

(defun check-no-v (pred vals exp) 
    "Apply predicate to values and 
        check if none evaluate to t"
    (liter:no-val pred vals exp))

(defun check-some-v (pred vals exp) 
    "Apply predicate to values and 
        check if some evaluate to t"
    (liter:some-val pred vals exp))

(defun check-some-not-v (pred vals exp) 
    "Apply predicate to values and 
        check if some don't evaluate to t"
    (liter:some-not-val pred vals exp))

(defun check-all-p (pred vals exp) 
    "Apply all predicates to value and 
        check if all evaluate to t"
    (liter:all-pred pred vals exp))

(defun check-no-p (pred vals exp) 
    "Apply all predicates to value and 
        check if none evaluate to t"
    (liter:no-pred pred vals exp))

(defun check-some-p (pred vals exp) 
    "Apply all predicates to value and 
        check if some evaluate to t"
    (liter:some-pred pred vals exp))

(defun check-some-not-p (pred vals exp) 
    "Apply all predicates to value and 
        check if some don't evaluate to t"
    (liter:some-not-pred pred vals exp))

;;Double iterative checks

(defun check-all-v-all-p (preds vals exp)
    "Apply all predicates to all values and check if all 
        predicates evaluate to t for all values"
    (funcall (liter:iter-2 #'liter:all-val #'liter:all-pred)
        preds vals exp))

(defun check-all-v-no-p (preds vals exp)
    "Apply all predicates to all values and check if none of the 
        predicates evaluate to t for all values"
    (funcall (liter:iter-2 #'liter:all-val #'liter:no-pred)
        preds vals exp))

(defun check-all-v-some-p (preds vals exp)
    "Apply all predicates to all values and check if some of the 
        predicates evaluate to t for all values"
    (funcall (liter:iter-2 #'liter:all-val #'liter:some-pred)
        preds vals exp))

(defun check-all-v-some-not-p (preds vals exp)
    "Apply all predicates to all values and check if some of the 
        predicates evaluate to nil for all values"
    (funcall (liter:iter-2 #'liter:all-val #'liter:some-not-pred)
        preds vals exp))

(defun check-no-v-all-p (preds vals exp)
    "Apply all predicates to all values and check if all 
    predicates evaluate to t for none of the values"
    (funcall (liter:iter-2 #'liter:no-val #'liter:all-pred)
        preds vals exp))

(defun check-no-v-no-p (preds vals exp)
    "Apply all predicates to all values and check if none of the 
        predicates evaluate to t for none of the values"
    (funcall (liter:iter-2 #'liter:no-val #'liter:no-pred)
        preds vals exp))

(defun check-no-v-some-p (preds vals exp)
    "Apply all predicates to all values and check if some of the 
        predicates evaluate to t for none of the values"
    (funcall (liter:iter-2 #'liter:no-val #'liter:some-pred)
        preds vals exp))

(defun check-no-v-some-not-p (preds vals exp)
    "Apply all predicates to all values and check if some of the 
        predicates evaluate to nil for none of the values"
    (funcall (liter:iter-2 #'liter:no-val #'liter:some-not-pred)
        preds vals exp))

(defun check-some-v-all-p (preds vals exp)
    "Apply all predicates to all values and check if all 
        predicates evaluate to t for some of the values"
    (funcall (liter:iter-2 #'liter:some-val #'liter:all-pred)
        preds vals exp))

(defun check-some-v-no-p (preds vals exp)
    "Apply all predicates to all values and check if none of the 
        predicates evaluate to t for some of the values"
    (funcall (liter:iter-2 #'liter:some-val #'liter:no-pred)
        preds vals exp))

(defun check-some-v-some-p (preds vals exp)
    "Apply all predicates to all values and check if some of the 
        predicates evaluate to t for some of the values"
    (funcall (liter:iter-2 #'liter:some-val #'liter:some-pred)
        preds vals exp))

(defun check-some-v-some-not-p (preds vals exp)
    "Apply all predicates to all values and check if some of the 
        predicates evaluate to nil for some of the values"
    (funcall (liter:iter-2 #'liter:some-val #'liter:some-not-pred)
        preds vals exp))

(defun check-some-not-v-all-p (preds vals exp)
    "Apply all predicates to all values and check if all 
        predicates evaluate to nil for some of the values"
    (funcall (liter:iter-2 #'liter:some-not-val #'liter:all-pred)
        preds vals exp))

(defun check-some-not-v-no-p (preds vals exp)
    "Apply all predicates to all values and check if none of the 
        predicates evaluate to nil for some of the values"
    (funcall (liter:iter-2 #'liter:some-not-val #'liter:no-pred)
        preds vals exp))

(defun check-some-not-v-some-p (preds vals exp)
    "Apply all predicates to all values and check if some of the 
        predicates evaluate to nil for some of the values"
    (funcall (liter:iter-2 #'liter:some-not-val #'liter:some-pred)
        preds vals exp))

(defun check-some-not-v-some-not-p (preds vals exp)
    "Apply all predicates to all values and check if some of the 
        predicates evaluate to t for some of the values"
    (funcall (liter:iter-2 #'liter:some-not-val #'liter:some-not-pred)
        preds vals exp))

 #|
 Assertion
  |#

(defun assertion (&key (check #'check-true) (pred #'equalp) (val nil) (exp t))
    "Execute the requested check and return a list the test result, assertion 
        arguments and, if applicable, errors."
    (handler-case
        (if (funcall check pred val exp)
            (list :result :pass 
                :check check :pred pred :val val :exp exp :error nil)
            (list :result :fail 
                :check check :pred pred :val val :exp exp :error nil))
        (error (e)
            (list :result :invalid 
                :check check :pred pred :val val :exp exp :error e))))

#|
Test
 |#

(defun to-test-result-table (assertions name) 
    "Process the test name and assertion results into a result table 
        and return the table"
    (let ((result-table (make-hash-table)))
        (setf (gethash :name result-table) name)
        (setf (gethash :pass result-table) 0)
        (setf (gethash :fail result-table) 0)
        (setf (gethash :invalid result-table) 0)
        (dolist (assertion assertions)
            (incf (gethash (getf assertion :result) result-table 0)))
        (setf (gethash :result result-table) 
            (cond ((> (gethash :fail result-table) 0) :fail)
                ((> (gethash :invalid result-table) 0) :invalid)
                (t :pass)))
        (setf (gethash :assertions result-table) 
            assertions)
        result-table))

(defun test (&key name assertions (output-stream nil))
    "Process the assertion results into a result table, call on the output 
        function to display results and return the result table"
    (let ((result-table (to-test-result-table assertions name)))
        (out:test-out result-table output-stream)
        result-table))

#|
Test Set
 |#

(defun to-test-set-result-table (tests name)
    "Process the test set name and test results into a result table 
        and return the table"
    (let ((result-table (make-hash-table))
            (total (list-length tests)))
        (setf (gethash :name result-table) name)
        (setf (gethash :pass result-table) 0)
        (setf (gethash :fail result-table) 0)
        (setf (gethash :invalid result-table) 0)
        (dolist (test tests)
            (incf (gethash (gethash :result test) result-table 0)))
        (setf (gethash :result result-table) 
            (if (= total (gethash :pass result-table)) :pass :fail))
        result-table))

(defun test-set (&key name tests (output-stream nil))
    "Process the test results into a result table, call on the output 
        function to display results and return the result table"
    (let ((result-table (to-test-set-result-table tests name)))
        (out:test-set-out result-table output-stream)
        result-table))

#|
Test Suite
 |#

(defun to-test-suite-result-table (test-sets name)
    "Process the test suite name and test set results into a result table 
        and return the table"
    (let ((result-table (make-hash-table))
            (total (list-length test-sets)))
        (setf (gethash :name result-table) name)
        (setf (gethash :pass result-table) 0)
        (setf (gethash :fail result-table) 0)
        (dolist (test-set test-sets)
            (incf (gethash (gethash :result test-set) result-table 0)))
        (setf (gethash :result result-table) 
            (if (= total (gethash :pass result-table)) :pass :fail))
        result-table))

(defun test-suite (&key name test-sets (output-stream 0))
    "Process the test-set results into a result table, call on the output 
        function to display results and return the result table"
    (let ((result-table (to-test-suite-result-table test-sets name))
            (use-output (if (equal output-stream 0) t output-stream)))
        (out:test-suite-out result-table use-output)))