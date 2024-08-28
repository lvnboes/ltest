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
        :assertion :test))

(in-package :ltest)

#|
Checks
 |#

;;Simple checks

(defun check-true (pred val exp) 
    (funcall pred val exp))

(defun check-false (pred val exp)
    (not (funcall pred val exp)))

;;Iterative checks

(defun check-all-v (pred vals exp) 
    (liter:all-val pred vals exp))

(defun check-no-v (pred vals exp) 
    (liter:no-val pred vals exp))

(defun check-some-v (pred vals exp) 
    (liter:some-val pred vals exp))

(defun check-some-not-v (pred vals exp) 
    (liter:some-not-val pred vals exp))

(defun check-all-p (pred vals exp) 
    (liter:all-pred pred vals exp))

(defun check-no-p (pred vals exp) 
    (liter:no-pred pred vals exp))

(defun check-some-p (pred vals exp) 
    (liter:some-pred pred vals exp))

(defun check-some-not-p (pred vals exp) 
    (liter:some-not-pred pred vals exp))

;;Double iterative checks

(defun check-all-v-all-p (preds vals exp)
    (funcall (liter:iter-2 #'liter:all-val #'liter:all-pred)
        preds vals exp))

(defun check-all-v-no-p (preds vals exp)
    (funcall (liter:iter-2 #'liter:all-val #'liter:no-pred)
        preds vals exp))

(defun check-all-v-some-p (preds vals exp)
    (funcall (liter:iter-2 #'liter:all-val #'liter:some-pred)
        preds vals exp))

(defun check-all-v-some-not-p (preds vals exp)
    (funcall (liter:iter-2 #'liter:all-val #'liter:some-not-pred)
        preds vals exp))

(defun check-no-v-all-p (preds vals exp)
    (funcall (liter:iter-2 #'liter:no-val #'liter:all-pred)
        preds vals exp))

(defun check-no-v-no-p (preds vals exp)
    (funcall (liter:iter-2 #'liter:no-val #'liter:no-pred)
        preds vals exp))

(defun check-no-v-some-p (preds vals exp)
    (funcall (liter:iter-2 #'liter:no-val #'liter:some-pred)
        preds vals exp))

(defun check-no-v-some-not-p (preds vals exp)
    (funcall (liter:iter-2 #'liter:no-val #'liter:some-not-pred)
        preds vals exp))

(defun check-some-v-all-p (preds vals exp)
    (funcall (liter:iter-2 #'liter:some-val #'liter:all-pred)
        preds vals exp))

(defun check-some-v-no-p (preds vals exp)
    (funcall (liter:iter-2 #'liter:some-val #'liter:no-pred)
        preds vals exp))

(defun check-some-v-some-p (preds vals exp)
    (funcall (liter:iter-2 #'liter:some-val #'liter:some-pred)
        preds vals exp))

(defun check-some-v-some-not-p (preds vals exp)
    (funcall (liter:iter-2 #'liter:some-val #'liter:some-not-pred)
        preds vals exp))

(defun check-some-not-v-all-p (preds vals exp)
    (funcall (liter:iter-2 #'liter:some-not-val #'liter:all-pred)
        preds vals exp))

(defun check-some-not-v-no-p (preds vals exp)
    (funcall (liter:iter-2 #'liter:some-not-val #'liter:no-pred)
        preds vals exp))

(defun check-some-not-v-some-p (preds vals exp)
    (funcall (liter:iter-2 #'liter:some-not-val #'liter:some-pred)
        preds vals exp))

(defun check-some-not-v-some-not-p (preds vals exp)
    (funcall (liter:iter-2 #'liter:some-not-val #'liter:some-not-pred)
        preds vals exp))

 #|
 Assertion
  |#

(defun assertion (&key (check #'check-true) (pred #'equalp) (val nil) (exp t))
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

(defun to-result-table (assertion-results) 
    (let ((result-table (make-hash-table)))
        (setf (gethash :pass result-table) 0)
        (setf (gethash :fail result-table) 0)
        (setf (gethash :invalid result-table) 0)
        (dolist (assertion-result assertion-results)
            (incf (gethash (getf assertion-result :result) result-table 0)))
        (setf (gethash :result result-table) 
            (cond ((> (gethash :fail result-table) 0) :fail)
                ((> (gethash :invalid result-table) 0) :invalid)
                (t :pass)))
        (setf (gethash :assertions result-table) 
            assertion-results)
        result-table))

(defun test (&key name assertions)
    (let* ((result-table (to-result-table assertions)))
        (setf (gethash :name result-table) name)
        (out:test-result result-table)))