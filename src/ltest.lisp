(defpackage :ltest
    (:use :cl :lcheck :out)
    (:export :assertion :test))

(in-package :ltest)

(defun determine-default-check ())

(defun determine-default-pred ())

(defun assertion (&key (check #'lcheck:check-true) (pred #'equalp) (val nil) (exp t))
    (handler-case
        (if (funcall check pred val exp)
            (list :result :pass 
                :check check :pred pred :val val :exp exp :error nil)
            (list :result :fail 
                :check check :pred pred :val val :exp exp :error nil))
        (error (e)
            (list :result :invalid 
                :check check :pred pred :val val :exp exp :error e))))

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