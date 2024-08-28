(defpackage :lassert
    (:use :cl :lcheck)
    (:export :assertion :test))

(in-package :lassert)

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
        (dolist (assertion-result assertion-results)
            (incf (gethash (getf assertion-result :result) result-table 0)))
        result-table))

(defun test (&key assertions)
    (let* ((result-table (to-result-table assertions))
            (passed-assertions (gethash :pass result-table))
            (failed-assertions (gethash :fail result-table))
            (invalid-assertions (gethash :invalid result-table))
            (result (cond 
                ((> failed-assertions 0) :fail)
                ((> invalid-assertions 0) :invalid)
                ((> passed-assertions 0) :pass))))
    (format nil "~a~%~a~%~a~%~a~%~%~a" result passed-assertions failed-assertions invalid-assertions assertions)))