(defpackage :lassert
    (:use :cl :lcheck)
    (:export :assertion))

(in-package :lassert)

(defun determine-default-check ())

(defun determine-default-pred ())

(defun assertion (&key (check #'lcheck:check-true) (pred #'equalp) val (exp t))
    (handler-case
        (if (funcall check pred val exp)
            (list 
                'pass 
                (list :check check :pred pred :val val :exp exp :error nil))
            (list 
                'fail 
                (list :check check :pred pred :val val :exp exp :error nil)))
        (error (e) 
            (list 
                'invalid 
                (list :check check :pred pred :val val :exp exp :error e)))))

(defun check-assertions (assertions result) 
    (if (null assertions)
        (reverse result)
        (check-assertions 
            (cdr assertions) 
            (cons (funcall (car assertions)) result))))

(defun test (&key assertions (out t))
    (let* ((assertion-results (check-assertions assertions)))
        (format out "TODO ~a" assertion-results)))