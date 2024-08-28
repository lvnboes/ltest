(defpackage :lassert
    (:use :cl :lcheck)
    (:export :assertion))

(in-package :lassert)

(defun determine-default-check ())

(defun determine-default-pred ())

(defun assertion (&key check pred val (exp t))
    (handler-case
        (if (funcall check pred val exp)
            (list 'pass)
            (list 
                'fail 
                (format nil "failed: ~a | ~a | ~a | ~a")
                    check pred val exp))
        (error (e) (list 
            'invalid 
            (format nil "invalid: ~a | ~a | ~a | ~a | ~a"
                check pred val exp e)))))