(defpackage :iter
    (:use :cl)
    (:export :all-val :no-val :some-val :some-not-val
        :all-pred :no-pred :some-pred :some-not-pred
        :iter-2))

(in-package :iter)

#|
Herlper functions for iterating over predicates provided values
 |#

(defun all-val (pred-or-preds vals x &optional (fn #'funcall)) 
    "Check if the provided predicate(s) evaluate(s) to t for 
        all provided values in relation to the expected value"
    (cond ((null vals) t)
        ((funcall fn pred-or-preds (car vals) x)
            (all-val pred-or-preds (cdr vals) x fn))
        (t nil)))

(defun no-val (pred-or-preds vals x &optional (fn #'funcall)) 
    "Check if the provided predicate(s) evaluate(s) to t for 
        none of the provided values in relation to the expected value"
    (cond ((null vals) t)
        ((funcall fn pred-or-preds (car vals) x) nil)
        (t (no-val pred-or-preds (cdr vals) x fn))))

(defun some-val (pred-or-preds vals x &optional (fn #'funcall)) 
    "Check if the provided predicate(s) evaluate(s) to t for 
        some of the provided values in relation to the expected value"
    (cond ((null vals) nil)
        ((funcall fn pred-or-preds (car vals) x) t)
        (t (some-val pred-or-preds (cdr vals) x fn))))

(defun some-not-val (pred-or-preds vals x &optional (fn #'funcall)) 
    "Check if the provided predicate(s) evaluate(s) to nil for 
        some of the provided values in relation to the expected value"
    (cond ((null vals) nil)
        ((funcall fn pred-or-preds (car vals) x)
            (some-not-val pred-or-preds (cdr vals) x fn))
        (t t)))

(defun all-pred (preds val x) 
    "Check if applying the provided predicates to the provided and 
        expected values evaluates to t for all predicates"
    (cond ((null preds) t)
        ((funcall (car preds) val x)
            (all-pred (cdr preds) val x))
        (t nil)))

(defun no-pred (preds val x) 
    "Check if applying the provided predicates to the provided and 
        expected values evaluates to t for none of the predicates"
    (cond ((null preds) t)
        ((funcall (car preds) val x) nil)
        (t (no-pred (cdr preds) val x))))

(defun some-pred (preds val x) 
    "Check if applying the provided predicates to the provided and 
        expected values evaluates to t for some of the predicates"
    (cond ((null preds) nil)
        ((funcall (car preds) val x) t)
        (t (some-pred (cdr preds) val x))))

(defun some-not-pred (preds val x) 
    "Check if applying the provided predicates to the provided and 
        expected values evaluates to nil for some of the predicates"
    (cond ((null preds) nil)
        ((funcall (car preds) val x) 
            (some-not-pred (cdr preds) val x))
        (t t)))

(defun iter-2 (val-fn pred-fn)
    "Combine one of the functions for iterating over the provided values
        and one of the functions for iterating over the predicates
        and return a combined (anonymous) function."
    #'(lambda (preds vals x)
        (funcall val-fn preds vals x pred-fn)))