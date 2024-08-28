(defpackage :liter
    (:use :cl)
    (:export :all-val :no-val :some-val :some-not-val
        :all-pred :no-pred :some-pred :some-not-pred
        :iter-2))

(in-package :liter)

(defun all-val (pred-or-preds vals x &optional (fn #'funcall)) 
    (cond ((null vals) t)
        ((funcall fn pred-or-preds (car vals) x)
            (all-val pred-or-preds (cdr vals) x))
        (t nil)))

(defun no-val (pred-or-preds vals x &optional (fn #'funcall)) 
    (cond ((null vals) t)
        ((funcall fn pred-or-preds (car vals) x) nil)
        (t (no-val pred-or-preds (cdr vals) x))))

(defun some-val (pred-or-preds vals x &optional (fn #'funcall)) 
    (cond ((null vals) nil)
        ((funcall fn pred-or-preds (car vals) x) t)
        (t (some-val pred-or-preds (cdr vals) x))))

(defun some-not-val (pred-or-preds vals x &optional (fn #'funcall)) 
    (cond ((null vals) nil)
        ((funcall fn pred-or-preds (car vals) x)
            (some-not-val pred-or-preds (cdr vals) x))
        (t t)))

(defun all-pred (preds val x) 
    (cond ((null preds) t)
        ((funcall (car preds) val x)
            (all-pred (cdr preds) val x))
        (t nil)))

(defun no-pred (preds val x) 
    (cond ((null preds) t)
        ((funcall (car preds) val x) nil)
        (t (no-pred (cdr preds) val x))))

(defun some-pred (preds val x) 
    (cond ((null preds) nil)
        ((funcall (car preds) val x) t)
        (t (some-pred (cdr preds) val x))))

(defun some-not-pred (preds val x) 
    (cond ((null preds) nil)
        ((funcall (car preds) val x) 
            (some-not-pred (cdr preds) val x))
        (t t)))

(defun iter-2 (var-fn pred-fn)
    #'(lambda (preds vals x)
        (funcall var-fn preds vals x pred-fn)))