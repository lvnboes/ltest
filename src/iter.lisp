(defpackage :inter
    (:use :cl)
    (:export :all-val :no-val :some-val :some-not-val
        :all-pred :no-pred :some-pred :some-not-pred))

(defun all-val (pred vals x) 
    (cond ((null vals) t)
        ((funcall pred (car vals) x)
            (all-val pred (cdr vals) x))
        (t nil)))

(defun no-val (pred vals x) 
    (cond ((null vals) t)
        ((funcall pred (car vals) x) nil)
        (t (no-val pred (cdr vals) x))))

(defun some-val (pred vals x) 
    (cond ((null vals) nil)
        ((funcall pred (car vals) x) t)
        (t (some-val pred (cdr vals) x))))

(defun some-not-val (pred vals x) 
    (cond ((null vals) nil)
        ((funcall pred (car vals) x)
            (some-not-val pred (cdr vals) x))
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

(defun all-all (preds vals x)
    (cond ((null vals) t)
        ((all-pred preds (car vals) x)
            (all-val preds (cdr vals) x))
        (t nil)))
