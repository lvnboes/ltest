(defpackage :lpred
    (:use :cl)
    (:export 
        :not-eq-p :not-eql-p :not-equal-p :not-equalp-p
        :hash-table-equal-p))

(in-package :lpred)

#|
Custom predicates
 |#

(defun not-eq-p (v1 v2)
    (not (eq v1 v2)))

(defun not-eql-p (v1 v2)
    (not (eql v1 v2)))

(defun not-equal-p (v1 v2)
    (not (equal v1 v2)))

(defun not-equalp-p (v1 v2)
    (not (equalp v1 v2)))

 (defun hash-table-equal-p (t-1 t-2 &optional (predicate #'equal))
    "Test equality between two hash tables by testing if they have the same
        size and if the keys of table one when used on table 2 refer to values
        that evaluate to true with the predicate equal (default predicate, can be changed)."
    (if (= (hash-table-count t-1) (hash-table-count t-2))
        (loop for key being the hash-keys of t-1 always 
            (and (gethash key t-1)
                (funcall predicate (gethash key t-1) (gethash key t-2))))
        nil))


