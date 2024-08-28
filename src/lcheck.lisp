(defpackage :lcheck
    (:use :cl :iter)
    (:export :check-true :check-false
        :check-all-v :check-no-v :check-some-v :check-some-not-v
        :check-all-p :check-no-p :check-some-p :check-some-not-p
        :check-all-v-all-p :check-all-v-no-p :check-all-v-some-p 
        :check-all-v-some-not-p :check-no-v-all-p :check-no-v-no-p 
        :check-no-v-some-p :check-no-v-some-not-p :check-some-v-all-p 
        :check-some-v-no-p :check-some-v-some-p :check-some-v-some-not-p
        :check-some-not-v-all-p :check-some-not-v-no-p :check-some-not-v-some-p 
        :check-some-not-v-some-not-p))

(in-package :lcheck)

#|
Simple checks
 |#

(defun check-true (pred val exp) 
    (pred val exp))

(defun check-false (pred val exp)
    (not (pred val exp)))

#|
Iterative checks
 |#

(defun check-all-v (pred vals exp) 
    (iter:all-val pred vals exp))

(defun check-no-v (pred vals exp) 
    (iter:no-val pred vals exp))

(defun check-some-v (pred vals exp) 
    (iter:some-val pred vals exp))

(defun check-some-not-v (pred vals exp) 
    (iter:some-not-val pred vals exp))

(defun check-all-p (pred vals exp) 
    (iter:all-pred pred vals exp))

(defun check-no-p (pred vals exp) 
    (iter:no-pred pred vals exp))

(defun check-some-p (pred vals exp) 
    (iter:some-pred pred vals exp))

(defun check-some-not-p (pred vals exp) 
    (iter:some-not-pred pred vals exp))

#|
Double iterative checks
 |#

(defun check-all-v-all-p (preds vals exp)
    (funcall (iter:iter-2 #'iter:all-val #'iter:all-pred)
        preds vals exp))

(defun check-all-v-no-p (preds vals exp)
    (funcall (iter:iter-2 #'iter:all-val #'iter:no-pred)
        preds vals exp))

(defun check-all-v-some-p (preds vals exp)
    (funcall (iter:iter-2 #'iter:all-val #'iter:some-pred)
        preds vals exp))

(defun check-all-v-some-not-p (preds vals exp)
    (funcall (iter:iter-2 #'iter:all-val #'iter:some-not-pred)
        preds vals exp))

(defun check-no-v-all-p (preds vals exp)
    (funcall (iter:iter-2 #'iter:no-val #'iter:all-pred)
        preds vals exp))

(defun check-no-v-no-p (preds vals exp)
    (funcall (iter:iter-2 #'iter:no-val #'iter:no-pred)
        preds vals exp))

(defun check-no-v-some-p (preds vals exp)
    (funcall (iter:iter-2 #'iter:no-val #'iter:some-pred)
        preds vals exp))

(defun check-no-v-some-not-p (preds vals exp)
    (funcall (iter:iter-2 #'iter:no-val #'iter:some-not-pred)
        preds vals exp))

(defun check-some-v-all-p (preds vals exp)
    (funcall (iter:iter-2 #'iter:some-val #'iter:all-pred)
        preds vals exp))

(defun check-some-v-no-p (preds vals exp)
    (funcall (iter:iter-2 #'iter:some-val #'iter:no-pred)
        preds vals exp))

(defun check-some-v-some-p (preds vals exp)
    (funcall (iter:iter-2 #'iter:some-val #'iter:some-pred)
        preds vals exp))

(defun check-some-v-some-not-p (preds vals exp)
    (funcall (iter:iter-2 #'iter:some-val #'iter:some-not-pred)
        preds vals exp))

(defun check-some-not-v-all-p (preds vals exp)
    (funcall (iter:iter-2 #'iter:some-not-val #'iter:all-pred)
        preds vals exp))

(defun check-some-not-v-no-p (preds vals exp)
    (funcall (iter:iter-2 #'iter:some-not-val #'iter:no-pred)
        preds vals exp))

(defun check-some-not-v-some-p (preds vals exp)
    (funcall (iter:iter-2 #'iter:some-not-val #'iter:some-pred)
        preds vals exp))

(defun check-some-not-v-some-not-p (preds vals exp)
    (funcall (iter:iter-2 #'iter:some-not-val #'iter:some-not-pred)
        preds vals exp))
