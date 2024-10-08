(defpackage :out 
    (:use :cl :colour)
    (:export :get-current-output-stream :set-current-output-stream
        :test-out :test-set-out :test-suite-out))

(in-package :out)

(defparameter *output-width* 80)

(defparameter *output-stream* t)

(defun get-current-output-stream ()
    *output-stream*)

(defun set-current-output-stream (new-output-stream)
    (setf *output-stream* new-output-stream))

(defun get-colour-fun (result)
    "Determine which coloration function to use based on the result"
    (case result
        (:pass #'colour:bright-green)
        (:fail #'colour:bright-red)
        (:invalid #'colour:bright-yellow)))

(defun test-out-prefix (test-name result &optional (width *output-width*) (pad-char #\.))
    "Based on the test name and result, create a prefix for the test report"
    (let* ((name-len (length test-name))
            (result-len (length result))
            (padding-len (- width name-len result-len 2))
            (padding (make-string padding-len :initial-element pad-char)))
        (format nil "~a ~a ~a~%" test-name padding result)))

(defun test-out-assertions (assertions &optional (assertion-strings '()))
    "Based on the details of the assertions in the test execution, create a detailed report string 
        on all the failed and invalid assertions for inclusion in the tests reporting message 
        and return the assertion report string. Empty if the test passed"
    (if (null assertions) 
        (apply 'concatenate 'string (reverse assertion-strings))
        (progn
            (if (not (eq (getf (car assertions) :result) :pass))
                (let* ((assertion (car assertions))
                        (overview (format nil "ASSERTION ~a WITH CHECK ~a~%"
                            (getf assertion :result)
                            (getf assertion :check)))
                        (predicates (format nil "    With predicate(s) ~a~%"
                            (getf assertion :pred)))
                        (exp (getf assertion :exp))
                        (expected (format nil "    Expected value ~s of type ~a~%" 
                            exp (type-of exp)))
                        (val (getf assertion :val))
                        (got-val (format nil "    Got value ~s of type ~a~%"
                            val (type-of val)))
                        (err (getf assertion :error))
                        (error (if err
                            (format nil "~a~%" err)
                            "")))
                    (push (format nil "~a~a~a~a~a" overview predicates expected got-val error)
                        assertion-strings)))
            (test-out-assertions (cdr assertions) assertion-strings))))

(defun test-out-suffix (pass fail invalid &optional (width *output-width*) (pad-char #\Space))
    "Based on number of passed, failed and invalid assertions in the test execution, create a suffix 
        string for the reporting message and return the suffix string. Empty if the test passed."
    (let* ((total-str (format nil "Total assertions: ~a " (+ pass fail invalid)))
            (passed-str (format nil "Passed: ~a " pass))
            (failed-str (format nil "Failed: ~a " fail))
            (invalid-str (format nil "Invalid ~a" invalid))
            (total-padding (- width 
                (length total-str) 
                (length passed-str) 
                (length failed-str) 
                (length invalid-str)))
            (padding-len (floor (/ total-padding 3)))
            (padding (make-string padding-len :initial-element pad-char)))
        (format nil "~a~a~a~a~a~a~a~%"
            total-str padding 
            passed-str padding 
            failed-str padding 
            invalid-str)))

(defun test-out (result-table)
    "Based on the results of a unit test, create a reporting string 
        and push to output."
    (let* ((name (gethash :name result-table))
            (result (gethash :result result-table))
            (print-colour (get-colour-fun result))
            (result-prefix (case result 
                (:pass (test-out-prefix name "[PASS]"))
                (:fail (test-out-prefix name "[FAIL]"))
                (:invalid  (test-out-prefix name "[INVD]"))))
            (details (if (not (equal result :pass))
                (test-out-assertions (gethash :assertions result-table)) 
                ""))
            (result-suffix (if (not (eq result :pass)) 
                (test-out-suffix 
                    (gethash :pass result-table)
                    (gethash :fail result-table)
                    (gethash :invalid result-table)) 
                ""))
            (test-result-message 
                (format nil "~a~a~a" result-prefix details result-suffix)))
        (format *output-stream* (funcall print-colour test-result-message *output-stream*))))

(defun test-set-out (result-table)
    "Based on the results of a test set, create a reporting string 
        and push to output."
    (let* ((separator (make-string  *output-width* :initial-element #\-))
            (print-colour (get-colour-fun (gethash :result result-table)))
            (result-str (format nil 
                "~a~%Test set '~a' with ~a tests~%Result: ~a~%Passed: ~a    Failed: ~a    Invalid: ~a~%~%~%" 
                separator
                (string-downcase (gethash :name result-table))
                (+ (gethash :pass result-table) (gethash :fail result-table) (gethash :invalid result-table))
                (gethash :result result-table)
                (gethash :pass result-table)
                (gethash :fail result-table)
                (gethash :invalid result-table))))
        (format *output-stream* (funcall print-colour result-str *output-stream*))))

(defun test-suite-out (result-table)
    "Based on the results of a test suite, create a reporting string 
        and push to output."
    (let* ((separator (make-string  *output-width* :initial-element #\-))
            (print-colour (get-colour-fun (gethash :result result-table)))
            (result-str (format nil 
                "~%~%~a~%~a~%TEST SUITE '~a' WITH ~a TEST SETS~%RESULT: ~a~%SETS PASSED: ~a        SETS FAILED: ~a~%~a~%~a~%~%" 
                separator separator
                (string-upcase (gethash :name result-table))
                (+ (gethash :pass result-table) (gethash :fail result-table))
                (gethash :result result-table)
                (gethash :pass result-table)
                (gethash :fail result-table)
                separator separator)))
        (format *output-stream* (funcall print-colour result-str *output-stream*))
        (format *output-stream* "~%Powered by Ltest - v.1.0.0 - 2024.08.31 - (c) Lieven Boes~%~%")))