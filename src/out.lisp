(defpackage :out 
    (:use :cl :colour)
    (:export :test-result))

(in-package :out)

(defparameter *output-width* 100)

(defun test-result-prefix (test-name result &optional (width *output-width*) (pad-char #\.))
    "Based on the test name and result, create a prefix for the test report"
    (let* ((name-len (length test-name))
            (result-len (length result))
            (padding-len (- width name-len result-len 2))
            (padding (make-string padding-len :initial-element pad-char)))
        (format nil "~a ~a ~a~%" test-name padding result)))

(defun test-result-assertions (assertions &optional (assertion-strings '()))
    "Based on the details of the assertions in the test execution, create a detailed report string 
        on all the failed and invalid assertions for inclusion in the tests reporting message 
        and return the assertion report string. Empty if the test passed"
    (if (null assertions) 
        (apply 'concatenate 'string (reverse assertion-strings))
        (progn
            (if (not (eq (getf (car assertions) :result) :pass))
                (let* ((assertion (car assertions))
                        (overview (format nil "ASSERTION ~a WITH CHECK ~a AND PRED ~a~%"
                            (getf assertion :result)
                            (getf assertion :check)
                            (getf assertion :pred)))
                        (exp (getf assertion :exp))
                        (expected (format nil "    Expected ~a of type ~a~%" 
                            exp (type-of exp)))
                        (val (getf assertion :val))
                        (got-val (format nil "    Got value ~a of type ~a~%"
                            val (type-of val)))
                        (err (getf assertion :error))
                        (error (if err
                            (format nil "~a~%" err)
                            "")))
                    (push (format nil "~a~a~a~a" overview expected got-val error)
                        assertion-strings)))
            (test-result-assertions (cdr assertions) assertion-strings))))

(defun test-result-suffix (pass fail invalid &optional (width *output-width*) (pad-char #\Space))
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

(defun test-result (result-table)
    "Based on the results of a unit test, create a reporting string 
        and push to output."
    (let* ((name (gethash :name result-table))
            (result (gethash :result result-table))
            (total (+ (gethash :pass result-table 0) 
                (gethash :fail result-table 0) 
                (gethash :invalid result-table 0)))
            (print-colour (case result
                (:pass #'colour:bright-green)
                (:fail #'colour:bright-red)
                (:invalid #'colour:bright-yellow)))
            (result-prefix (case result 
                (:pass (test-result-prefix name "[PASS]"))
                (:fail (test-result-prefix name "[FAIL]"))
                (:invalid  (test-result-prefix name "[INVD]"))))
            (details (if (not (equal result :pass))
                (test-result-assertions (gethash :assertions result-table)) 
                ""))
            (result-suffix (if (not (eq result :pass)) 
                (test-result-suffix 
                    (gethash :pass result-table)
                    (gethash :fail result-table)
                    (gethash :invalid result-table)) 
                ""))
            (test-result-message 
                (format nil "~a~a~a" result-prefix details result-suffix)))
        (format t (funcall print-colour test-result-message))
    ))