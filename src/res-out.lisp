(defpackage :res-out
    (:use :cl)
    (:export :to-output :pass :fail :invalid
        :show-test-set-result :show-test-suite-result))

(in-package :res-out)

(defparameter *output-file* nil)

#|
Wrapper to optonally write to a specific output file
 |#

(defun to-output (&key test-suite output-file)
    (let ((result nil))
        (if output-file
            (progn
                (setf *output-file* output-file)
                (with-open-file (stream *output-file*
                    :direction :output
                    :if-exists :overwrite
                    :if-does-not-exist :create)
                    (let ((*standard-output* stream))
                        (setf result (eval test-suite)))))
        (setf result (eval test-suite)))
        result
    ))

#|
Formatting of result strings
 |#

(defun insert-padding (str1 str2 &key (len 100) (pad-char #\.))
    (let* ((len1 (length str1))
            (len2 (length str2))
            (len-padding (max (- len (+ len1 len2)) 0))
            (padding-str (make-string len-padding :element-type 'character :initial-element pad-char)))
        (format nil "~a~a~a" str1 padding-str str2)))
 
#|
Format and output test result
 |#

(defun pass (f-name var1 var2 &key (predicates nil) (test-name nil)) 
    (let* ((pred-str (if predicates (format nil " ~{~a~^ ~} |" predicates) ""))
            (test-name-str (if test-name (format nil "~a | " test-name) ""))
            (result-str (format nil
                "~%~a~a |~a ~a ~a | ~a ~a "
                test-name-str f-name pred-str
                (type-of var1) var1 (type-of var2) var2)))
        (format t (colour:bright-green 
            (insert-padding result-str " [PASS]") 
            *output-file*))
        :pass))

(defun fail (f-name var1 var2 &key (predicates nil) (test-name nil)) 
    (let* ((pred-str (if predicates (format nil " ~{~a~^ ~} |" predicates) ""))
            (test-name-str (if test-name (format nil "~a | " test-name) ""))
            (result-str (format nil
                "~%~a~a |~a ~a ~a | ~a ~a "
                test-name-str f-name pred-str
                (type-of var1) var1 (type-of var2) var2)))
        (format t (colour:bright-red 
            (insert-padding result-str " [FAIL]")
            *output-file*))
        :fail))
    

(defun invalid (f-name var1 var2 &key (predicates nil) (test-name nil)) 
    (let* ((pred-str (if predicates (format nil " ~{~a~^ ~} |" predicates) ""))
            (test-name-str (if test-name (format nil "~a | " test-name) ""))
            (result-str (format nil
                "~%~a~a |~a ~a ~a | ~a ~a "
                test-name-str f-name pred-str
                (type-of var1) var1 (type-of var2) var2)))
        (format t (colour:bright-yellow 
            (insert-padding result-str " [INVD]")
            *output-file*))
        :invalid))

#|
Format and output test set and test- uite results
 |#


(defun show-test-set-result (name result-hash-table)
    (let* ((passed (gethash :pass result-hash-table 0))
            (failed (gethash :fail result-hash-table 0))
            (invalid (gethash :invalid result-hash-table 0))
            (total (+ failed passed invalid)))
        (format t 
            "~%Results of test set - ~a~%~\t~\tPASSED: ~a~\t~\tFAILED: ~a~\t~\tINVALID: ~a~%~\t~\tTOTAL: ~a TESTS~%~%"
            name passed failed invalid total)
        result-hash-table))

 (defun show-test-suite-result (result-hash-table) 
    (let* ((passed (gethash :pass result-hash-table 0))
            (failed (gethash :fail result-hash-table 0))
            (invalid (gethash :invalid result-hash-table 0))
            (total (+ failed passed invalid))
            (sets (gethash :sets result-hash-table 0))
            (all-passed? (eq (+ failed invalid) 0))
            (result-str
                (format nil
                    "~%~a~%~%FINAL RESULT OF ~a TEST SETS IN SUITE~%~\t~\tPASSED: ~a~\t~\tFAILED: ~a~\t~\tINVALID: ~a~%~\t~\tTOTAL: ~a TESTS~%~%"
                    (insert-padding "" "" :pad-char #\-) sets passed failed invalid total)))
        (format t (if all-passed? 
            (colour:bright-green 
                result-str 
                *output-file*) 
            (colour:bright-blue 
                result-str 
                *output-file*)))
        all-passed?))