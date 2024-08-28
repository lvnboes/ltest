(defpackage :out 
    (:use :cl :colour)
    (:export :test-result))

(in-package :out)

(defun test-result (result pass fail invalid assertions)
    (let* ((total (+ pass fail invalid))
            (not-passed (+ fail invalid))
            (print-colour (cond
                ((> fail 0) #'colour:bright-red)
                ((> invalid 0) #'colour:bright-yellow)
                (t #'colour:bright-green)))
            (result-prefix (cond 
                ((= not-passed 0) "TODO: format pass-string")
                ((= fail 0) "TODO: format invalid-string")
                (t "TODO: format fail-string")))
            (details (if (> not-passed 0)
                "TODO: loop over assertions and print detail" ""))
            (result-suffix (if (> not-passed 0) "TODO overview" "")))
        "TODO: print result with values above"))