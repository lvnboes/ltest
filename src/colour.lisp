(defpackage :colour
    (:use :cl)
    (:export :black :red :green :yellow :blue :magenta :cyan :white
        :gray :bright-red :bright-green :bright-yellow :bright-blue
        :bright-magenta :bright-cyan :bright-white))

(in-package :colour)

(defun black (str non-std-out) 
    (if (not non-std-out)
        (format nil "~c[30m~a~c[0m" #\esc str #\esc)
        str))

(defun red (str non-std-out) 
    (if (not non-std-out)
        (format nil "~c[31m~a~c[0m" #\esc str #\esc)
        str))

(defun green (str non-std-out) 
    (if (not non-std-out)
        (format nil "~c[32m~a~c[0m" #\esc str #\esc)
        str))

(defun yellow (str non-std-out) 
    (if (not non-std-out)
        (format nil "~c[33m~a~c[0m" #\esc str #\esc)
        str))

(defun blue (str non-std-out) 
    (if (not non-std-out)
        (format nil "~c[34m~a~c[0m" #\esc str #\esc)
        str))

(defun magenta (str non-std-out) 
    (if (not non-std-out)
        (format nil "~c[35m~a~c[0m" #\esc str #\esc)
        str))

(defun cyan (str non-std-out) 
    (if (not non-std-out)
        (format nil "~c[36m~a~c[0m" #\esc str #\esc)
        str))

(defun white (str non-std-out) 
    (if (not non-std-out)
        (format nil "~c[37m~a~c[0m" #\esc str #\esc)
        str))

(defun gray (str non-std-out) 
    (if (not non-std-out)
        (format nil "~c[90m~a~c[0m" #\esc str #\esc)
        str))

(defun bright-red (str non-std-out) 
    (if (not non-std-out)
        (format nil "~c[91m~a~c[0m" #\esc str #\esc)
        str))

(defun bright-green (str non-std-out) 
    (if (not non-std-out)
        (format nil "~c[92m~a~c[0m" #\esc str #\esc)
        str))

(defun bright-yellow (str non-std-out) 
    (if (not non-std-out)
        (format nil "~c[93m~a~c[0m" #\esc str #\esc)
        str))

(defun bright-blue (str non-std-out) 
    (if (not non-std-out)
        (format nil "~c[94m~a~c[0m" #\esc str #\esc)
        str))

(defun bright-magenta (str non-std-out) 
    (if (not non-std-out)
        (format nil "~c[95m~a~c[0m" #\esc str #\esc)
        str))

(defun bright-cyan (str non-std-out) 
    (if (not non-std-out)
        (format nil "~c[96m~a~c[0m" #\esc str #\esc)
        str))

(defun bright-white (str non-std-out) 
    (if (not non-std-out)
        (format nil "~c[97m~a~c[0m" #\esc str #\esc)
        str))