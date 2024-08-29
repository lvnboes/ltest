(defpackage :colour
    (:use :cl)
    (:export :black :red :green :yellow :blue :magenta :cyan :white
        :gray :bright-red :bright-green :bright-yellow :bright-blue
        :bright-magenta :bright-cyan :bright-white))

(in-package :colour)

(defun black (str &optional (non-std-out nil)) 
    "Apply ansi escape chars to turn a string black 
        if output-stream is standard and return the string"
    (if (not non-std-out)
        (format nil "~c[30m~a~c[0m" #\esc str #\esc)
        str))

(defun red (str &optional (non-std-out nil)) 
    "Apply ansi escape chars to turn a string red 
        if output-stream is standard and return the string"
    (if (not non-std-out)
        (format nil "~c[31m~a~c[0m" #\esc str #\esc)
        str))

(defun green (str &optional (non-std-out nil)) 
    "Apply ansi escape chars to turn a string green 
        if output-stream is standard and return the string"
    (if (not non-std-out)
        (format nil "~c[32m~a~c[0m" #\esc str #\esc)
        str))

(defun yellow (str &optional (non-std-out nil)) 
    "Apply ansi escape chars to turn a string yellow 
        if output-stream is standard and return the string"
    (if (not non-std-out)
        (format nil "~c[33m~a~c[0m" #\esc str #\esc)
        str))

(defun blue (str &optional (non-std-out nil)) 
    "Apply ansi escape chars to turn a string blue 
        if output-stream is standard and return the string"
    (if (not non-std-out)
        (format nil "~c[34m~a~c[0m" #\esc str #\esc)
        str))

(defun magenta (str &optional (non-std-out nil)) 
    "Apply ansi escape chars to turn a string magenta 
        if output-stream is standard and return the string"
    (if (not non-std-out)
        (format nil "~c[35m~a~c[0m" #\esc str #\esc)
        str))

(defun cyan (str &optional (non-std-out nil)) 
    "Apply ansi escape chars to turn a string cyan 
        if output-stream is standard and return the string"
    (if (not non-std-out)
        (format nil "~c[36m~a~c[0m" #\esc str #\esc)
        str))

(defun white (str &optional (non-std-out nil)) 
    "Apply ansi escape chars to turn a string white 
        if output-stream is standard and return the string"
    (if (not non-std-out)
        (format nil "~c[37m~a~c[0m" #\esc str #\esc)
        str))

(defun gray (str &optional (non-std-out nil)) 
    "Apply ansi escape chars to turn a string gray 
        if output-stream is standard and return the string"
    (if (not non-std-out)
        (format nil "~c[90m~a~c[0m" #\esc str #\esc)
        str))

(defun bright-red (str &optional (non-std-out nil)) 
    "Apply ansi escape chars to turn a string bright red 
        if output-stream is standard and return the string"
    (if (not non-std-out)
        (format nil "~c[91m~a~c[0m" #\esc str #\esc)
        str))

(defun bright-green (str &optional (non-std-out nil)) 
    "Apply ansi escape chars to turn a string bright green 
        if output-stream is standard and return the string"
    (if (not non-std-out)
        (format nil "~c[92m~a~c[0m" #\esc str #\esc)
        str))

(defun bright-yellow (str &optional (non-std-out nil))
    "Apply ansi escape chars to turn a string bright yellow 
        if output-stream is standard and return the string" 
    (if (not non-std-out)
        (format nil "~c[93m~a~c[0m" #\esc str #\esc)
        str))

(defun bright-blue (str &optional (non-std-out nil)) 
    "Apply ansi escape chars to turn a string bright blue 
        if output-stream is standard and return the string"
    (if (not non-std-out)
        (format nil "~c[94m~a~c[0m" #\esc str #\esc)
        str))

(defun bright-magenta (str &optional (non-std-out nil)) 
    "Apply ansi escape chars to turn a string bright magenta 
        if output-stream is standard and return the string"
    (if (not non-std-out)
        (format nil "~c[95m~a~c[0m" #\esc str #\esc)
        str))

(defun bright-cyan (str &optional (non-std-out nil)) 
    "Apply ansi escape chars to turn a string bright cyan 
        if output-stream is standard and return the string"
    (if (not non-std-out)
        (format nil "~c[96m~a~c[0m" #\esc str #\esc)
        str))

(defun bright-white (str &optional (non-std-out nil)) 
    "Apply ansi escape chars to turn a string bright white 
        if output-stream is standard and return the string"
    (if (not non-std-out)
        (format nil "~c[97m~a~c[0m" #\esc str #\esc)
        str))