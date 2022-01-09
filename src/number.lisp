;; Copyright (c) 2022 Wenqiao Zhu (zhuwnq@outlook.com)

(in-package :cl-metric)

(defun is-prime (n)
  (let ((flag t))
    (loop for x from 2 to (ceiling (sqrt n))
	  when (= (mod n x) 0)
	    do (setf flag nil))
    flag))

(defun range (begin end)
  (loop for x from begin below end
	collect x))
