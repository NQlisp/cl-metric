;; Copyright (c) 2022 Wenqiao Zhu (zhuwnq@outlook.com)

(in-package :cl-metric)

(defun sigmoid (x)
  (/ 1.0 (1+ (exp (* -1.0 x)))))

(defun softmax (arr)
  (flet ((sum-exp (arr)
	   (let ((size (array-dimension arr 0))
		 (res 0))
	     (loop for i from 0 below size do
	       (incf res (exp (aref arr i))))
	     res)))
    (let* ((sum (sum-exp arr))
	   (size (array-dimension arr 0))
	   (res (make-array size)))
      (loop for i from 0 below size do
	(setf (aref res i) (/ (exp (aref arr i)) sum)))
      res)))

(defun taylor-softmax (arr)
  (flet ((sum-taylor (arr)
	   (let ((size (array-dimension arr 0))
		 (res 0))
	     (loop for i from 0 below size do
	       (let ((x (aref arr i)))
		 (incf res (+ 1 x (* 0.5 x x)))))
	     res)))
    (let* ((sum (sum-taylor arr))
	   (size (array-dimension arr 0))
	   (res (make-array size)))
      (loop for i from 0 below size do
	(let* ((x (aref arr i))
	       (tx (+ 1 x (* 0.5 x x))))
	  (setf (aref res i) (/ tx sum))))
      res)))
