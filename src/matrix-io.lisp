;; Copyright (c) 2022 Wenqiao Zhu (zhuwnq@outlook.com)

(in-package :cl-metric)

(defun read-mat-file (filepath &key (symmetric t))
  "format:
   height width
   i0 j0 value0
   in jn valuen"
  (labels ((read-file (filename)
	     (with-open-file (in filename)
	       (loop for line = (read-line in nil)
		     while line
		     collect line)))
	   (split-content (str-list)
	     (loop for str in str-list
		   collect (split-sequence:split-sequence #\Space str)))
	   (parse-content (str-arr sym)
	     (let* ((first (car str-arr))
		    (h (parse-integer (car first)))
		    (w (parse-integer (cadr first)))
		    (mat (make-matrix h w))
		    (rest (cdr str-arr)))
	       (loop for lst in rest do
		 (progn
		   (let ((i (parse-integer (car lst)))
			 (j (parse-integer (cadr lst)))
			 (v (parse-float:parse-float (caddr lst))))
		     (if sym
			 (progn
			   (setf (aref mat i j) v)
			   (setf (aref mat j i) v))
			 (setf (aref mat i j) v)))))
	       mat)))
    (parse-content (split-content (read-file filepath)) symmetric)))

	     
	     
	     



