;; Copyright (c) 2022 Wenqiao Zhu (zhuwnq@outlook.com)

(in-package :cl-metric)

(defun make-matrix (h w &key (initial-element 0))
  (make-array `(,h ,w) :initial-element initial-element))

(defun height (mat)
  (array-dimension mat 0))

(defun width (mat)
  (array-dimension mat 1))

(defun rank (mat)
  (length (array-dimensions mat)))

(defun mat-op (m1 m2 binary-op)
  (let* ((h (height m1))
	 (w (width m2))
	 (mat (make-matrix h w)))
    (loop for i from 0 below h do
      (loop for j from 0 below w do
	(setf (aref mat i j) (funcall binary-op (aref m1 i j) (aref m2 i j)))))
    mat))

(defun mat-binary-op (m1 m2 binary-op)
  (assert (or (arrayp m1) (arrayp m2)))
  (typecase m1
    (number (rotatef m1 m2)))
  (if (and (arrayp m1) (arrayp m2))
      (let ((rk1 (rank m1))
	    (rk2 (rank m2)))
	(if (< rk1 rk2)
	    (rotatef m1 m2))))
  (typecase m2
    (number (flet ((op-num (mat num)
		     (let* ((h (height mat))
			    (w (width mat))
			    (res (make-matrix h w)))
		       (dotimes (i h res)
			 (dotimes (j w)
			   (setf (aref res i j) (funcall binary-op (aref mat i j) num)))))))
	      (op-num m1 m2)))
    (array (let* ((h1 (height m1))
		  (w1 (width  m1))
		  (rk (rank   m2))
		  (res (make-matrix h1 w1)))
	     (if (= rk 2)
		 (let ((h2 (height m2))
		       (w2 (width m2)))
		   (assert (and (= h1 h2) (= w1 w2)))
		   (dotimes (i h1 res)
		     (dotimes (j w1)
		       (setf (aref res i j) (funcall binary-op (aref m1 i j) (aref m2 i j)))))
		   res)
		 (let ((w2 (array-dimension m2 0)))
		   (assert (= w1 w2))
		   (dotimes (i h1 res)
		     (dotimes (j w1)
		       (setf (aref res i j) (funcall binary-op (aref m1 i j) (aref m2 j)))))))))
    (otherwise (format t "error! not ok"))))

(defun mat-add (m1 m2)
  (mat-binary-op m1 m2 #'+))

(defun mat-minus (m1 m2)
  (mat-binary-op m1 m2 #'-))

(defun mat-elt-mul (m1 m2)
  (mat-binary-op m1 m2 #'*))

(defun mat-elt-div (m1 m2)
  (mat-binary-op m1 m2 #'/))

(defun mat-mul (m1 m2)
  (let* ((h1 (height m1))
	 (w1 (width m1))
	 (h2 (height m2))
	 (w2 (width m2))
	 (mat (make-matrix h1 w2)))
    (assert (= w1 h2))
    (dotimes (i h1 mat)
      (dotimes (j w2)
	(let ((res 0))
	  (dotimes (k w1)
	    (setf res (+ res (* (aref m1 i k) (aref m2 k j)))))
	  (setf (aref mat i j) res))))))
	  
	




