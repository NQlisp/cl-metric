;; Copyright (c) 2022 Wenqiao Zhu (zhuwnq@outlook.com)

(in-package :cl-metric)

(defun make-disjoint-set (n)
  (make-array n :initial-contents (range 0 n)))


(defun find-disjoint-set (s k)
  (if (= (aref s k) k)
      k
      (setf (aref s k) (find-disjoint-set s (aref s k)))))

(defun union-disjoint-set (s a b)
  (let ((x (find-disjoint-set s a))
	(y (find-disjoint-set s b)))
    (if (not (= x y))
	(setf (aref s y) x))))

(defun same-disjoint-set (s a b)
  (if (= (find-disjoint-set s a)
	 (find-disjoint-set s b))
      t
      nil))
