;; Copyright (c) 2022 Wenqiao Zhu (zhuwnq@outlook.com)

(in-package :cl-metric)

(defstruct queue
  (q (cons nil nil))
  (size 0))

(defun enqueue (o q)
  (incf (queue-size q))  
  (if (null (car (queue-q q)))
      (setf (cdr (queue-q q)) (setf (car (queue-q q)) (list o)))
      (setf (cdr (cdr (queue-q q))) (list o)
	    (cdr (queue-q q)) (cdr (cdr (queue-q q))))))

(defun dequeue (q)
  (if (> (queue-size q) 0)
      (progn
	(decf (queue-size q))	
	(pop (car (queue-q q))))))
