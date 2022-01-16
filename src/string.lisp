;; Copyright (c) 2022 Wenqiao Zhu (zhuwnq@outlook.com)

(in-package :cl-metric)


(defun str-find (str chr &key (start 0))
  (let ((size (length str)))
    (loop for i from start below size do
      (if (char= (aref str i) chr)
	  (return-from str-find i)))))
      
