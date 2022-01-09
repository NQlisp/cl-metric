(in-package :cl-metric/tests/main)


(deftest make-matrix
  (testing "make-matrix"
	   (let ((mat (make-matrix 2 3)))
	     (ok (= (rank mat) 2))
	     (ok (= (width mat) 3))
	     (ok (= (height mat) 2))
	     (ok (= (aref mat 0 0) 0)))))


(deftest mat-add
  (testing "mat add num"
    (let ((mat (make-matrix 2 3 :initial-element 5))
	  (num 4)
	  (res (make-matrix 2 3)))
      (setf res (mat-add mat num))
      (dotimes (i 2)
	(dotimes (j 3)
	  (ok (= (aref res i j) 9))))))
  
  (testing "num add mat"
    (let ((mat (make-matrix 2 3 :initial-element 5))
	  (num 4)
	  (res (make-matrix 2 3)))
      (setf res (mat-add num mat))
      (dotimes (i 2)
	(dotimes (j 3)
	  (ok (= (aref res i j) 9))))))

  (testing "mat add mat"
    (let ((m1 (make-matrix 2 3 :initial-element 5))
	  (m2 (make-matrix 2 3 :initial-element 6))
	  (res (make-matrix 2 3)))
      (setf res (mat-add m1 m2))
      (dotimes (i 2)
	(dotimes (j 3)
	  (ok (= (aref res i j) 11))))))

  (testing "mat add vector"
    (let ((m1 (make-matrix 2 3 :initial-element 5))
	  (v2 (make-array 3 :initial-element 3))
	  (res (make-matrix 2 3)))
      (setf res (mat-add m1 v2))
      (dotimes (i 2)
	(dotimes (j 3)
	  (ok (= (aref res i j) 8))))))
  
  (testing "vector add mat"
    (let ((m1 (make-matrix 2 3 :initial-element 5))
	  (v2 (make-array 3 :initial-element 3))
	  (res (make-matrix 2 3)))
      (setf res (mat-add v2 m1))
      (dotimes (i 2)
	(dotimes (j 3)
	  (ok (= (aref res i j) 8)))))))

  
