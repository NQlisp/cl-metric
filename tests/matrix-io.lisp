(in-package :cl-metric/tests/main)


(deftest read-mat-file
  (testing "read-mat-file-sym"
    (let ((mat nil))
      (setf mat (read-mat-file "~/cl-metric/tests/read-mat-file.test"))
      (ok (= 2 (rank mat)))
      (ok (= 4 (width mat)))
      (ok (= 4 (height mat)))
      (ok (= 4.4 (aref mat 3 0)))
      (ok (= 4.4 (aref mat 0 3)))
      (ok (= 9.9 (aref mat 3 2)))
      (ok (= 9.9 (aref mat 2 3))))))

(deftest read-mat-file-no-sym
  (testing "read-mat-file-no-sym"
    (let ((mat nil))
      (setf mat (read-mat-file "~/cl-metric/tests/read-mat-file.test" :symmetric nil))
      (ok (= 2 (rank mat)))
      (ok (= 4 (width mat)))
      (ok (= 4 (height mat)))
      (ok (= 0 (aref mat 3 0)))
      (ok (= 4.4 (aref mat 0 3)))
      (ok (= 0 (aref mat 3 2)))
      (ok (= 9.9 (aref mat 2 3))))))

