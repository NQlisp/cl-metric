(in-package :cl-metric/tests/main)

(deftest disjoint-set
  (testing "make-disjoint-set"
    (ok (equalp (make-disjoint-set 4) #(0 1 2 3))))

  (testing "same-disjiont-set"
    (let ((s (make-disjoint-set 10)))
      (union-disjoint-set s 3 5)
      (union-disjoint-set s 7 9)
      (union-disjoint-set s 5 7)
      (ok (eql t (same-disjoint-set s 3 9)))
      (ng (eql t (same-disjoint-set s 0 7))))))
      
