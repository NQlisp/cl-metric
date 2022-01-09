(in-package :cl-metric/tests/main)

(deftest st-min-cut
  (testing "st-min-cut-graph-1"
    (let ((adj #2A((0 16 13 0 0 0)
		   (0 0 10 12 0 0)
		   (0 4 0 0 14 0)
		   (0 0 9 0 0 20)
		   (0 0 0 7 0 4)
		   (0 0 0 0 0 0)))
	  (node-num 6))
      (multiple-value-bind (s-set t-set) (st-min-cut adj node-num 0 5)
	(ok (equal s-set '(4 2 1 0)))
	(ok (equal t-set '(3 5))))))
  (testing "st-min-cut-graph-2"
    (let ((adj2 #2A((0 0 1 100000)
		    (100000 0 100000 0)
		    (0 0 0 100000)
		    (0 0 0 0)))
	  (node-num 4))
      (multiple-value-bind (s-set t-set) (st-min-cut adj2 node-num 1 3)
	(ok (equal s-set '(1)))
	(ok (equal t-set '(0 2 3)))))))

    
      


(deftest edmonds-karp
  (testing "edmonds-karp-1"
    (let ((adj #2A((0 16 13 0 0 0)
		   (0 0 10 12 0 0)
		   (0 4 0 0 14 0)
		   (0 0 9 0 0 20)
		   (0 0 0 7 0 4)
		   (0 0 0 0 0 0)))
	  (node-num 6))
      (ok (= 23 (edmonds-karp adj node-num 0 5))))))

(deftest edmonds-karp-2
  (testing "edmonds-karp-2"
    (let ((adj2 #2A((0 0 1 100000)
		    (100000 0 100000 0)
		    (0 0 0 100000)
		    (0 0 0 0)))
	  (node-num 4))
      (ok (= 200000 (edmonds-karp adj2 node-num 1 3))))))

(deftest adj->edge-list
  (testing "adj->edge-list"
    (let ((adj (make-array '(4 4) :initial-contents '((0 1 0.0 0) (1 0.0 -1 0) (0 -1 0 0.3) (0 0 0.3 0)))))
      (ok (equal (cl-metric:adj->edge-list adj 4) '((2 3 0.3) (1 2 -1) (0 1 1))))
      (ok (equal (cl-metric:adj->edge-list adj 4 :undirected nil) '((3 2 0.3) (2 3 0.3) (2 1 -1) (1 2 -1) (1 0 1) (0 1 1)))))))

(deftest edge-list->adj
  (testing "edge-list->adj"
    (let ((edge-list '((2 3 0.3) (1 2 -1) (0 1 1))))
      (ok (equalp (cl-metric:edge-list->adj edge-list 4 :undirected nil) #2A((0 1 0 0) (0 0 -1 0) (0 0 0 0.3) (0 0 0 0))))
      (ok (equalp (cl-metric:edge-list->adj edge-list 4) #2A((0 1 0 0) (1 0 -1 0) (0 -1 0 0.3) (0 0 0.3 0)))))))

(deftest topn-edges
  (testing "topn-edges"
    (let ((adj (make-array '(4 4) :initial-contents '((1.1 2.2 3.3 4.4)
						      (2.2 5.5 6.6 7.7)
						      (3.3 6.6 8.8 9.9)
						      (4.4 7.7 9.9 10.1))))
	  (res nil))
      (setf res (topn-edges adj 3))
      (ok (equal (elt res 0) '(2 3 9.9)))
      (ok (equal (elt res 1) '(1 3 7.7)))
      (ok (equal (elt res 2) '(1 2 6.6))))))


(deftest split-forest
  (testing "split-forest"
    (let ((tmp nil)
	  (e-list '((2 3 1.0) (7 8 4.0) (2 4 1.0))))
      (setf tmp (split-forest e-list))
      (ok (equal (elt tmp 0) '((2 4 1.0) (2 3 1.0))))
      (ok (equal (elt tmp 1) '((7 8 4.0)))))))


(deftest bin-forest
  (testing "bin-forest"
    (let ((tmp nil)
	  (res #(3.9 1.0 4.0 6.0))
	  (e-list '((0 7 1.0) (11 4 0.5) (2 3 4.0) (5 10 2.0)
		    (4 12 0.5) (5 6 2.0) (6 10 2.0) (7 8 1.6) (8 9 1.3))))
      (multiple-value-bind (b s) (bin-forest e-list 4)
	  (ok (equalp s res))))))
	  
(deftest kruskal
  (testing "kruskal"
    (let ((e-list '((0 1 2.2) (0 2 3.3) (0 3 4.4) (1 2 6.6) (1 3 7.7) (2 3 9.9)))
	  (e2-list '((0 1 -2.2) (0 2 -3.3) (0 3 -4.4) (1 2 -6.6) (1 3 -7.7) (2 3 -9.9))))

      (ok (equal (kruskal e-list 4) '((0 3 4.4) (0 2 3.3) (0 1 2.2))))
      (ok (equal (kruskal e2-list 4) '((0 3 -4.4) (1 3 -7.7) (2 3 -9.9)))))))


