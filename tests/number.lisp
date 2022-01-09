(in-package :cl-metric/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-metric)' in your Lisp.

(deftest is-prime
  (testing "should (= (is-prime 10) t) to be false"
    (ng (eql (is-prime 10) t)))
  
  (testing "should (= (is-prime 7) t) to be true"
    (ok (eql (is-prime 7) t))))
