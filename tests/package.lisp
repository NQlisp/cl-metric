(defpackage cl-metric/tests/main
  (:use :cl
        :cl-metric
        :rove))
(in-package :cl-metric/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-metric)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
