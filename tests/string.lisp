(in-package :cl-metric/tests/main)

(deftest string
  (testing "str-find"
    (ok (= (str-find "this_is_a_test" #\_) 4))
    (ok (= (str-find "this_is_a_test" #\_ :start 5) 7))))
