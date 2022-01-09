(in-package :cl-metric/tests/main)


(deftest queue
  (testing "queue"
    (let ((q (make-queue)))
      (ok (= (queue-size q) 0))
      (enqueue 0 q)
      (ok (= (queue-size q) 1))
      (enqueue 1 q)
      (ok (= (queue-size q) 2))
      (enqueue 1 q)
      (ok (= (queue-size q) 3))
      (dequeue q)
      (enqueue 1 q)
      (ok (= (queue-size q) 3)))))


(deftest dequeue
  (testing "dequeue"
    (let ((q (make-queue)))
      (enqueue 0 q)
      (enqueue 1 q)
      (enqueue 1 q)
      (ok (dequeue q) 0)
      (enqueue 2 q)
      (ok (dequeue q) 1)
      (ok (dequeue q) 1)
      (ok (dequeue q) 2))))
      
      

      



      
