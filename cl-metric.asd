(defsystem "cl-metric"
  :version "0.1.0"
  :author "Wenqiao Zhu"
  :depends-on ("alexandria"
	       "parse-float"
	       "split-sequence")
  :components ((:module "src"
                :components
                ((:file "package")
		 (:file "disjoint-set")
		 (:file "queue")
		 (:file "graph")
		 (:file "matrix-io")
		 (:file "matrix")
		 (:file "number"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-metric/tests"))))

(defsystem "cl-metric/tests"
  :author "Wenqiao Zhu"
  :depends-on ("cl-metric"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "package")
		 (:file "disjoint-set")		 
		 (:file "queue")
		 (:file "graph")
		 (:file "matrix-io")
		 (:file "matrix")
		 (:file "number"))))
  :description "Test system for cl-metric"
  :perform (test-op (op c) (symbol-call :rove :run c)))
