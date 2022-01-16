;; Copyright (c) 2022 Wenqiao Zhu (zhuwnq@outlook.com)

(defpackage cl-metric
  (:use :alexandria :common-lisp :parse-float :split-sequence)
  (:export
   ;;string
   #:str-find
   ;;activations
   #:sigmoid
   #:softmax
   #:taylor-softmax
   ;; number
   #:range
   #:is-prime
   ;; disjoint-set
   #:make-disjoint-set
   #:find-disjoint-set
   #:union-disjoint-set
   #:same-disjoint-set
   ;; queue
   #:queue
   #:make-queue
   #:queue-q
   #:queue-size
   #:enqueue
   #:dequeue
   ;; graph operations
   #:adj->edge-list
   #:edge-list->adj
   #:topn-edges
   #:downn-edges
   #:split-forest
   #:bin-forest
   #:kruskal
   #:edmonds-karp
   #:st-min-cut
   ;; matrix-io operations
   #:read-mat-file
   ;; matrix operations
   #:make-matrix
   #:height
   #:width
   #:rank
   #:mat-add
   #:mat-minus
   #:mat-elt-mul
   #:mat-elt-div
   #:mat-mul))

(in-package :cl-metric)

;; blah blah blah.
