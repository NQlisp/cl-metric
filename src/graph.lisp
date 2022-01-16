;; Copyright (c) 2022 Wenqiao Zhu (zhuwnq@outlook.com)

;; for undirected graph

(in-package :cl-metric)

(defun adj->edge-list (adj node-num &key (undirected t))
  (let ((res nil))
    (if undirected
	(loop for i from 0 below node-num do
	  (loop for j from (1+ i) below node-num do
	    (if (not (= (aref adj i j) 0))
		(push (list i j (aref adj i j)) res))))
	(loop for i from 0 below node-num do
	  (loop for j from 0 below node-num do
	    (if (and (not (= i j)) (not (= (aref adj i j) 0)))
		(push (list i j (aref adj i j)) res)))))
    res))

(defun edge-list->adj (edge-list node-num &key (undirected t))
  (let ((adj (make-matrix node-num node-num)))
    (if undirected
	(dolist (entry edge-list)
	  (let ((i (car entry))
		(j (cadr entry))
		(v (caddr entry)))
	    (setf (aref adj i j) v
		  (aref adj j i) v)))
	(dolist (entry edge-list)
	  (let ((i (car entry))
		(j (cadr entry))
		(v (caddr entry)))
	    (setf (aref adj i j) v))))
    adj))
  

;; for undirected graph
(defun sort-edges (adj n cmp-fn)
  (let ((h (height adj))
	(w (width adj))
	(res nil))
    (loop for i from 0 below h do
      (loop for j from (1+ i) below w do
	(push (list i j (aref adj i j)) res)))
    (setf res (sort res cmp-fn :key #'caddr))
    (subseq res 0 (min n (length res)))))
  
(defun topn-edges (adj n)
  (sort-edges adj n #'>))

(defun downn-edges (adj n)
  (sort-edges adj n #'<))

;; split-forest
;; input-format: '((i0 j0 v0) (i1 j1 v1) ... (in jn vn))
(defun split-forest (edge-list node-num)
  (let ((d-set (make-disjoint-set node-num))
	(forest (make-hash-table :test #'equal))
	(at-key (make-hash-table :test #'equal))
	(key -1))
    (dolist (entry edge-list)
      (let ((i (car entry))
	    (j (cadr entry)))
	(union-disjoint-set d-set i j)))

    (dolist (entry edge-list)
      (let ((at (find-disjoint-set d-set (car entry))))
	(unless (gethash at at-key)
	  (setf (gethash at at-key) (incf key)))
	(push entry (gethash (gethash at at-key) forest))))    
	
    (loop for v being each hash-value of forest
	  append (list v))))

;; greedily bin a forest
(defun bin-forest (edge-list node-num &optional (bin-num 2))
  (let ((forest (split-forest edge-list node-num))
	(cur-bin 0)
	(res (make-array bin-num :initial-element nil))
	(scores (make-array bin-num)))
    (flet ((tree-score (tree)
	     (reduce #'+ (mapcar #'(lambda (edge) (caddr edge)) tree)))
	   (get-bin ()
	     (dotimes (i bin-num)
	       (if (< (aref scores i) (aref scores cur-bin))
		   (setf cur-bin i)))))
      (dolist (tree forest)
	(get-bin)
	(push tree (aref res cur-bin))
	(setf (aref scores cur-bin)
	      (+ (aref scores cur-bin) (tree-score tree)))))
    (values res scores)))

;; dfs bin a forest

;; Minimum Spanning Tree
;; input format '((i j value) (i j value))
(defun kruskal (edge-list node-num)
  (let ((sorted-edge-list (sort edge-list #'< :key #'caddr))
	(res nil)
	(dset (make-disjoint-set node-num)))
    (dolist (entry sorted-edge-list res)
      (let ((i (car entry))
	    (j (cadr entry)))
	(if (not (same-disjoint-set dset i j))
	  (progn
	    (push entry res)
	    (union-disjoint-set dset i j)))))))

;; max-flow min-cut
(defun edmonds-karp (adj node-num source sink &key (inf 1e18))
  (labels ((augment-bfs (parent graph)
	     (let ((visited (make-array node-num :initial-element nil))
		   (queue (make-queue)))
	       (enqueue source queue)
	       (setf (aref visited source) t)
	       (do ((u (dequeue queue) (dequeue queue)))
		   ((eql u nil))
		 (loop for i from 0 below node-num do
		   (if (and
			(eql (aref visited i) nil)
			(> (aref graph u i) 0))
		       (progn
			 (enqueue i queue)
			 (setf (aref visited i) t
			       (aref parent i) u)))))
	       (if (eql (aref visited sink) t) t nil))))
    (let ((res adj)
	  (parent (make-array node-num :initial-element -1))
	  (max-flow 0))
      (do ((path-flow 0)
	   (have (augment-bfs parent res) (augment-bfs parent res)))
	  ((eql have nil) (values max-flow res))
	(setf path-flow (do ((s sink (aref parent s))
			     (flow inf (min flow (aref res (aref parent s) s))))
			    ((= s source) flow)))
	(incf max-flow path-flow)
	(do* ((v sink (aref parent v))
	      (u (aref parent v) (aref parent v)))
	     ((= v source))
	  (decf (aref res u v) path-flow)
	  (incf (aref res v u) path-flow)))
      (values max-flow res))))

;; GraphCut
(defun st-min-cut (adj node-num source sink &key (inf 1e18))
  (multiple-value-bind (max-flow res-graph) (edmonds-karp adj node-num source sink :inf inf)
    (let ((source-set nil)
	  (sink-set nil)
	  (queue (make-queue))
	  (vis (make-array node-num :initial-element nil)))
      (setf (aref vis source) t)
      (push source source-set)
      (enqueue source queue)
      (do ((u (dequeue queue) (dequeue queue)))
	  ((eql u nil))
	(loop for v from 0 below node-num do
	  (if (and
	       (not (aref vis v))
	       (> (aref res-graph u v) 0))
	      (progn
		(enqueue v queue)
		(setf (aref vis v) t)
		(push v source-set)))))
      (values (remove-duplicates source-set :test #'equal)
	      (remove-duplicates
	       (remove-if #'(lambda (x) (member x source-set)) (range 0 node-num)))))))
				 
      
      
