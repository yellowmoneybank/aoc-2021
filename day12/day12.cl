(load "../utils.cl")
(ql:quickload :cl-ppcre)
(ql:quickload :array-operations)
(ql:quickload :serapeum)
(ql:quickload :alexandria)

(declaim (optimize (speed 0) (space 0) (debug 3)))

(defstruct node
  val
  (children '()))

(defun get-node(val nodes)
  (let ((node (find-if #'(lambda(x)(string= val (node-val x))) nodes)))
	(if node
		(values node nodes)
		(let ((new-node (make-node :val val)))
		  (values new-node (cons new-node nodes))))))

(defun make-graph (input-file)
  (let ((lines (get-file-linewise input-file))
		(nodes))
	(loop for line in lines
		  do (let* ((con (ppcre:split "-" line))
					(start (multiple-value-bind
								 (node node-list)
							   (get-node (first con) nodes)
							 (setf nodes node-list)
							 ;; (break)
							 node))
					(end (multiple-value-bind (node node-list)
							 (get-node (second con) nodes)
						   (setf nodes node-list)
						   node)))
			   (push end (node-children start))
			   (push start (node-children end))
			   ;; (break)
			   )
		  finally (return (find-if (lambda(x)(string= "start" (node-val x)))  nodes )))))

(defun output(path)
  (print path))

(defun two-doublesp(path)
  (remove-duplicates path :test #'string=))
(defun print-path(node path)
  (if (string= "end" (node-val node))
	  ;; (output path)
	  1
	  (loop for child in (node-children node)
			when (or (string-uppercasep (node-val child))
					 (not (two-doublesp (cons (node-val child) path))))
			  sum (print-path child (cons (node-val child) path)))))

(defun string-uppercasep(s)
  (string= s (string-upcase s)))
(defun part1 (input-file)
  (let* ((start (make-graph input-file)))
    (print-path start '("start"))))
