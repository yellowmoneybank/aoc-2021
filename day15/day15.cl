(load "../utils.cl")

(defstruct vertex
  coord
  (cost 10)
  visited)

(defun smallest(input-array)
  
  (let (smallest (0 . 0))(aops:each-index (i j) (array-dimensions input-array)
										  (if (> (aref input-array i j) (aref input-array (first smallest) (second smallest)))
											  (setf smallest '(i . j))))
	smallest))

(defun dijkstra(start end input-array)
  (let* ((distance (make-array (array-dimensions input-array) :initial-element 10))
		(precurser (make-array (array-dimensions input-array)))
		(visited (make-array (array-dimensions input-array) :initial-element nil)))
	
	(setf (aref distance (first start) (second start)) 0)
	(do ((u start (smallest distance)))
		((equal u end))   
	  (progn
		(setf (aref visited (first u) (second u)) 1)
		(loop for v in (neighbors u)
			  when (not (aref visited (first v) (second v)))
				do (distance-update u v distance precurser))))
	precurser))

(defun part1(input-file)
  (let* ((input-array (get-input-array input-file))
		(distance (dijkstra '(0 . 0) '( (1- (array-dimension input-array 0)) . (1- (array-dimension input-array 1))) input-array)))
	distance
    ))
