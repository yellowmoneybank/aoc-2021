(load "../utils.cl")
(ql:quickload :cl-ppcre)
(ql:quickload :array-operations)
(ql:quickload :serapeum)
(ql:quickload :alexandria)

(declaim (optimize (speed 0) (space 0) (debug 3)))


(defun get-input-array(input-file)
  (let* ((lines (get-file-linewise input-file))
		 (number-strings (mapcar #'(lambda (x)(ppcre:split "" x)) lines))
		 (numbers (loop for line in number-strings
						collect (mapcar #'parse-integer line)))
		 (columns (length numbers))
		 (rows (length (car numbers)))
		 (input-array (make-array (list columns rows) :initial-contents numbers)))
	input-array))

(defstruct neighbor
  i
  j
  val)
(defun get-neighbors (i j input-array)
  (let* ((up-neighbor (if (= i 0) nil (make-neighbor :i (1- i) :j j :val (aref input-array (1- i ) j)) ))
		 (down-neighbor (if (= (1+ i) (array-dimension input-array 0)) nil (make-neighbor :i (1+ i) :j j :val (aref input-array (1+ i ) j))))
		 (left-neighbor (if (= j 0) nil (make-neighbor :i i :j (1- j) :val (aref input-array i (1- j)))))
		 (right-neigbor (if (= (1+ j) (array-dimension input-array 1)) nil (make-neighbor :i i :j (1+ j) :val (aref input-array i (1+ j)))))
		 (neighbors (remove nil (list up-neighbor down-neighbor left-neighbor right-neigbor))))
	neighbors))

(defun minimump (i j input-array)
  (let ((neighbors (get-neighbors i j input-array)))
	(every #'(lambda (x) (< (aref input-array i j) (neighbor-val x))) neighbors)))

(defun get-minima (input-array)
  (let ((minima '()))
	(array-operations/utilities:nested-loop (i j) (array-dimensions input-array)
	  (when(minimump i j input-array ) (push (make-neighbor :i i :j j :val (aref input-array i j)) minima )))
	minima))

(defun part1(input)
  (let* ((input-array (get-input-array input))
		 (minima (get-minima input-array)))
				 (reduce #'+ (mapcar #'(lambda (x) (1+ (neighbor-val x))) minima))))

(defun calc-basin (place input-array)
  (let* ((neighbors (get-neighbors (neighbor-i place) (neighbor-j place) input-array)))
	(loop for n in neighbors
		  when (and (< (neighbor-val place)(neighbor-val n))
					(> 9 (neighbor-val n)))
			collect (calc-basin n input-array) into basins
		  finally (return  (append basins place)))))

(defun neighbor-duplictates(a b)
	(and
	 (= (neighbor-j a) (neighbor-j b))
	 (= (neighbor-i a) (neighbor-i b))))

(defun part2(input)
  (let* ((input-array (get-input-array input))
		 (minima (get-minima input-array))
		 (bazinswdups (mapcar #'alexandria:flatten (mapcar #'(lambda(x)(calc-basin x input-array)) minima)))
		 (bazins (mapcar #'(lambda (x)(remove-duplicates x :test #'neighbor-duplictates)) bazinswdups))
		 (bazins-sizes (mapcar #'length bazins))
		 (sorted-bazins-sizes (sort bazins-sizes #'>)))
	(* (first sorted-bazins-sizes) (second sorted-bazins-sizes)(third sorted-bazins-sizes))))
