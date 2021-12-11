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
						collect (mapcar #'(lambda (x) (make-octopus :val (parse-integer x))) line)))
				 (columns (length numbers))
		 (rows (length (car numbers)))
		 (input-array (make-array (list columns rows) :initial-contents numbers)))
	(aops:each-index (i j)(array-dimensions input-array)
	  (setf (octopus-i (aref input-array i j )) i
			(octopus-j (aref input-array i j )) j))
	input-array))

(defstruct octopus
  i
  j
  val
  (flashed nil))

(defun get-neighbors (i j input-array)
  (let* ((i-max (array-dimension input-array 0))
		 (j-max (array-dimension input-array 1))
		 (up-neighbor (if (= i 0) nil (aref input-array (1- i) j)))
		 (up-left-neighbor (if (or (= i 0) (= j 0)) nil (aref input-array (1- i) (1- j))))
		 (up-right-neighbor (if (or (= i 0) (= (1+ j) j-max)) nil (aref input-array (1- i) (1+ j))))
		 
		 (down-neighbor (if (= (1+ i) i-max) nil (aref input-array (1+ i) j)))
		 (down-left-neighbor (if (or (= (1+ i) i-max) (= j 0)) nil (aref input-array (1+ i) (1- j))))
		 (down-right-neighbor (if (or (= (1+ i) i-max) (= (1+ j) j-max)) nil (aref input-array (1+ i) (1+ j))))

		 (left-neighbor (if (= j 0) nil (aref input-array i (1- j))))
		 (right-neighbor (if (= (1+ j) j-max) nil (aref input-array i (1+ j))))
		 
		 
		 (neighbors (remove nil (list up-neighbor up-left-neighbor up-right-neighbor down-neighbor down-left-neighbor down-right-neighbor left-neighbor right-neighbor))))
	neighbors))

(defun flash (octopus input-array)
  (let ((neighbors (get-neighbors (octopus-i octopus) (octopus-j octopus) input-array)))
	(setf (octopus-flashed octopus) t)
	(loop for n in neighbors
		  do (incf (octopus-val n))
		  when (and (not (octopus-flashed n)) (< 9 (octopus-val n)))
			sum (flash n input-array) into neigh-flashes
		  finally (return (1+ neigh-flashes)))))

(defun print-octo (input-array)
  (aops:each-index (i j ) (array-dimensions input-array)
	(progn (format t "~a" (octopus-val (aref input-array i j)))
		   (when (= (1+ j)(array-dimension input-array 1))
			 (format t "~%"))
		   (when (and (= (1+ j)(array-dimension input-array 1)) (= (1+ i)(array-dimension input-array 0)))
			 (format t "~%")))))

(defun array-sum(input-array)
  (let ((sum 0))
	(aops:each-index (i j) (array-dimensions input-array)
	  (incf sum (octopus-val (aref input-array i j))))
	sum))
(defun part1(input-file)
  (let* ((input-array (get-input-array input-file))
		 (flashes 0))
	(loop for step below 10000
		  ;; do (print-octo input-array)
		  do (aops:each-index (i j) (array-dimensions input-array)
			   (incf (octopus-val (aref input-array i j))))
		  do (aops:each-index (i j) (array-dimensions input-array)
			   (let* ((octopus (aref input-array i j)))
				 (when (and (not (octopus-flashed octopus)) (< 9 (octopus-val octopus)))
				   (incf flashes  (flash octopus input-array)))))
		  do (aops:each-index (i j) (array-dimensions input-array)
			   (when (octopus-flashed (aref input-array i j))
				   (setf (octopus-flashed (aref input-array i j)) nil
						 (octopus-val (aref input-array i j)) 0)))
		  when (= 0 (array-sum input-array))
			do (print (1+ step)))
	flashes))
