(load "../utils.cl")
(ql:quickload :cl-ppcre)
(ql:quickload :array-operations)



(defstruct line
  start-x
  start-y
  
  end-x
  end-y)

(defstruct point x y)




(defun horizontal-linep (line)
  (= (line-start-x line)
	 (line-end-x line)))

(defun vertical-linep (line)
  (= (line-start-y line)
	 (line-end-y line)))

(defun horizontal-line-to-points (line)
  (let* ((x (line-start-x line))
		 (y1 (line-start-y line))
		 (y2 (line-end-y line))
		 (start-y (if(< y1 y2) y1 y2))
		 (end-y (if (> y2 y1) y2 y1)))
	(loop for y from start-y to end-y
		  collect (make-point :x x :y y))))

(defun vertical-line-to-points (line)
  (let* ((y (line-start-y line))
		 (x1 (line-start-x line))
		 (x2 (line-end-x line))
		 (start-x (if(< x1 x2) x1 x2))
		 (end-x (if (> x2 x1) x2 x1)))
	(loop for x from start-x to end-x
		  collect (make-point :x x :y y))))

(defun diagonal-line-to-points(line)
  (let* ((point1 (make-point :x (line-start-x line) :y (line-start-y line)))
		 (point2 (make-point :x (line-end-x line) :y (line-end-y line)))
		 (start-point (if(< (point-x point1)(point-x point2)) point1 point2 ))
		 (end-point (if(< (point-x point1)(point-x point2)) point2 point1)))
	
	(if (< (point-y start-point) (point-y end-point))
		(loop for i to (- (point-y end-point) (point-y start-point))
			  collect (make-point :x (+ (point-x start-point) i) :y (+ (point-y start-point) i)))
		
		(loop for i to (- (point-y start-point) (point-y end-point))
			  collect (make-point :x (+ (point-x start-point) i) :y (- (point-y start-point) i))))))

(defun line-to-points(line)
  (let ((points '()))
	(cond
	  ((horizontal-linep line) (setf points (append points (horizontal-line-to-points line))))
	  ((vertical-linep line) (setf points (append points (vertical-line-to-points line))))
	  (t (setf points (append points (diagonal-line-to-points line)))))
	points))



(defun part1 (file)
  (let* ((input (get-file-linewise file))
		 (lines (loop for line in input
					  collect (let* ((split ( cl-ppcre:split " -> " line))
									 (start (cl-ppcre:split "," (first split)))
									 (end (cl-ppcre:split "," (second split)))
									 (venture-line (make-line
													:start-x (parse-integer (first start))
													:start-y (parse-integer(second start))
													:end-x (parse-integer (first end))
													:end-y (parse-integer (second end)))))
								venture-line)))
		 (h+v-lines (remove-if-not #'(lambda (x) (or (horizontal-linep x)
													 (vertical-linep x)))
								   lines))
		 (points (loop for line in h+v-lines
					   append (line-to-points line)))
		 (sea-floor (make-array '(10 10) :initial-element 0)))
	(loop for point in points
		  do (incf (aref sea-floor (point-x point) (point-y point))))
	(print (array-operations:each-index (i j) (aref sea-floor j i)))
	(loop for i across (array-operations:flatten sea-floor)
		  count (< 1 i))))

(part1 "control-input.txt")

(defun part2 (file)
  (let* ((input (get-file-linewise file))
		 (lines (loop for line in input
					  collect (let* ((split ( cl-ppcre:split " -> " line))
									 (start (cl-ppcre:split "," (first split)))
									 (end (cl-ppcre:split "," (second split)))
									 (venture-line (make-line
													:start-x (parse-integer (first start))
													:start-y (parse-integer(second start))
													:end-x (parse-integer (first end))
													:end-y (parse-integer (second end)))))
								venture-line)))
		 (points (loop for line in lines
					   append (line-to-points line)))
		 (sea-floor (make-array '(1000 1000) :initial-element 0)))
	(loop for point in points
		  do (incf (aref sea-floor (point-x point) (point-y point))))
	;; (print (array-operations:each-index (i j) (aref sea-floor j i)))
	(loop for i across (array-operations:flatten sea-floor)
		  count (< 1 i))))

(part2 "input.txt")
