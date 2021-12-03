(load "~/quicklisp/setup.lisp")

(defun get-file (filename)
  (with-open-file (stream filename)
	(loop for line = (read-line stream nil)
		  while line
		  collect line)))

(defun average( lines pos)
  (loop
	for line in lines
	count (string= "0" (nth pos line)) into zeroes
	count (string= "1" (nth pos line)) into ones
	finally (if (<= zeroes ones)
				(return "1")
				(return "0"))))

(defun stringToNum(input)
  (mapcar 'parse-integer input))

(defun binToInt(input)
  (reduce (lambda (x y) (+ (* 2 x) y)) input))

(defun get-gamma()
  (let ((input (get-file "input.txt"))
		(gamma '()))
	(loop for pos below (length (car input))
		  do (loop
			   for line in input
			   count (string= "0" (char line pos)) into zeroes
			   count (string= "1" (char line pos)) into ones
			   finally (if (< zeroes ones)
						   (setf gamma (append gamma '("0")))
						   (setf gamma (append gamma '("1 "))))))
	gamma))

(defun get-epsilon(gamma)
  (let ((epsilon ()))
	(dolist (item gamma)
	  (if (string= item "0")
		  (setf epsilon (append epsilon '("1")))
		  (setf epsilon (append epsilon '("0")))))
	epsilon))





(defun filter-overAverage (input pos)
  (let ((avg (average input pos)))
	(remove-if #'(lambda (x) (string-equal avg (nth pos x))) input)))

   
(defun filter-underAverage(input pos)
  (let ((avg (average input pos)))
	(remove-if #'(lambda (x) (string-not-equal avg (nth pos x))) input)))

(defun explodeString(inputString)
  (loop for pos below (length inputString)
		collect (char inputString pos)))



(defun part1()
  (let* ((gamma (get-gamma))
		 (epsilon (get-epsilon gamma)))
	(* (binToInt (stringToNum gamma))
	   (binToInt(stringToNum epsilon)))))

(defun part2()
  (let* ((input (get-file "input.txt"))
		 (lines (mapcar 'explodeString input))
		 (oxygen lines)
		 (co2 lines))
	
	(loop for pos below (length (car lines))
		  while (< 1 (length oxygen))
		  do
			 (setf oxygen (filter-underAverage oxygen pos)))
	
	(loop for pos below (length (car lines))
		  while (< 1 (length co2))
		  do
			 (print co2)
			 (setf co2 (filter-overAverage co2 pos)))
	(let* ((oxygen (first oxygen))
		   (co2 (first co2))
		   (oxy-int (mapcar 'digit-char-p oxygen))
		   (co2-int (mapcar 'digit-char-p co2)))
		   
	(print (* (binToInt co2-int)(binToInt oxy-int))))))
