(load "../utils.cl")
(ql:quickload :cl-ppcre)
(ql:quickload :array-operations)
(ql:quickload :serapeum)
(ql:quickload :alexandria)

(declaim (optimize (speed 0) (space 0) (debug 3)))


(defun get-syntax-score (line)
  (let ((stack '()))
	(loop for token in line
		  do (alexandria:switch (token :test equal)
			   ("(" (push token stack))
			   ("[" (push token stack))
			   ("{" (push token stack))
			   ("<" (push token stack))
			   (")" (if(string= "(" (car stack))(pop stack) (return 3)))
			   ("]" (if(string= "[" (car stack))(pop stack) (return 57)))
			   ("}" (if(string= "{" (car stack))(pop stack) (return 1197)))
			   (">" (if(string= "<" (car stack))(pop stack) (return 25137))))
		  finally (return 0))))

(defun part1(input-file)
  (let*((lines (get-file-linewise input-file))
		(splitted-lines (mapcar #'(lambda(x) (cl-ppcre:split "" x)) lines)))
	(loop for line in splitted-lines
		  sum (get-syntax-score line))))


(defun discard-corrupt (lines)
  (remove-if #'(lambda (x)(/= 0 (get-syntax-score x))) lines))

(defun get-autocomplete (line)
  (let ((stack '()))
	(loop for token in line
		  do (alexandria:switch (token :test equal)
			   ("(" (push token stack))
			   ("[" (push token stack))
			   ("{" (push token stack))
			   ("<" (push token stack))
			   (")" (pop stack))
			   ("]" (pop stack))
			   ("}" (pop stack))
			   (">" (pop stack))))
	(nsubstitute-if ")" #'(lambda(x) (string= "(" x)) stack)
	(nsubstitute-if "]" #'(lambda(x) (string= "[" x)) stack)
	(nsubstitute-if "}" #'(lambda(x) (string= "{" x)) stack)
	(nsubstitute-if ">" #'(lambda(x) (string= "<" x)) stack)
	stack))

(defun middle (list )
  (let* ((len (length list))
		 (sorted (sort list #'<))
		 (middle (floor (/ len 2))))
	(nth middle sorted)))


(defun calc-score(list-autocomplete)
  (let ((points '()))
	(loop for completion in list-autocomplete
		  collect (reduce
				   #'(lambda (a b) (+
									(* 5 a)
									(alexandria:switch (b :test equal)
									  (")" 1)
									  ("]" 2)
									  ("}" 3)
									  (">" 4))))
				   completion
				   :initial-value 0) into scores
		  finally (return (middle scores)))))

(defun part2 (input-file)
  (let*((lines (get-file-linewise input-file))
		(splitted-lines (mapcar #'(lambda(x) (cl-ppcre:split "" x)) lines))
		(incomplete-lines (discard-corrupt splitted-lines)))
	(loop for line in incomplete-lines
		  collect (get-autocomplete line) into autocomplete
		  finally (return (calc-score autocomplete)))))
