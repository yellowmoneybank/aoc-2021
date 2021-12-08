(load "../utils.cl")
(ql:quickload :cl-ppcre)
(ql:quickload :array-operations)

(defun part1(input-file)
  (let* ((input (cl-ppcre:split "," (read-file-as-string input-file)))
		 (input-numbers (mapcar #'parse-integer input))
		 (sorted-input (sort input-numbers #'<))
		 (median (nth (/ (length sorted-input) 2) sorted-input))
		 (fuel (reduce #'+ (mapcar #'(lambda(x)(abs (- x median))) sorted-input))))
	fuel))

(defun fuel (x)
  (loop for i from 1 to x
		sum i))

(defun get-fuel (input-numbers)
  (loop for i from (reduce #'min input-numbers) to (reduce #'max input-numbers)
		minimize (reduce #'+ (mapcar #'(lambda (x)(fuel (abs (- x i)))) input-numbers))))
(defun part2(input-file)
  (let* ((input (cl-ppcre:split "," (read-file-as-string input-file)))
		 (input-numbers (mapcar #'parse-integer input)))
	(get-fuel input-numbers)))

