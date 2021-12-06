(load "../utils.cl")
(ql:quickload :cl-ppcre)
(ql:quickload :array-operations)



(defun part1 (input-file)
  (let* ((input (read-file-as-string input-file))
		 (fish-list (loop for timer in (cl-ppcre:split "," input)
						  with fish-list = (make-array 0 :adjustable t)
						  do (vector-push-extend (parse-integer timer) fish-list)
						  finally (return fish-list))))

    (loop for day from 1 to 256
		  do (loop for i below (length fish-list)
				   do (if (= 0 (aref fish-list i))
						  (progn
							(vector-push-extend 8 fish-list)
							(setf (aref fish-list i) 6))
						  (decf (aref fish-list i)))))
	(length fish-list)))

(part1 "input.txt")


