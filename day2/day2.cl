(load "~/quicklisp/setup.lisp")
(ql:quickload "cl-ppcre")

(defun get-file (filename)
  (with-open-file (stream filename)
	(loop for line = (read-line stream nil)
		  while line
		  collect line)))

(defun part1 ()
  (loop for line in (get-file "input.txt")
	  for instruction = (car (ppcre:split "\\s" line))
	  for value = (apply 'parse-integer (cdr (ppcre:split "\\s" line)))
	  when (string= instruction "forward")
		sum value into forward
	  when (string= instruction "up")
		sum value into up
	  when (string= instruction "down")
		sum value into down
	  finally (print (* forward (abs (- up down))))))

(defun part2()
  (let ((horizontal 0)
		(depth 0)
		(aim 0))
	(loop for line in (get-file "input.txt")
		  for instruction = (car (ppcre:split "\\s" line))
		  for value = (apply 'parse-integer (cdr (ppcre:split "\\s" line)))
		  do
			 (cond
			   ((string= "down" instruction) (incf aim value))
			   ((string= "up" instruction) (decf aim value))
			   ((string= "forward" instruction) (progn (incf horizontal value)
													   (incf depth (* aim value))))))
	(print (* horizontal depth))))
