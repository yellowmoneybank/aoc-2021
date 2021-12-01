(defun get-file (filename)
  (with-open-file (stream filename)
	(loop for line = (read-line stream nil)
		  while line
		  collect (parse-integer line))))

(loop for sublist on (get-file "input.txt")
	  while (> (length sublist) 2)
	  count (< (first sublist) (second sublist)))

(loop for sublist on (get-file "input.txt")
	  while (> (length sublist) 3)
	  for window1 = (subseq sublist 0 3)
      for window2 = (subseq sublist 1 4)
	  count (< (apply '+ window1) (apply '+ window2)))
