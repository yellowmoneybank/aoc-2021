(load "../utils.cl")

(defun parse-input(input-file)
  (let* ((input-string (read-file-as-string input-file))
		 (hash-coordinates-string (first (ppcre:split "\\n\\n" input-string)))
		 (hashtag-locations (mapcar #'(lambda (x)(ppcre:split "," x))(ppcre:split "\\n" hash-coordinates-string)))
		 (lines-string  (second (ppcre:split "\\n\\n" input-string)))
		 (lines (ppcre:split "\\n" lines-string))
		 (trimmed-lines (mapcar #'(lambda (x)(string-left-trim "fold along " x)) lines))
		 (splitted-lines (mapcar #'(lambda (x)(ppcre:split "=" x)) trimmed-lines))
		 (y-max (loop for loc in hashtag-locations maximize (parse-integer (first loc))))
		 (x-max (loop for loc in hashtag-locations maximize (parse-integer (second loc))))
		 (input-array (make-array (list (1+ y-max) (1+ x-max)) :initial-element "." )))
	(loop for (y-val x-val) string in hashtag-locations
		  do (setf (aref input-array (parse-integer y-val) (parse-integer x-val)) "#"))
	(values input-array splitted-lines)))

(defun fold-array(input-array line)
  )

(defun part1 (input-file)
  (multiple-value-bind (input-array lines) (parse-input input-file)
	(loop for line in lines
		  do (setf input-array (fold-array input-array line)))))
