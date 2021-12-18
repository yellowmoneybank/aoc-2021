(load "../utils.cl")

(defun parse-rewrite(lines)
  (let ((hash-table (make-hash-table :test 'equal)))

	(dolist (line lines)
	  (let ((rule (ppcre:split " -> " line)))
		(setf (gethash (first rule) hash-table) (second rule))))
	
	hash-table))


(defun common(line)
  (let ((hash (make-hash-table :test 'equal)))
	(loop for c across line
		  do (incf (gethash c hash 0)))
	
	(loop for value being the hash-values of hash
		  maximize value into max
		  minimize value into min
		  finally (return (- max min)))))

(defun rewrite(dict line)
  (let ((output ""))
	(loop for i below (1- (length line))
		  do (let* ((two-chars (subseq line i (+ 2 i)))
				   (filler (gethash two-chars dict)))

			   (if filler
				   (setf output (concatenate 'string output (list (char two-chars 0)) filler (list)))
				   (setf output (concatenate 'string output (list (first two-chars))))))
		  finally (return (concatenate 'string output  (first (reverse (ppcre:split "" line)))) ))))

(defun part1 (input-file)
  (let* ((start-line (first (get-file-linewise input-file)))
		(rewrite-dict (parse-rewrite (cddr (get-file-linewise input-file))))
		(end-line start-line))
	(loop for i below 40
		  do (progn (print i)
					(setf end-line (rewrite rewrite-dict end-line))))
    (common end-line)))
