(defun get-file-linewise (filename)
  (with-open-file (stream filename)
	(loop for line = (read-line stream nil)
		  while line
		  collect line)))

(defun read-file-as-string (infile)
  (with-open-file (instream infile :direction :input :if-does-not-exist nil)
    (when instream 
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

(ql:quickload :cl-ppcre)
(ql:quickload :array-operations)
(ql:quickload :serapeum)
(ql:quickload :alexandria)

(declaim (optimize (speed 0) (space 0) (debug 3)))

(defun get-input-array(input-file)
  (let* ((lines (get-file-linewise input-file))
		 (number-strings (mapcar #'(lambda (x)(ppcre:split "" x)) lines))
		 (numbers (loop for line in number-strings
						collect (mapcar #'parse-integer line)))
				 (columns (length numbers))
		 (rows (length (car numbers)))
		 (input-array (make-array (list columns rows) :initial-contents numbers)))
	input-array))
