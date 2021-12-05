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
