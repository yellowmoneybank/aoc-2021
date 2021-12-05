(load "../utils.cl")
(ql:quickload :cl-ppcre)
(defstruct board-number
  value
  markedp)

(setf input (cl-ppcre:split "\\n\\n" (read-file-as-string "control-input.txt")))

(defun string-to-boardnumber(line)
  (let* ((trim-line (string-trim " " line))
		 (numbers (cl-ppcre:split "\\s." trim-line)))
	(loop for number in numbers
		  collect (make-board-number :value number :markedp nil))))

(defun mark-numbers(boards number)
  (loop for board in boards
		do (loop for line in board
				 do (loop for entry in line
						  do (if (string-equal number (board-number-value entry))
								 (setf (board-number-markedp entry) 1))))))
(defun winnerp (boards)
  (loop for board in boards
	   when (board-winningp board)
	   return board))

(defun winner-rowp (board)
  (loop for line in board
		when (line-is-winning line)
		  return board))
(defun board-winningp(board)
  (or (winner-columnp board)
	  (winner-rowp board)))

(defun get-columns (board)
  (apply #'mapcar #'list board))

(defun line-is-winning (line)
  (if (every #'(lambda (x) (= 1 (board-number-markedp x))) line)
	  line))
(defun winner-columnp (board)
  (let ((columns (get-columns board)))
	(loop for column in columns
		  return (line-is-winning column))))

(setf numbers (cl-ppcre:split "," (car input)))
(setf board-strings (rest input))
(setf boards (loop for board-string in board-strings
			collect (loop for line in (cl-ppcre:split "\\n" board-string)
						  collect (string-to-boardnumber line))))
(loop for number in numbers
	  do (mark-numbers boards number)
	  until (winnerp boards) do (print number))


