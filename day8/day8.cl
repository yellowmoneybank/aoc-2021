(load "../utils.cl")
(ql:quickload :cl-ppcre)
(ql:quickload :array-operations)
(ql:quickload :serapeum)
(ql:quickload :alexandria)

(defun part1(input-file)
  (let* ((input (get-file-linewise input-file))
		 (input-split (mapcar #'(lambda(x) (cl-ppcre:split " | " x)) input))
		 (output-values (mapcar #'(lambda(x) (last x 4)) input-split)))
    (loop for i in (alexandria:flatten output-values)
		  count (cond ((= 2 (length i)) t)
					  ((= 4 (length i)) t)
					  ((= 3 (length i)) t)
					  ((= 7 (length i)) t)))))

(defun get-n0 (input bd)
  (let* ((len6 (remove-if-not #'(lambda (x)(= 6 (length x))) input)))
	(loop for set in len6
		  do (
			  if ( = 7 (length (union bd set :test #'string=)))
				(return set)))))

(defun remove-from-set (input r)
  (loop for i in input
		if (not (alexandria:set-equal i r :test #'string=)) collect i))

(defun rem-dup(sets)
  (remove-duplicates sets :test #'(lambda (a b)(alexandria:set-equal a b :test #'string=))))

(defun get-n9 (character-eg n9n6)
  (loop for set in n9n6
		if (= 7 (length (union set character-eg :test #'string=)))
		  return set))

(defun solve-riddle (input-list)
  (let* ((sets (mapcar #'(lambda (x)(ppcre:split "" x)) input-list))
		 (n1 (find-if #'(lambda (x)(= 2 (length x))) sets))
		 (n4 (find-if #'(lambda (x)(= 4 (length x))) sets))
		 (n7 (find-if #'(lambda (x)(= 3 (length x))) sets))
		 (n8 (find-if #'(lambda (x)(= 7 (length x))) sets))
		 (character-a (set-difference n7 n1 :test #'string=))
		 (character-eg (set-difference n8 (union n4 n7) :test #'string=))
		 (character-bd (set-difference n4 n7 :test #'string=))
		 (n0 (get-n0 sets character-bd))
		 (character-d  (set-difference n8 n0 :test #'string=))
		 (character-b  (set-difference character-bd character-d :test #'string=))
		 (length6 (rem-dup (remove-if-not #'(lambda (x)(= 6 (length x))) sets)))
		 (n9n6 (remove-from-set length6 n0))
		 (n9 (get-n9 character-eg n9n6))
		 (n6 (alexandria:flatten (remove-from-set n9n6 n9)))
		 (character-c (set-difference n8 n6 :test #'string=))
		 (character-f (set-difference n1 character-c :test #'string=))
		 (character-e (set-difference n8 n9 :test #'string=))
		 (character-g (set-difference character-eg character-e :test #'string=))
		 (n2 (append character-a character-c character-d character-e character-g))
		 (n3 (append character-a character-c character-d character-f character-g))
		 (n5 (append character-a character-b character-d character-f character-g)))
	(loop for set in (last sets 4)
		  when (alexandria:set-equal set n0 :test #'string=) collect "0" into final-number
			when (alexandria:set-equal set n1 :test #'string=) collect "1" into final-number
			when (alexandria:set-equal set n2 :test #'string=) collect "2" into final-number
			when (alexandria:set-equal set n3 :test #'string=) collect "3" into final-number
			when (alexandria:set-equal set n4 :test #'string=) collect "4" into final-number
			when (alexandria:set-equal set n5 :test #'string=) collect "5" into final-number
			when (alexandria:set-equal set n6 :test #'string=) collect "6" into final-number
			when (alexandria:set-equal set n7 :test #'string=) collect "7" into final-number
			when (alexandria:set-equal set n8 :test #'string=) collect "8" into final-number
			when (alexandria:set-equal set n9 :test #'string=) collect "9" into final-number
			  finally (return (parse-integer(format nil "~{~a~}" final-number))))))






(defun part2 (input-file)
  (let* ((input (get-file-linewise input-file))
		 (input-split-raw (mapcar #'(lambda(x) (cl-ppcre:split " " x)) input))
		 (input-split (mapcar #'(lambda(x)(remove-if #'(lambda(y)(string= "|" y)) x)) input-split-raw)))
	(loop for line in input-split
		  sum (solve-riddle line))))

;; 0 = 6 + abc efg          ----  
;; 1 = 2 -   c  f
;; 2 = 5 + a cde g           ---- 
;; 3 = 5 + a cd fg
;; 4 = 4 -  bcd f            ---- 
;; 5 = 5 + ab d fg           ---- 
;; 6 = 6 + ab defg           ---- 
;; 7 = 3 - a c  f            ---- 
;; 8 = 7 - abcdefg           ---- 
;; 9 = 6 + abcd fg
;; ----------------------------------------------
;; 1.  a = 7 - 1                                                                    ## 1. 1,4,7,8 sind bekannt
;; 2.  eg = 8 - (4 + 7)											                 ## 2. 0,9,6        
;; 3.  bd = 4 - 7													                 ##             
;; 4.  nummer 0 ist, wenn len(3. + len(für alle 6)) == 7 			                 ##             
;; 5.  d = 8 - 0 													                 ##             
;; 6.  b = (3.) - d												                 ##             
;; 7.  nummer 9 ist, (2.) + (für alle 6) == len 7					                 ##             
;; 8.  e = 8 - 9													                 ##             
;; 9.  g = (2.) - e												                 ##             
;; 10. nummer 6 ist letzter nicht identifizierter len(6)			                              
;; 11. c = 8 - 6													                              
;; 12. f = 1 - c
