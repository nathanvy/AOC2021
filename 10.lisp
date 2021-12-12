(load "util.lisp")

(defparameter *input* (str:split #\newline (fetch-input 10)))
(defparameter *tinput* (uiop:read-file-lines "test10.txt"))
(defun closing (opening)
  (case opening (#\( #\)) (#\[ #\]) (#\{ #\}) (#\< #\>)))

(defun parse-line (line)
  (loop for c across line
	with stack = '()
	do (case c
	     ((#\( #\[ #\{ #\<) (push (closing c) stack))
	     ((#\) #\] #\} #\>) (unless (eql c (pop stack))
				  (return (values "corrupt" c)))))
	finally (if (not (null stack)) (return (values "incomplete" stack)))))

(defun score1 (c)
  (case c
    (#\) 3)
    (#\] 57)
    (#\} 1197)
    (#\> 25137)))

(defun score-char)

(defun score2 (c)
  (reduce (lambda (score c)
            (+ (* score 5) (case c
			     (#\) 1)
			     (#\] 2)
			     (#\} 3)
			     (#\> 4))))
          c :initial-value 0))

(defun part1 (input)
  (loop for line in input
	with scorelist = '()
	for (status c) = (multiple-value-list (parse-line line))
	when (string= status "corrupt")
	  do (push (score1 c) scorelist)
	finally (return (reduce #'+ scorelist))))

(defun part2 (input)
  (let* ((sorted-scores (loop for line in input
			      with scorelist = '()
			      for (status c) = (multiple-value-list (parse-line line))
			      when (string= status "incomplete")
				do (push (score2 c) scorelist)
			      finally (return (sort scorelist #'>))))
	 (len (length sorted-scores)))
    (nth (floor (/ len 2)) sorted-scores)))
