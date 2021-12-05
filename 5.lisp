(load "util.lisp")

(defparameter *test-input* (mapcar #'mangle (uiop:read-file-lines "test5a.txt")))
(defparameter *input* (mapcar #'mangle (uiop:read-file-lines "5.txt")))
(defparameter *field* (make-array '(1000 1000) :initial-element 0))

(defun mangle (input)
  (map 'vector #'parse-integer (ppcre:all-matches-as-strings "[0-9]+" input)))

(defun consider (input)
  (loop for line in input
	for x1 = (elt line 0)
	for x2 = (elt line 2)
	for y1 = (elt line 1)
	for y2 = (elt line 3)
	if (or (= x1 x2)
	       (= y1 y2)
	       (= 1 (/ (- y2 y1) (- x2 x1)))
	       (= -1 (/ (- y2 y1) (- x2 x1))))
	  collect line))

(defun draw (field lines)
  (loop for line in lines
	for x1 = (elt line 0)
	for x2 = (elt line 2)
	for y1 = (elt line 1)
	for y2 = (elt line 3)
	do (cond
	     ((= x1 x2)
	      (let* ((a (min y1 y2))
		     (b (max y1 y2)))
		(loop for i from a to b
		      do (incf (aref field i x1)))))
	     ((= y1 y2)
	      (let* ((a (min x1 x2))
		     (b (max x1 x2)))
		(loop for i from a to b
		      do (incf (aref field y1 i)))))
	     ((= 1 (/ (- y2 y1) (- x2 x1)))
	      (progn
		(if (> x1 x2)
		    (loop for i from x1 downto x2
			  for j from y1 downto y2
			  do (incf (aref field j i)))
		    (loop for i from x1 to x2
			  for j from y1 to y2
			  do (incf (aref field j i))))))
	     ((= -1 (/ (- y2 y1) (- x2 x1)))
	      (progn
		(if (> x1 x2)
		    (loop for i from x1 downto x2
			  for j from y1 to y2
			  do (incf (aref field j i)))
		    (loop for i from x1 to x2
			  for j from y1 downto y2
			  do (incf (aref field j i))))))))
  (loop for i from 0 below (array-total-size field)
	with count = 0
	if (< 1 (row-major-aref field i))
	  do (incf count)
	finally (return count)))

(defun part1 (input)
  (let* ((lines (consider input)))
    (draw *field* lines)))
