(defparameter *test-input* '#(199
			     200
			     208
			     210
			     200
			     207
			     240
			     269
			     260
			      263))

(defparameter *input-a* (mapcar #'parse-integer (uiop:read-file-lines "1a.txt")))

(defun increases (input)
  (loop for i from 1 to (1- (length input))
	with a = 0
	do (if (> (elt input i) (elt input (1- i)))
	       (incf a))
	finally (return a)))

(defun sliding-window-increases (input)
  (loop for i from 3 to (1- (length input))
	with a = 0
	for prev-sum = (+ (elt input (- i 1)) (elt input (- i 2)) (elt input (- i 3)))
	for window-sum = (+ (elt input i) (elt input (- i 1)) (elt input (- i 2)))
	do (if (> window-sum prev-sum)
	       (incf a))
	finally (return a)))
