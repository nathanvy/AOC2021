(load "util.lisp")

(defparameter *testinput* (mapcar #'parse-integer (ppcre:all-matches-as-strings "[0-9]+" (uiop:read-file-string "test7.txt"))))
(defparameter *input-a* (mapcar #'parse-integer (ppcre:all-matches-as-strings "[0-9]+" (fetch-input 7))))

(defun crunch (input n)
  (reduce #'+ (mapcar #'(lambda (x)  (abs (- x n))) input)))

(defun crunch-part2 (input n)
  (reduce #'+ (mapcar #'(lambda (x)  (/ (* (abs (- x n)) (1+ (abs (- x n)))) 2)) input)))

(defun part1 (input)
  (let ((far-crab (loop for crab in input
		       maximize crab)))
    (loop for i from 0 to far-crab
	  minimize (crunch input i))))

(defun part2 (input)
  (let ((far-crab (loop for crab in input
			maximize crab)))
    (loop for i from 0 to far-crab
	  minimize (crunch-part2 input i))))
