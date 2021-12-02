(load "util.lisp")

(defun mangle (input)
  (uiop:split-string input :separator " "))

(defparameter *test-input* (mapcar #'mangle (uiop:read-file-lines "test2a.txt")))
(defparameter *input-a* (mapcar #'mangle (split-sequence:split-sequence #\newline (fetch-input 2) :remove-empty-subseqs t)))

(defun part1 (input)
  (let* ((pos 0)
	 (depth 0))
    (loop for sublist in input
	  for x = (parse-integer (cadr sublist))
	  do (cond ((string= (car sublist) "forward") (incf pos x))
		   ((string= (car sublist) "down") (incf depth x))
		   ((string= (car sublist) "up") (decf depth x))))
    (format t "~a ~a ~a ~%" depth pos (* depth pos))))


(defun part2 (input)
  (let* ((pos 0)
	 (depth 0)
	 (aim 0))
    (loop for sublist in input
	  for x = (parse-integer (cadr sublist))
	  do (cond ((string= (car sublist) "forward") (progn
							(incf pos x)
							(incf depth (* x aim))))
		   ((string= (car sublist) "down") (incf aim x))
		   ((string= (car sublist) "up") (decf aim x))))
    (format t "~a ~a ~a ~%" depth pos (* depth pos))))
