(defparameter *test-input* '(("forward" "5")
			     ("down" "5")
			     ("forward" "8")
			     ("up" "3")
			     ("down" "8")
			     ("forward" "2")))

(defparameter *input-a* (mapcar #'mangle (uiop:read-file-lines "2a.txt")))

(defun mangle (input)
  (uiop:split-string input :separator " "))

(defun part1 ()
  (let* ((pos 0)
	 (depth 0))
    (loop for sublist in *input-a*
	  for x = (parse-integer (cadr sublist))
	  do (cond ((string= (car sublist) "forward") (incf pos x))
		   ((string= (car sublist) "down") (incf depth x))
		   ((string= (car sublist) "up") (decf depth x))))
    (format t "~a ~a ~a ~%" depth pos (* depth pos))))


(defun part2 ()
  (let* ((pos 0)
	 (depth 0)
	 (aim 0))
    (loop for sublist in *input-a*
	  for x = (parse-integer (cadr sublist))
	  do (cond ((string= (car sublist) "forward") (progn
							(incf pos x)
							(incf depth (* x aim))))
		   ((string= (car sublist) "down") (incf aim x))
		   ((string= (car sublist) "up") (decf aim x))))
    (format t "~a ~a ~a ~%" depth pos (* depth pos))))
