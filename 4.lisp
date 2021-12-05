(load "util.lisp")

(defun mangle (input)
  (parse-integer input :radix 2))

(defparameter *test-input* (mapcar #'parse-integer (split-sequence:split-sequence #\, (uiop:read-file-string "test4a.txt"))))
(defparameter *test-boards* (uiop:read-file-string "testboards.txt"))
(defparameter *input-boards* (uiop:read-file-string "4.txt"))
(defparameter *input* (mapcar #'parse-integer (split-sequence:split-sequence #\, (uiop:read-file-string "4-calls.txt"))))

(defun parse-board (stream)
  (handler-case
      (make-array '(5 5)
                  :initial-contents
                  (loop repeat 5 collect (loop repeat 5 collect (read stream))))
    (end-of-file () nil)))

(defun parse-input (file)
  (with-open-file (input file)
    (values
     (mapcar #'parse-integer (uiop:split-string (read-line input) :separator ","))
     (loop for board = (parse-board input) while board collect board))))

(defun score-uncalled (board)
  (loop for i below 25 for x = (row-major-aref board i) when x sum x))

(defun bingo-scores (numbers boards)
  (uiop:while-collecting (winner)
    (loop for n in numbers
	  do (loop for b in boards
		   do (loop for i below 5
			    do (loop for j below 5
				     do (when (eql (aref b i j) n)
					  (setf (aref b i j) nil)
					  (when (or (loop for k below 5 never (aref b k j))
						    (loop for k below 5 never (aref b i k)))
					    (winner (* n (score-uncalled b)))
					    (setf boards (remove b boards))))))))))

(defun main (file)
  (multiple-value-bind (numbers boards) (parse-input file)
    (let ((scores (bingo-scores numbers boards)))
      (values (car scores) (car (last scores))))))


(defun wins-p (board called)
  (loop for i from 0 below 5
	for cols = (loop for j below 5
			 collect (aref board i j))
	for rows = (loop for j below 5
			 collect (aref board j i))
	do (if (= 5 (length (intersection cols called))) (return-from wins-p t))
	do (if (= 5 (length (intersection rows called))) (return-from wins-p t))
	finally (return nil)))
