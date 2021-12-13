(load "util.lisp")

(defparameter *tinput* (ppcre:split "\\n\\n" (uiop:read-file-string "test13.txt")))
(defparameter *tcoords* (mapcar (lambda (x) (mapcar #'parse-integer
						    (ppcre:all-matches-as-strings "[0-9]+" x)))
				(ppcre:split #\newline (car *tinput*))))
(defparameter *tfolds* (mapcar (lambda (x) (elt x 2))
			       (mapcar #'str:words
				       (ppcre:split #\newline (cadr *tinput*)))))
(defparameter *input* (ppcre:split "\\n\\n" (fetch-input 13)))
(defparameter *coords* (mapcar (lambda (x) (mapcar #'parse-integer
						    (ppcre:all-matches-as-strings "[0-9]+" x)))
				(ppcre:split #\newline (car *input*))))
(defparameter *folds* (mapcar (lambda (x) (elt x 2))
			      (mapcar #'str:words
				      (ppcre:split #\newline (cadr *input*)))))

(defun fold (axis value coords)
  (cond
    ((char= axis #\x)
     (loop for point in coords
	   for x = (car point)
	   for y = (cadr point)
	   if (> x value)
	     do (setf (car point) (- value (- x value)))))
    ((char= axis #\y)
     (loop for point in coords
	   for x = (car point)
	   for y = (cadr point)
	   if (> y value)
	     do (setf (cadr point) (- value (- y value)))))))

(defun origami (coords folds)
  (loop for f in folds
	for a = (elt f 0)
	for v = (parse-integer (car (ppcre:all-matches-as-strings "[0-9]+" f)))
	do (fold a v coords))
  (remove-duplicates coords :test #'equal))

(defun part2 (coords folds)
  (let* ((uniqs (origami coords folds))
	 (field (make-array '(6 40) :initial-element #\.)))
    (loop for point in uniqs
	  for x = (car point)
	  for y = (cadr point)
	  do (setf (aref field y x) #\#))
    (format t "~a~%" field)))
