(load "util.lisp")

(defparameter *input* (map 'vector #'parse-integer (ppcre:all-matches-as-strings "[0-9]" (fetch-input 9))))
(defparameter *tinput* (map 'vector #'parse-integer (ppcre:all-matches-as-strings "[0-9]" (uiop:read-file-string "test9.txt"))))
(defparameter *field* (make-array '(100 100) :initial-element 0))

(defun init (input)
  (loop for i from 0 below (array-total-size *field*)
	do (setf (row-major-aref *field* i) (elt input i))))

;; is the adjacent point p2 higher than the point under consideration p1?
(defun higher-p (x1 y1 x2 y2)
  (let* ((p1 (aref *field* x1 y1))
	 (p2 (aref *field* x2 y2)))
    (> p2 p1)))

;; is this current point a low point?
;; if i = 0, we're on the top row.  if i == imax we're on the bottom
;; if j = 0 we're on the left side.  if j == jmax we're on the right side
;; if i == j == 0 we're at the origin, etc.
(defun lowpoint-p (i j)
  (let* ((imax (1- (car (array-dimensions *field*))))
	 (jmax (1- (cadr (array-dimensions *field*)))))
    (cond
      ((and (zerop i) (zerop j)) (and (higher-p i j (1+ i) j)
				      (higher-p i j i (1+ j))))
      ((and (= i imax) (= j jmax)) (and (higher-p i j i (1- j))
					(higher-p i j (1- i) j)))
      ((and (= i imax) (zerop j)) (and (higher-p i j i (1+ j))
				       (higher-p i j (1- i) j)))
      ((and (= j jmax) (zerop i)) (and (higher-p i j (1+ i) j)
				       (higher-p i j i (1- j))))
      ((zerop i) (and (higher-p i j (1+ i) j)
		      (higher-p i j i (1- j))
		      (higher-p i j i (1+ j))))
      ((zerop j) (and (higher-p i j (1- i) j)
		      (higher-p i j (1+ i) j)
		      (higher-p i j i (1+ j))))
      ((= i imax) (and (higher-p i j (1- i) j)
		       (higher-p i j i (1+ j))
		       (higher-p i j i (1- j))))
      ((= j jmax) (and (higher-p i j (1- i) j)
		       (higher-p i j (1+ i) j)
		       (higher-p i j i (1- j))))
      (t (and (higher-p i j (1+ i) j)
	      (higher-p i j (1- i) j)
	      (higher-p i j i (1+ j))
	      (higher-p i j i (1- j)))))))

(defun crawl (i j)
  (let* ((imax (1- (car (array-dimensions *field*))))
	 (jmax (1- (cadr (array-dimensions *field*))))
	 (c (complex i j)))
    (if (= (aref *field* i j) 9) (return-from crawl))
    (if (null (position c *seen*)) (push c *seen*))
    (cond
      ((and (zerop i) (zerop j)) (progn
				   (if (higher-p i j (1+ i) j) (crawl (1+ i) j))
				   (if (higher-p i j i (1+ j)) (crawl i (1+ j)))))
      ((and (= i imax) (= j jmax)) (progn
				     (if (higher-p i j i (1- j)) (crawl i (1- j)))
				     (if (higher-p i j (1- i) j) (crawl (1- i) j))))
      ((and (= i imax) (zerop j)) (progn
				    (if (higher-p i j i (1+ j)) (crawl i (1+ j)))
				    (if (higher-p i j (1- i) j) (crawl (1- i) j))))
      ((and (= j jmax) (zerop i)) (progn
				    (if (higher-p i j (1+ i) j) (crawl (1+ i) j))
				    (if (higher-p i j i (1- j)) (crawl i (1- j)))))
      ((zerop i) (progn
		   (if (higher-p i j (1+ i) j) (crawl (1+ i) j))
		   (if (higher-p i j i (1- j)) (crawl i (1- j)))
		   (if (higher-p i j i (1+ j)) (crawl i (1+ j)))))
      ((zerop j) (progn
		   (if (higher-p i j (1- i) j) (crawl (1- i) j))
		   (if (higher-p i j (1+ i) j) (crawl (1+ i) j))
		   (if (higher-p i j i (1+ j)) (crawl i (1+ j)))))
      ((= i imax) (progn
		    (if (higher-p i j (1- i) j) (crawl (1- i) j))
		    (if (higher-p i j i (1+ j)) (crawl i (1+ j)))
		    (if (higher-p i j i (1- j)) (crawl i (1- j)))))
      ((= j jmax) (progn
		    (if (higher-p i j (1- i) j) (crawl (1- i) j))
		    (if (higher-p i j (1+ i) j) (crawl (1+ i) j))
		    (if (higher-p i j i (1- j)) (crawl i (1- j)))))
      (t (progn
	   (if (higher-p i j (1+ i) j) (crawl (1+ i) j))
	   (if (higher-p i j (1- i) j) (crawl (1- i) j))
	   (if (higher-p i j i (1+ j)) (crawl i (1+ j)))
	   (if (higher-p i j i (1- j)) (crawl i (1- j))))))))

(defun part1 ()
  (let* ((imax (1- (car (array-dimensions *field*))))
	 (jmax (1- (cadr (array-dimensions *field*)))))
    (reduce #'+ (loop with risks = '()
		      for i from 0 to imax
		      do (loop for j to jmax
			       do (if (lowpoint-p i j) (push (+ 1 (aref *field* i j)) risks)))
		      finally (return risks)))))

(defparameter *seen* '())
(defun part2 ()
  (let* ((imax (1- (car (array-dimensions *field*))))
	 (jmax (1- (cadr (array-dimensions *field*))))
	 (basins '()))
    (loop for i to imax
	  do (loop for j to jmax
		   do (if (lowpoint-p i j) (progn
					     (setf *seen* '())
					     (crawl i j)
					     (format t "~a~%" *seen*)
					     (push (length *seen*) basins)))))
    (reduce #'* (subseq (sort basins #'>) 0 3))))
