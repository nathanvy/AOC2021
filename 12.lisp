(load "util.lisp")

(defparameter *tinput* (mapcar (lambda (x) (str:split #\- x)) (uiop:read-file-lines "test12.txt")))
(defparameter *edges* (make-hash-table :test #'equalp))
(defparameter *input* (mapcar (lambda (x) (str:split #\- x)) (str:split #\newline (fetch-input 12))))

(defun init (input)
  (loop for line in input
	for a = (car line)
	for b = (cadr line)
	do (fuckery a b)
	do (fuckery b a)))

(defun fuckery (k v)
  (cond
    ((null (gethash k *edges*)) (setf (gethash k *edges*) (list v)))
    (T (setf (gethash k *edges*) (append (gethash k *edges*) (list v))))))

(defun fgsfds (current seen &optional (duplicate nil))
  (cond
    ((string= current "end") (return-from fgsfds 1))
    ((and (not (every #'upper-case-p current))
	  (fset:contains? seen current))
     (if (null duplicate) (setf duplicate current) (return-from fgsfds 0))))
  (setf seen (fset:with seen current))
  (loop for edge in (gethash current *edges*)
	summing (fgsfds edge seen duplicate)))

(defun part1 ()
  (fgsfds "start" (fset:set)))

(defun part2 ()
  (fgsfds "start" (fset:set) nil))
