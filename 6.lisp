(load "util.lisp")

(defparameter *testinput* (mapcar #'parse-integer (ppcre:all-matches-as-strings "[0-9]+" (uiop:read-file-string "test6.txt"))))
(defparameter *input-a* (mapcar #'parse-integer (ppcre:all-matches-as-strings "[0-9]+" (fetch-input 6))))
(defparameter *counts* (vector 0 0 0 0 0 0 0 0 0))

(defun evolve ()
  (rotatef (elt *counts* 0) (elt *counts* 1) (elt *counts* 2) (elt *counts* 3)
	   (elt *counts* 4) (elt *counts* 5) (elt *counts* 6) (elt *counts* 7) (elt *counts* 8))
  (incf (elt *counts* 6) (elt *counts* 8)))

(defun init (n)
  (incf (elt *counts* n)))

(defun part1 (input generations)
  (mapcar #'init input)
  (loop for i from 0 below generations
	do (evolve))
  (reduce #'+ *counts*))
