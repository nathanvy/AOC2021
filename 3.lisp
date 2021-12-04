(load "util.lisp")

(defun mangle (input)
  (parse-integer input :radix 2))

(defparameter *test-input* (uiop:read-file-lines "test3a.txt"))
(defparameter *input-a* (split-sequence:split-sequence #\newline (fetch-input 3) :remove-empty-subseqs t))

(defun part1 (input)
  (let* ((n (length (car input)))
         (p (parse-integer
             (coerce (loop for i below n collect (most-prevalent i input)) 'string)
             :radix 2)))
    (* p (- (ash 1 n) 1 p))))

(defun one-p (i) (lambda (n) (elt n i)))

(defun most-prevalent (i input)
  (if (>= (count #\1 input :key (one-p i)) (/ (length input) 2)) #\1 #\0))

(defun rating (input test)
  (loop for i from 0 until (null (cdr input))
	do (setf input (remove (most-prevalent i input) input test #'eq :key (one-p i))))
  (parse-integer (car input) :radix 2))

(defun part2 (input)
  (* (rating input :test) (rating input :test-not)))
