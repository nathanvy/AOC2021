(ql:quickload "split-sequence")
(ql:quickload "dexador")

(defun fetch-input (challenge)
  (let* ((session-data (uiop:read-file-string "session.cookie")))
    (dex:get
     (format nil "https://adventofcode.com/2021/day/~d/input" challenge)
     :headers `(("cookie" . ,(format nil "session=~a" session-data))))))
