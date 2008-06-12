(in-package #:tempus)

(defun perform-goto (ch room-num)
  (let ((destination (real-room room-num)))
    (cond
      ((null destination)
       (send-to-char ch "No room exists with that number.~%"))
      (t
       (when (in-room-of ch)
         (char-from-room ch))
       (char-to-room ch destination)))))