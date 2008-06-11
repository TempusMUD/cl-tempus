(in-package #:tempus)

(defun char-to-room (ch room)
  (setf (in-room-of ch) room)
  (push ch (people-of room)))

(defun char-from-room (ch)
  (setf (people-of (in-room-of ch)) (delete ch (people-of (in-room-of ch))))
  (setf (in-room-of ch) nil))