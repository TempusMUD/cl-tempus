(in-package #:tempus)

(defun perform-say (ch say-cmd message)
  (let ((message (act-escape message)))
    (act ch
         :all-emit (format nil "&B$n ~a$%$a, &c'$[~a]'" say-cmd message))))

(defun perform-emote (ch message)
  (let ((message (act-escape message)))
    (act ch
         :subject-emit (format nil "~a ~a" (name-of ch) message)
         :place-emit (format nil "$n ~a" message))))