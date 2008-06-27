(in-package #:tempus)

(defun perform-say (ch say-cmd message)
  (let ((message (act-escape message)))
    (act ch
         :all-emit (format nil "&B$n ~a$%$a, &c'$[~a]'&n" say-cmd message))))

(defun perform-say-to (ch target message)
  (let ((message (act-escape message)))
    (act ch :target target
         :all-emit (format nil "&B$n$a say$% to $N, &c'$[~a]'&n" message))))

(defun perform-emote (ch message)
  (let ((message (act-escape message)))
    (act ch
         :subject-emit (format nil "~a ~a" (name-of ch) message)
         :place-emit (format nil "$n ~a" message))))

(defcommand (ch "say" message) (:resting)
  (perform-say ch "say" message))

(defcommand (ch #\' message) (:resting)
  (perform-say ch "say" message))

(defcommand (ch "say" "to" target message) (:resting)
  (let ((vict (resolve-alias ch target)))
    (if vict
        (perform-say-to ch vict message)
        (send-to-char ch "There's no '~a' here.~%" target))))

(defcommand (ch #\> target message) (:resting)
  (let ((vict (resolve-alias ch target)))
    (if vict
        (perform-say-to ch vict message)
        (send-to-char ch "There's no '~a' here.~%" target))))

(defcommand (ch "emote" message) (:resting)
  (perform-emote ch message))

(defcommand (ch #\: message) (:resting)
  (perform-emote ch message))
