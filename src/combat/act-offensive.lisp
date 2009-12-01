(in-package #:tempus)

(defcommand (ch "kill") ()
  (send-to-char ch "Kill who?~%"))

(defun perform-hit (ch target)
  nil)

(defun perform-flee (ch)
  nil)

(defcommand (ch "slay" target) (:immortal)
  (let* ((targets (resolve-alias ch target (people-of (in-room-of ch))))
         (tch (first targets)))
    (cond
      ((null targets)
       (send-to-char ch "They aren't here.~%"))
      (t
       (act ch :target tch
            :subject-emit "You chop $M to pieces!  Ah!  The blood!"
            :target-emit "$N chops you to pieces!"
            :not-target-emit "$N brutally slays $N!")
       (mudlog 'alert t "~a killed ~a with a wiz-slay at ~a"
               (name-of ch)
               (name-of tch)
               (name-of (in-room-of ch)))
       (raw-kill tch ch +type-slash+)))))