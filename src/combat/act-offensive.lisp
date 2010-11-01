(in-package #:tempus)

(defun perform-hit (ch target)
  (cond
    ((null target)
     (send-to-char ch "They don't seem to be here.~%")
     (wait-state ch 4))
    ((eql ch tch)
     (act ch
          :subject-emit "You hit yourself...  OUCH!~%"
          :place-emit "$n hits $mself.  That's gotta hurt!~%"))
    ((and (aff-flagged ch +aff-charm+)
          (eql (master-of ch) tch))
     (act ch :target tch
          :subject-emit "$N is such a good friend, you simply can't hit $M."))
    ((not (can-attack ch tch))
     nil)
    ((find tch (fighting-of ch))
     (act ch :target tch :subject-emit  "You concentrate your attacks on $N!")
     (setf (fighting-of ch)
           (cons tch (delete tch (fighting-of ch)))))
    (t
     (setf (move-of ch) (max 0 (- (move-of ch) 5)))
     (attack ch tch +type-undefined+)
     (wait-state ch +pulse-violence+))))

(defun perform-flee (ch)
  nil)

(defcommand (ch "slay") ()
  (send-to-char ch "Kill who?~%"))

(defcommand (ch "slay" target) (:immortal)
  (let* ((targets (resolve-alias ch target (people-of (in-room-of ch))))
         (tch (first targets)))
    (cond
      ((null targets)
       (send-to-char ch "They aren't here.~%"))
      ((find ch targets)
       (send-to-char ch "Your mother would be so sad... :(~%"))
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

(defcommand (ch "hit" target) ()
  (perform-hit ch (first (resolve-alias ch target (people-of (in-room-of ch))))))

(defcommand (ch "kill") ()
  (send-to-char ch "Kill who?~%"))

(defcommand (ch "kill" target) ()
  (let ((victims (resolve-alias ch target (people-of (in-room-of ch)))))
    (cond
      ((null victims)
       (send-to-char ch "They aren't here.~%"))
      (()))))