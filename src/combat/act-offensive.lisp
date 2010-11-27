(in-package #:tempus)

(defun perform-hit (ch target)
  (cond
    ((null target)
     (send-to-char ch "They don't seem to be here.~%")
     (wait-state ch 4))
    ((eql ch target)
     (act ch
          :subject-emit "You hit yourself...  OUCH!"
          :place-emit "$n hits $mself.  That's gotta hurt!"))
    ((and (aff-flagged ch +aff-charm+)
          (eql (master-of ch) target))
     (act ch :target target
          :subject-emit "$N is such a good friend, you simply can't hit $M."))
    ((not (can-attack ch target))
     nil)
    ((find target (fighting-of ch))
     (act ch :target target :subject-emit  "You concentrate your attacks on $N!")
     (setf (fighting-of ch)
           (cons target (delete target (fighting-of ch)))))
    (t
     (setf (move-of ch) (max 0 (- (move-of ch) 5)))
     (attack ch target +type-undefined+)
     (wait-state ch +pulse-violence+))))

(defun perform-flee (ch)
  nil)

(defcommand (ch "attack") (:standing)
  (send-to-char ch "Attack who?~%"))

(defcommand (ch "attack" target) (:standing)
  (perform-hit ch (first (resolve-alias ch target (people-of (in-room-of ch))))))

(defcommand (ch "fight") (:standing)
  (send-to-char ch "Fight who?~%"))

(defcommand (ch "fight" target) (:standing)
  (perform-hit ch (first (resolve-alias ch target (people-of (in-room-of ch))))))

(defcommand (ch "hit") (:standing)
  (send-to-char ch "Hit who?~%"))

(defcommand (ch "hit" target) (:standing)
  (perform-hit ch (first (resolve-alias ch target (people-of (in-room-of ch))))))

(defcommand (ch "kill") (:standing)
  (send-to-char ch "Kill who?~%"))

(defcommand (ch "kill" target) (:standing)
  (perform-hit ch (first (resolve-alias ch target (people-of (in-room-of ch))))))

(defcommand (ch "murder") (:standing)
  (send-to-char ch "Murder who?~%"))

(defcommand (ch "murder" target) (:standing)
  (perform-hit ch (first (resolve-alias ch target (people-of (in-room-of ch))))))

(defcommand (ch "slay") (:standing)
  (send-to-char ch "Slay who?~%"))

(defcommand (ch "slay" target) (:standing)
  (let* ((targets (resolve-alias ch target (people-of (in-room-of ch))))
         (tch (first targets)))
    (cond
      ((null targets)
       (send-to-char ch "They aren't here.~%"))
      ((not (immortalp ch))
       (perform-hit ch (first (resolve-alias ch target (people-of (in-room-of ch))))))
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