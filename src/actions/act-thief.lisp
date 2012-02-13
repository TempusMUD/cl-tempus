(in-package #:tempus)

(defun perform-hide (ch)
  (send-to-char ch "You attempt to hide yourself.~%")
  (let ((percent (random-range 1 101)))
    (when (and (not (immortalp ch))
               (> (worn-weight-of ch)
                  (* (can-carry-weight ch) 3/4)))
      (send-to-char ch "...but it sure will be hard with all that heavy equipment.~%")
      (incf percent 30))
    (when (or (aff-flagged ch +aff-sanctuary+)
              (aff-flagged ch +aff-glowlight+)
              (aff2-flagged ch +aff2-divine-illumination+)
              (aff2-flagged ch +aff2-fire-shield+)
              (affected-by-spell ch +spell-quad-damage+))
      (incf percent 60))
    (if (room-is-dark (in-room-of ch))
        (decf percent 30)
        (incf percent 20))
    (when (< percent (+ (check-skill ch +skill-hide+)
                        (getf (aref +dex-app-skill+ (dex-of ch)) :hide)))
      (setf (aff-flags-of ch) (logior (aff-flags-of ch) +aff-hide+))
      (gain-skill-proficiency ch +skill-hide+)
      (wait-state ch (rl-sec 1)))))

(defcommand (ch "sneak") (:standing)
  (cond
    ((aff-flagged ch +aff-sneak+)
     (send-to-char ch "Okay, you will now attempt to walk normally.~%")
     (affect-from-char ch +skill-sneak+))
    ((< (random-range 20 70)
        (check-skill ch +skill-sneak+))
     (send-to-char ch "Okay, you'll try to move silently until further notice.~%"))
    (t
     (send-to-char ch "Okay, you'll try to move silently until further notice.~%")
     (affect-to-char ch
                     (make-instance 'affected-type
                                    :kind +skill-sneak+
                                    :duration (level-of ch)
                                    :bitvector +aff-sneak+
                                    :aff-index 0
                                    :level (+ (level-of ch)
                                              (remort-gen-of ch))
                                    :owner (idnum-of ch))))))

(defcommand (ch "hide") (:standing)
  (cond
    ((aff-flagged ch +aff-hide+)
     (send-to-char ch "You stop hiding.~%")
     (setf (aff-flags-of ch) (logandc2 (aff-flags-of ch) +aff-hide+)))
    (t
     (perform-hide ch))))
