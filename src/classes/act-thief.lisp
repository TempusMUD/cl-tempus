(in-package #:tempus)

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
