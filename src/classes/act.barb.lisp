(in-package #:tempus)

(defun perform-barb-berserk (ch)
  "Randomly select and attack someone in the room with CH.  Returns
the person attacked, or NIL if no one was selected."
  (let ((target (random-elt (remove-if
                             (lambda (tch)
                               (or (eql tch ch)
                                   (pref-flagged tch +pref-nohassle+)
                                   (and (is-npc ch)
                                        (is-npc tch)
                                        (not (mob2-flagged ch +mob2-atk-mobs+)))
                                   (not (is-visible-to tch ch))))
                             (people-of (in-room-of ch))))))
    (when target
      ; (hit ch target)
      target)))