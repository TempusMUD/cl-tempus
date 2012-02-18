(in-package #:tempus)

(defun perform-barb-berserk (ch)
  "Randomly select and attack someone in the room with CH.  Returns
the person attacked, or NIL if no one was selected.  May raise
CREATURE-DIED condition."
  (unless (fighting-of ch)
    (let ((victim (random-elt (remove-if (lambda (tch)
                                           (or (eql tch ch)
                                               (pref-flagged tch +pref-nohassle+)
                                               (and (is-npc ch)
                                                    (is-npc tch)
                                                    (not (mob2-flagged ch +mob2-atk-mobs+)))
                                               (not (is-visible-to tch ch))))
                                         (people-of (in-room-of ch))))))
      (when victim
        (act ch :target victim
             :subject-emit "You go berserk and attack $N!"
             :target-emit "$n attacks you in a BERSERK rage!!"
             :not-target-emit "$n attacks $N in a BERSERK rage!!")
        (attack ch victim (get-next-weapon ch +type-undefined+) +type-undefined+)
        victim))))
