(in-package #:tempus)

(defun check-mob-reaction (ch vict)
  "Returns T if VICT gets pissed off at CH for doing something
annoying."
  (cond
    ((and (not (is-pc vict))
         (> (- (/ (- (alignment-of vict)
                     (alignment-of ch))
                  20)
               (cha-of ch))
            (random 101)))
     (act vict :all-emit "$n get$% pissed off!")
     t)
    (t
     nil)))

(defun raw-eq-damage (ch pos base-damage)
  "Returns amount of damage done by the CH's equipment or implant at
POS, given the BASE-DAMAGE."
  (let ((obj (or (aref (equipment-of ch) pos)
                 (aref (implants-of ch) pos))))
    (cond
      ((is-obj-kind obj +item-armor+)
       (+ base-damage
          (weight-of obj)
          (floor (obj-val-of obj 0) 4)))
      ((is-obj-kind obj +item-weapon+)
       (+ base-damage
          (dice (obj-val-of obj 1)
                (obj-val-of obj 2))))
      (t
       base-damage))))

(defun calc-skill-prob (ch vict skill)
  "Returns a number from 0 to 100 which is the probability that CH
  will succeed at SKILL against VICT.  Also returns values for WAIT,
  VICT-WAIT, MOVE, MANA, DAM, FAIL-POS, VICT-POS, WEAPON, LOCATION,
  and AFFECT."
  (let ((prob (+ (check-skill ch skill)
                 (dex-of ch)
                 (floor (str-app-type-to-hit (str-of ch)) 2)
                 (floor (hitroll-of ch) 2)
                 (max -200 (floor (armor-of vict) 10))))
        (bad-terrain nil))
    (when (< (check-skill ch skill)
             (learned ch))
      (decf prob (- (learned ch) (check-skill ch skill))))
    (when (< (position-of vict) +pos-fighting+)
      (incf prob (* (- +pos-fighting+ (position-of vict)) 6))
      (decf prob (floor (dex-of vict) 2)))
    (when (is-drow ch)
      (cond
        ((room-is-dark (in-room-of ch))
         (incf prob 10))
        ((room-is-sunny (in-room-of ch))
         (decf prob 25))))
    (when (and (aff2-flagged ch +aff2-displacement+)
               (not (aff2-flagged vict +aff2-true-seeing+)))
      (incf prob 5))
    (when (aff-flagged ch +aff-blur+)
      (incf prob 5))
    (when (not (can-see-creature vict ch))
      (incf prob 20))
    (unless (zerop (get-condition ch +drunk+))
      (decf prob (* (get-condition ch +drunk+) 2)))
    (when (is-sick ch)
      (decf prob 5))
    (when (and (aff2-flagged vict +aff2-displacement+)
               (not (aff2-flagged ch +aff2-true-seeing+)))
      (decf prob (floor (level-of vict) 2)))
    (when (aff2-flagged vict +aff2-evade+)
      (decf prob (+ (floor (level-of vict) 2) 5)))
    (when (and (is-barb ch)
               (get-eq ch +wear-wield+))
      (incf prob (floor (- (learned ch)
                           (weapon-prof ch (get-eq ch +wear-wield+)))
                        2)))
    (when (or (room-is-watery (in-room-of ch))
              (eql (terrain-of (in-room-of ch)) +sect-astral+)
              (room-is-open-air (in-room-of ch)))
      (setf bad-terrain t)
      (decf prob (floor (* prob 20) 100)))

    (cond
      ((eql skill +skill-bash+)
       (decf prob (floor (str-app-type-wield-w (str-of vict)) 2))
       (decf prob (floor (- (weight-of vict)
                            (weight-of ch))
                         16))
       (when (get-eq ch +wear-wield+)
         (incf prob (weight-of (get-eq ch +wear-wield+))))
       (when (get-eq ch +wear-shield+)
         (incf prob (weight-of (get-eq ch +wear-shield+))))
       (when bad-terrain
         (setf prob (* prob 0.9)))
       (when (and (or (mob-flagged vict +mob-nobash+)
                      (< (position-of vict)
                         +pos-fighting+))
                  (not (immortalp ch)))
         (setf prob 0))

       (values prob
               (if (is-barb ch)
                   (- (rl-sec 9)
                      (floor (* 5 (get-skill-bonus ch +skill-bash+)
                                100)))
                   (rl-sec 9))
               (rl-sec 2)
               10 0
               (if (is-barb ch)
                   (dice 2 (floor (level-of ch) 4))
                   (+ (dice 2 (floor (level-of ch) 4))
                      (dice 2 (get-skill-bonus ch +skill-bash+))))
               +pos-sitting+
               +pos-sitting+
               nil
               nil
               nil))
      ((eql skill +skill-strike+)
       (let ((dam (cond
                    ((get-eq ch +wear-wield+)
                     (raw-eq-damage ch +wear-wield+ 0))
                    ((get-eq ch +wear-hands+)
                     (raw-eq-damage ch +wear-hands+ 0)))))
         (cond
           (dam
             (values prob
                     (rl-sec 2)
                     (rl-sec 1)
                     0 0
                     (* dam 2)
                     nil nil nil
                     +wear-random+
                     nil))
           (t
            (send-to-char ch "You need a weapon to strike out with!~%")
            nil))))
      ((eql skill +skill-headbutt+)
       (values
        (if (and (not (affected-by-spell ch +skill-kata+))
                 (or (is-pudding vict)
                     (is-slime vict)
                     (noncorporealp vict)))
            0
            prob)
        (rl-sec 4)
        (rl-sec 1)
        0 0
        (dice 3 (floor (level-of ch) 8))
        nil nil nil +wear-head+ nil))



      )
    ))

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
     (attack ch target (get-next-weapon ch +type-undefined+) +type-undefined+)
     (wait-state ch +pulse-violence+))))

(defun calc-fleeing-exp-penalty (ch tch)
  (unless tch
    (return-from calc-fleeing-exp-penalty 0))
  (let ((loss (* (level-of tch)
                 (1+ (floor (level-of ch)
                            (- (1+ +lvl-grimp+)
                               (level-of ch)))))))
    (when (is-remort ch)
      (setf loss (- loss (* loss (floor (remort-gen-of ch)
                                        (+ 2 (remort-gen-of ch)))))))

    (when (is-thief ch)
      (setf loss (floor loss 2)))
    loss))

(defun attempt-fleeing (ch)
  (let* ((dir (random-range 0 (1- +num-of-dirs+)))
         (exit-info (exit ch dir))
         (other-room (when exit-info (real-room (to-room-of exit-info)))))
    (when (and (can-go ch dir)
               (not (room-flagged other-room +room-noflee+))
               (not (room-flagged other-room +room-death+))
               (not (room-flagged other-room +room-godroom+))
               (or (is-pc ch) (not (room-flagged other-room +room-nomob+))))
      (act ch :place-emit "$n panics, and attempts to flee!")
      (when (or (room-is-open-air (in-room-of ch))
                (room-is-open-air other-room))
          (if (aff-flagged ch +aff-inflight+)
              (setf (position-of ch) +pos-flying+)
              (return-from attempt-fleeing nil)))
      (let* ((fighting (random-elt (fighting-of ch)))
             (loss (calc-fleeing-exp-penalty ch fighting)))
        (remove-all-combat ch)
        (when (= (do-simple-move ch dir :flee t) 1)
          (act ch :place-emit "$n tries to flee, but can't!")
          (return-from attempt-fleeing nil))

        (send-to-char ch "You flee head over heels.~%")
        (when fighting
          (gain-exp ch (- loss))
          (gain-exp fighting (floor loss 32)))
        t))))

(defun perform-flee (ch)
  (cond
    ((aff2-flagged ch +aff2-petrified+)
     (send-to-char ch "You are solid stone!~%"))
    ((and (aff2-flagged ch +aff2-berserk+)
          (fighting-of ch))
     (send-to-char ch "You are too enraged to flee!~%"))
    ((< (position-of ch)
        +pos-fighting+)
     (send-to-char ch "You can't flee until you get on your feet!~%"))
    (t
     (loop
        for time from 0 upto 6
        as success = (attempt-fleeing ch)
        until success
        finally (unless success
                  (send-to-char ch "PANIC!  You couldn't escape!~%"))))))

(defun perform-assist (ch tch)
  (let ((opponent (random-elt (remove-if-not (lambda (x)
                                               (is-visible-to x ch))
                                             (fighting-of tch)))))
    (cond
      ((fighting-of ch)
       (send-to-char ch "You're already fighting!  How can you assist someone else?~%"))
      ((eql ch tch)
       (send-to-char ch "You can't help yourself more than this!~%"))
      ((null (fighting-of tch))
       (act ch :target tch :subject-emit "But nobody is fighting $M!"))
      ((null opponent)
       (act ch :target tch :subject-emit "You can't see who is fighting $M!"))
      ((can-attack ch opponent)
       (act ch :target tch
            :subject-emit "You join the fight!"
            :target-emit "$N assists you!"
            :not-target-emit "$n assists $N.")
       (perform-hit ch opponent)))))

(defcommand (ch "assist") (:standing)
  (send-to-char ch "Whom do you wish to assist?~%"))

(defcommand (ch "assist" target) (:standing)
  (let ((tchs (resolve-alias ch target (people-of (in-room-of ch)))))
    (cond
      ((null tchs)
       (send-to-char ch "No-one by that name here.~%"))
      ((cdr tchs)
       (send-to-char ch "You can only assist one person at a time!~%"))
      (t
       (perform-assist ch (first tchs))))))

(defcommand (ch "flee") ()
  (perform-flee ch))

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

(defun perform-backstab (ch vict)
  (let ((weapon (cond ((and (get-eq ch +wear-wield+)
                            (stabbing-weapon? (get-eq ch +wear-wield+)))
                       (get-eq ch +wear-wield+))
                      ((and (get-eq ch +wear-wield-2+)
                            (stabbing-weapon? (get-eq ch +wear-wield-2+)))
                       (get-eq ch +wear-wield-2+))
                      ((and (get-eq ch +wear-hands+)
                            (stabbing-weapon? (get-eq ch +wear-hands+)))
                       (get-eq ch +wear-hands+)))))
    (cond
      ((null vict)
       (send-to-char ch "You don't see that person"))
      ((eql ch vict)
       (send-to-char ch "You fail to sneak up on yourself.~%"))
      ((< (check-skill ch +skill-backstab+) 30)
       (send-to-char ch "You aren't really sure how to backstab properly."))
      ((null weapon)
       (send-to-char ch "You need to be using a stabbing weapon.~%"))
      ((null (can-attack ch vict))
       ;; Message already sent
       nil)
      ((fighting-of vict)
       (send-to-char ch "Backstab a fighting person? They're way too alert!~%"))
      (t
       (let ((percent (+ (random-range 1 101) (int-of vict)))
             (prob (+ (check-skill ch +skill-backstab+)
                      (if (can-see-creature vict ch) 0 32)
                      (if (aff-flagged ch +aff-sneak+)
                          (random-range 10 25)
                          -5))))
         (cond
           ((and (awakep vict)
                 (> percent prob))
            (wait-state ch (rl-sec 2))
            (damage-creature ch vict 0 weapon +skill-backstab+ +wear-back+))
           (t
            (wait-state vict (rl-sec 1))
            (wait-state ch (rl-sec 4))
            (attack ch vict weapon +skill-backstab+))))))))