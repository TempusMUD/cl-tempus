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
      ((null obj)
       base-damage)
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
                 (floor (str-app-type-to-hit (aref +str-app+ (str-of ch))) 2)
                 (floor (hitroll-of ch) 2)
                 (max -200 (floor (armor-of vict) 10))))
        (wait 0)
        (vict-wait 0)
        (move 0)
        (mana 0)
        (dam 0)
        (fail-pos 0)
        (vict-pos 0)
        (weapon (get-eq ch +wear-wield+))
        (location 0)
        (affect nil)
        (needed-hands 0)
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

    (alexandria:switch (skill)
      (+skill-bash+
       (decf prob (floor (str-app-type-wield-w (aref +str-app+ (str-of vict))) 2))
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

       (setf dam (dice 2 (floor (level-of ch) 4)))
       (setf fail-pos +pos-sitting+)
       (setf vict-pos +pos-sitting+)
       (setf vict-wait (rl-sec 2))
       (setf wait (rl-sec 9))

       (when (is-barb ch)
         ;; Reduced wait state and increased damage for barbs
         (incf dam (dice 2 (get-skill-bonus ch +skill-bash+)))
         (decf wait (floor (* 5 (get-skill-bonus ch +skill-bash+) 100)))))

      (+skill-strike+
       (setf dam (cond
                   ((get-eq ch +wear-wield+)
                    (raw-eq-damage ch +wear-wield+ 0))
                   ((get-eq ch +wear-hands+)
                    (raw-eq-damage ch +wear-hands+ 0))
                   (t
                    0)))
       (when (zerop dam)
         (send-to-char ch "You need a weapon to strike out with!~%")
         (return-from calc-skill-prob -1))

       (setf dam (* 2 dam))
       (setf wait (rl-sec 2))
       (setf location +wear-random+)
       (setf vict-wait (rl-sec 1)))

      (+skill-headbutt+
       (setf dam (dice 3 (floor (level-of ch) 8)))
       (setf dam (raw-eq-damage ch +wear-head+ dam))

       (setf vict-wait (rl-sec 1))
       (setf wait (rl-sec 4))
       (setf location +wear-head+)

       (when (is-barb ch)
         (incf dam (get-skill-bonus ch +skill-headbutt+))
         (when (< (random-range 1 20)
                  (- (+ (str-of ch)
                        (dex-of ch))
                     (+ (con-of vict)
                        (dex-of vict))))
           (setf vict-pos +pos-sitting+))))

      (+skill-gouge+
       (when (mob-flagged vict +mob-noblind+)
         (setf prob 0))

       (setf needed-hands 1)
       (let ((eye-prot (get-eq vict +wear-eyes+)))
         (when (and eye-prot (is-obj-kind eye-prot +item-armor+))
           (decf prob (obj-val-of eye-prot 0))))

       (setf dam (dice 6 (floor (level-of ch) 16)))
       (setf wait (rl-sec 6))
       (setf vict-wait (rl-sec 1))
       (setf location +wear-eyes+)

       (setf affect (make-instance 'affected-type
                                   :kind +skill-gouge+
                                   :duration (1+ (if (> (level-of ch) 40) 1 0))
                                   :modifier -10
                                   :location +apply-hitroll+
                                   :owner (idnum-of ch)
                                   :aff-index 1
                                   :bitvector +aff-blind+)))

      (+skill-piledrive+
       (when (> (height-of vict)
                (* (height-of ch)
                   2))
         (decf prob (- 100 (level-of ch))))
       (when (< (position-of vict)
                +pos-standing+)
         (incf prob (* 10 (- +pos-standing+ (position-of vict)))))

       (when (mob-flagged vict +mob-nobash+)
         (setf prob 0))

       (when (> (+ (weight-of vict)
                   (floor (+ (carry-weight-of vict) (worn-weight-of vict)) 2))
                (* (can-carry-weight ch) 3/2))
         (act ch :target vict
              :subject-emit "$N is too heavy for you to lift!"
              :target-emit "$n tries to pick you up and piledrive you!"
              :not-target-emit "$n tries to pick $N up and piledrive $M!")
         (setf prob 0)
         (when (check-mob-reaction ch vict)
           (attack vict ch (get-next-weapon vict +type-undefined+)
                   +type-undefined+))
         (return-from calc-skill-prob -1)))

      (+skill-bodyslam+
       (when (mob-flagged vict +mob-nobash+)
         (setf prob 0))

       (when (> (+ (weight-of vict)
                   (floor (+ (carry-weight-of vict) (worn-weight-of vict)) 2))
                (* (can-carry-weight ch) 3/2))
         (act ch :target vict
              :subject-emit "$N is too heavy for you to lift!"
              :target-emit "$n tries to pick you up and bodyslam you!"
              :not-target-emit "$n tries to pick $N up and bodyslam $M!")
         (setf prob 0)
         (when (check-mob-reaction ch vict)
           (attack vict ch (get-next-weapon vict +type-undefined+)
                   +type-undefined+))
         (return-from calc-skill-prob -1))

       (setf needed-hands 1)
       (setf dam (dice 10 (floor (level-of ch)
                                 8)))
       (setf vict-pos +pos-sitting+)
       (setf wait (rl-sec 7))
       (setf vict-wait (rl-sec 2))
       (setf move 15))

      (+skill-kick+
       (setf dam (dice 2 (floor (level-of ch) 4)))
       (setf dam (raw-eq-damage ch +wear-feet+ dam))
       (setf wait (rl-sec 3)))

      (+skill-spinkick+
       (setf dam (dice 3 (floor (level-of ch) 4)))
       (setf dam (raw-eq-damage ch +wear-feet+ dam))
       (setf wait (rl-sec 4)))

      (+skill-roundhouse+
       (setf dam (dice 4 (floor (level-of ch) 4)))
       (setf dam (raw-eq-damage ch +wear-feet+ dam))
       (setf wait (rl-sec 5)))

      (+skill-pele-kick+
       (setf dam (dice 7 (floor (level-of ch) 4)))
       (setf dam (raw-eq-damage ch +wear-feet+ dam))
       (setf wait (rl-sec 6))
       (setf fail-pos +pos-sitting+))

      (+skill-sidekick+
       (when (or (< (position-of vict) +pos-fighting+)
                 (mob-flagged vict +mob-nobash+))
         (setf prob 0))
       (setf dam (dice 5 (floor (level-of ch) 2)))
       (setf dam (raw-eq-damage ch +wear-feet+ dam))
       (setf wait (rl-sec 5))
       (setf fail-pos +pos-sitting+)
       (setf vict-pos +pos-sitting+)
       (setf vict-wait (rl-sec 2)))

      (+skill-groinkick+
       (setf dam (dice 3 (floor (level-of ch) 4)))
       (setf dam (raw-eq-damage ch +wear-feet+ dam))
       (setf wait (rl-sec 5)))

      (+skill-punch+
       (setf needed-hands 1)
       (incf prob (dex-of ch))
       (setf dam (dice 1 (floor (level-of ch) 8)))
       (setf dam (raw-eq-damage ch +wear-hands+ dam))
       (setf wait (rl-sec 3)))

      (+skill-spinfist+
       (setf needed-hands 1)
       (setf dam (dice 2 (floor (level-of ch) 8)))
       (setf dam (raw-eq-damage ch +wear-hands+ dam))
       (setf wait (rl-sec 3)))

      (+skill-claw+
       (setf needed-hands 1)
       (setf dam (dice 2 (floor (level-of ch) 8)))
       (setf dam (raw-eq-damage ch +wear-hands+ dam))
       (setf wait (rl-sec 3)))

      (+skill-jab+
       (setf needed-hands 1)
       (setf dam (1+ (dice 1 (floor (level-of ch) 8))))
       (setf dam (raw-eq-damage ch +wear-hands+ dam))
       (setf wait (rl-sec 3)))

      (+skill-rabbitpunch+
       (setf needed-hands 1)
       (setf dam (1+ (dice 1 (floor (level-of ch) 8))))
       (setf dam (raw-eq-damage ch +wear-hands+ dam))
       (setf wait (rl-sec 3)))

      (+skill-hook+
       (setf needed-hands 1)
       (setf dam (1+ (dice 2 (floor (level-of ch) 8))))
       (setf dam (raw-eq-damage ch +wear-hands+ dam))
       (setf wait (rl-sec 3)))

      (+skill-uppercut+
       (setf needed-hands 1)
       (setf dam (+ 10 (dice 3 (floor (level-of ch) 8))))
       (when (is-ranger ch)
         (setf dam (* dam 2)))
       (setf dam (raw-eq-damage ch +wear-hands+ dam))
       (setf wait (rl-sec 4))
       (setf location +wear-face+))

      (+skill-elbow+
       (setf dam (dice 2 (floor (level-of ch) 8)))
       (setf dam (raw-eq-damage ch +wear-arms+ dam))
       (setf wait (rl-sec 4)))

      (+skill-knee+
       (setf dam (dice 3 (floor (level-of ch) 8)))
       (setf dam (raw-eq-damage ch +wear-legs+ dam))
       (setf wait (rl-sec 5)))

      (+skill-stomp+
       (setf dam (dice 5 (floor (level-of ch) 4)))
       (setf dam (raw-eq-damage ch +wear-feet+ dam))
       (setf location +wear-feet+)
       (setf wait (rl-sec 4))
       (setf vict-wait (rl-sec 1)))

      (+skill-clothesline+
       (when (or (mob-flagged vict +mob-nobash+)
                 bad-terrain)
         (setf prob 0))

       (setf dam (dice 2 (floor (level-of ch) 8)))
       (setf dam (raw-eq-damage ch +wear-arms+ dam))
       (setf location +wear-neck-1+)
       (setf wait (rl-sec 4))
       (setf vict-pos +pos-sitting+))

      (+skill-sweepkick+
       (when (or (mob-flagged vict +mob-nobash+)
                 bad-terrain)
         (setf prob 0))

       (setf dam (dice 2 (floor (level-of ch) 8)))
       (setf wait (rl-sec 5))
       (setf vict-pos +pos-sitting+)
       (setf vict-wait (rl-sec 2)))

      (+skill-trip+
       (when (or (mob-flagged vict +mob-nobash+)
                 bad-terrain)
         (setf prob 0))

       (setf dam (dice 2 (floor (level-of ch) 8)))
       (setf wait (rl-sec 5))
       (setf vict-pos +pos-sitting+)
       (setf vict-wait (rl-sec 2)))

      (+skill-bearhug+
       (setf needed-hands 2)
       (setf location +wear-body+)
       (setf dam (dice 4 (floor (level-of ch) 8)))
       (setf wait (rl-sec 6))
       (setf vict-wait (rl-sec 3)))

      (+skill-bite+
       (setf dam (dice 2 (floor (level-of ch) 8)))
       (setf wait (rl-sec 4)))

      (+skill-choke+
       (when (and (aff2-flagged vict +aff2-neck-protected+)
                  (> (random-range 0 (* (level-of vict) 4))
                     (random-range 0 (level-of ch))))

         (let ((neck (or (get-eq vict +wear-neck-1+)
                         (get-eq vict +wear-neck-2+))))
           (act ch :target vict :item neck
                :all-emit "$n attempt$% to choke $N, but $p has $M covered!")
           (return-from calc-skill-prob -1)))

       (setf needed-hands 1)
       (setf location +wear-neck-1+)
       (setf dam (dice 3 (floor (level-of ch)
                                8)))
       (setf wait (rl-sec 4))
       (setf vict-wait (rl-sec 2)))

      (+skill-behead+
       (when (or (null weapon) (not (slashing-weapon? weapon)))
         (setf weapon (get-eq ch +wear-hands+)))

       (when (or (null weapon)
                 (not (slashing-weapon? weapon)))
         (send-to-char ch "You need to wield a slashing weapon to do this.~%")
         (return-from calc-skill-prob -1))

       (when (is-alias-of "headless" (aliases-of vict))
         (act ch :target vict :subject-emit "$N doesn't have a head!"))

       (when (and (< (height-of ch) (floor (height-of vict) 2))
                  (> (position-of vict) +pos-sitting+))
         (cond
           ((aff-flagged ch +aff-inflight+)
            (setf dam (floor dam 4)))
           (t
            (act ch :target vict :subject-emit "$N is over twice your height!  You can't reach $S head!")
            (return-from calc-skill-prob -1))))

       (decf prob 20)

       (when (or (is-pudding vict)
                 (is-slime vict)
                 (is-race vict +race-beholder+))
         (setf prob 0))

       (setf location +wear-neck-1+)
       (setf move 20)
       (setf wait (rl-sec 6))

       (setf dam (* (dice (obj-val-of weapon 1)
                          (obj-val-of weapon 2))
                    6))
       (incf dam (dice (floor (* (level-of ch) 3) 4) 5))
       (if (and (is-obj-stat2 weapon +item2-two-handed+)
                (eql (worn-on-of weapon) +wear-wield+))
           (setf dam (* dam 2))
           (setf dam (floor (* dam 4) 5)))
       (when (and (aff2-flagged vict +aff2-neck-protected+)
                  (> (random-range 0 (* (level-of vict) 4))
                     (random-range 0 (level-of ch))))
         (let ((neck (get-eq vict +wear-neck-1+)))
           (when (or (null neck)
                     (logtest (aref (bitvector-of neck) 1) +aff2-neck-protected+))
             (setf neck (get-eq vict +wear-neck-2+)))
           (when (or (null neck)
                     (logtest (aref (bitvector-of neck) 1) +aff2-neck-protected+))
             (setf neck (get-implant vict +wear-neck-1+)))
           (when (or (null neck)
                     (logtest (aref (bitvector-of neck) 1) +aff2-neck-protected+))
             (setf neck (get-implant vict +wear-neck-2+)))

           (when neck
             ;; half the damage only if neck eq survives
             (when (damage-eq ch neck dam +type-hit+)
               (setf dam (floor dam 2)))))))

      (+skill-shoot+
       (incf prob (floor (dex-of ch) 2))
       (decf prob (strength-hit-bonus (str-of ch))))

      (+skill-archery+
       (incf prob (floor (dex-of ch) 2))
       (decf prob (strength-hit-bonus (str-of ch))))

      (+skill-palm-strike+
       (setf needed-hands 1)
       (setf location +wear-body+)
       (setf dam (dice (level-of ch)
                       (floor (str-of ch)
                              4)))
       (setf dam (raw-eq-damage ch +wear-hands+ dam))
       (setf wait (rl-sec 3))
       (setf vict-wait (rl-sec 1)))

      (+skill-throat-strike+
       (setf needed-hands 1)
       (setf location +wear-neck-1+)
       (setf dam (dice (level-of ch) 5))
       (setf dam (raw-eq-damage ch +wear-hands+ dam))
       (setf wait (rl-sec 4))
       (setf vict-wait (rl-sec 2)))

      (+skill-crane-kick+
       (setf location +wear-head+)
       (setf dam (dice (level-of ch) 7))
       (setf dam (raw-eq-damage ch +wear-feet+ dam))
       (setf wait (rl-sec 6))
       (setf vict-wait (rl-sec 2))
       (setf fail-pos +pos-sitting+))

      (+skill-scissor-kick+
       (setf location +wear-neck-1+)
       (setf dam (dice (level-of ch) 9))
       (setf dam (raw-eq-damage ch +wear-feet+ dam))
       (setf wait (rl-sec 6))
       (setf vict-wait (rl-sec 2))
       (setf fail-pos +pos-sitting+)
       (setf move 10))

      (+skill-ridgehand+
       (setf needed-hands 1)
       (setf location +wear-neck-1+)
       (setf dam (dice (level-of ch) 9))
       (setf dam (raw-eq-damage ch +wear-hands+ dam))
       (setf wait (rl-sec 4))
       (setf vict-wait (rl-sec 1))
       (setf move 10))

      (+skill-death-touch+
       (setf needed-hands 1)
       (setf location +wear-neck-1+)
       (setf dam (dice (level-of ch) 13))
       (setf wait (rl-sec 7))
       (setf vict-wait (rl-sec (+ 2 (random-range 0 (floor (level-of ch) 8)))))
       (setf move 35))

      (+skill-hip-toss+
       (setf needed-hands 1)
       (setf dam (dice 2 (level-of ch)))
       (setf dam (raw-eq-damage ch +wear-waist+ dam))
       (setf wait (rl-sec 7))
       (setf vict-wait (rl-sec 3))
       (setf vict-pos +pos-resting+)
       (setf move 10))

      (+skill-shoulder-throw+
       (setf needed-hands 1)
       (setf dam (dice 3 (+ (str-of ch) (floor (level-of ch) 4))))
       (setf wait (rl-sec 6))
       (setf vict-wait (rl-sec 2))
       (setf vict-pos +pos-resting+)
       (setf move 15))

      (+skill-psiblast+
       (when (mindlessp vict)
         (setf prob 0))
       (setf dam (dice (floor (get-skill-bonus ch +skill-psiblast+) 2)
                       (1+ (int-of ch))))
       (incf dam (check-skill ch +skill-psiblast+))
       (when (mag-savingthrow ch (level-of ch)
                              +saving-psi+)
         (setf dam (floor dam 2)))
       (setf wait (rl-sec 5))
       (setf vict-wait (rl-sec 2))
       (setf mana (spell-mana-cost ch +skill-psiblast+))
       (setf move 10))

      (+skill-scream+
       (when (is-undead vict)
         (setf prob 0))
       (setf dam (* (con-of ch)
                    (floor (+ 10 (get-skill-bonus ch +skill-scream+)) 4)))
       (incf dam (dice 15 (floor (get-skill-bonus ch +skill-scream+) 4)))

       (let ((aff (affected-by-spell ch +song-fortissimo+)))
         (when aff
           (incf dam (+ (get-skill-bonus ch +song-fortissimo+)
                        (level-of aff)))))

       (when (mag-savingthrow ch (level-of ch) +saving-breath+)
         (setf dam (floor dam 2)))

       (setf wait (rl-sec 5))
       (setf vict-wait (rl-sec 2))
       (setf mana (spell-mana-cost ch +skill-scream+))
       (setf move 25))

      (+skill-garotte+
       (when (and (not (aff-flagged ch +aff-sneak+))
                  (not (aff3-flagged ch +aff3-infiltrate+)))
         (setf prob 0))

       (setf needed-hands 2)
       (setf location +wear-neck-1+)
       (setf dam (dice (level-of ch)
                       5))
       (setf wait (rl-sec 4))
       (setf vict-wait (rl-sec 4)))
      (t
       (send-to-char ch "There was an error.~%")
       (return-from calc-skill-prob -1)))

    (let ((hands-free (hands-free ch)))
      (cond
        ((and (= needed-hands 1)
              (> needed-hands hands-free))
         (send-to-char ch "You need a hand free to do that!~%")
         (return-from calc-skill-prob -1))
        ((and (= needed-hands 2)
              (> needed-hands hands-free))
         (send-to-char ch "You need both hands free to do that!~%")
         (return-from calc-skill-prob -1))
        ((and (plusp move)
              (< (move-of ch) move))
         (send-to-char ch "You are too exhausted!~%")
         (return-from calc-skill-prob -1))
        ((and (plusp mana)
              (< (mana-of ch) mana))
         (send-to-char ch "You lack the spiritual energies needed!~%")
         (return-from calc-skill-prob -1))
        (t
         (incf dam (strength-damage-bonus (str-of ch)))
         (incf dam (damroll-of ch))
         (incf dam (floor (* dam (remort-gen-of ch))
                          10))
         (if (> (check-skill ch skill)
                (learned ch))
             (setf dam (* dam (+ (learned ch)
                                 (floor (- (check-skill ch skill)
                                           (learned ch))
                                        2))))

             (setf dam (* dam (check-skill ch skill))))
         (setf dam (floor dam (learned ch)))

         (when (and (is-monk ch)
                    (not (is-neutral ch)))
           (decf prob (floor (* prob (abs (alignment-of ch))) 1000))
           (decf dam (floor (* dam (abs (alignment-of ch))) 2000)))


         (when (and (not (affected-by-spell ch +skill-kata+))
                    (or (is-pudding vict)
                        (is-slime vict)
                        (noncorporealp vict)))
           (setf prob 0))

         (values (alexandria:clamp prob 0 110)
                 wait vict-wait move mana dam fail-pos vict-pos weapon location affect))))))

(defun perform-offensive-skill (ch vict skill)
  (cond
    ((not (can-attack ch vict))
     (act ch :target vict
          :subject-emit "You seem to be unable to attack $N."
          :target-emit "$n shakes with rage as $e tries to attack you!"
          :not-target-emit "$n shakes with rage as $e tries to attack $N!"))
    ((and (spell-is-psionic skill)
          (room-flagged (in-room-of ch)
                        +room-nopsionics+))
     (send-to-char ch "Psychic powers are useless here!~%"))
    (t
     (multiple-value-bind (prob wait vict-wait move mana dam fail-pos
                                vict-pos weapon location affect)
         (calc-skill-prob ch vict skill)
       (when (pref-flagged ch +pref-debug+)
         (send-to-char ch "&c[SKILL] Prob: ~a&n~%" prob))
       (unless (= prob -1)
         (cond
           ((< prob (random-range 1 120))
            ;; failure
            (damage-creature ch vict 0 weapon skill location)
            (unless (is-dead ch)
              (when fail-pos
                (setf (position-of ch) fail-pos)
                (when (< prob 50)
                  ;; 0.1 sec for every point below 50, up to 7
                  (incf wait (max (- 50 prob) 70))))
              (decf (move-of ch) (floor move 2))
              (decf (mana-of ch) (floor mana 2))
              (wait-state ch (floor wait 2))))
           (t
            ;; success
            (decf (move-of ch) move)
            (decf (mana-of ch) mana)
            (gain-skill-proficiency ch skill)
            (wait-state ch wait)
            (damage-creature ch vict dam weapon skill location)
            (unless (or (is-dead ch) (is-dead vict))
              (when (plusp vict-pos)
                (setf (position-of vict) vict-pos))
              (when (plusp vict-wait)
                (wait-state vict vict-wait))
              (when (and affect (not (affected-by-spell vict (kind-of affect))))
                (affect-to-char vict affect))))))))))

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

(defun perform-bash-door (ch dir)
  (let ((exit (exit ch dir)))
    (cond
      ((not (logtest (exit-info-of exit) +ex-isdoor+))
       (send-to-char ch "You cannot bash that!~%"))
      ((not (logtest (exit-info-of exit) +ex-closed+))
       (send-to-char ch "It's already open!~%"))
      ((< (move-of ch) 20)
       (send-to-char ch "You are too exhausted.~%"))
      (t
       (let* ((door-damage (+ (dice 2 (floor (level-of ch) 4))
                              (* (strength-damage-bonus (str-of ch)) 4)))
              (door-str (if (keyword-of exit)
                            (first-word (keyword-of exit))
                            "door"))
              (ch-damage (dice 4 8))
              (dest-room (real-room (to-room-of exit)))
              (other-side (abs-exit dest-room (aref +rev-dir+ dir))))
         (when (< (check-skill ch +skill-break-door+)
                  (random-range 1 99))
           (setf door-damage (* door-damage 2)))
         (when (or (minusp door-damage)
                   (logtest (exit-info-of exit) +ex-pickproof+)
                   (minusp (damage-of exit))
                   (null dest-room))
           (setf door-damage 0))

         (when (pref-flagged ch +pref-debug+)
           (send-to-char ch "&c[BASH] door-damage: ~d, durability left: ~d&n~%"
                         door-damage
                         (damage-of exit)))

         (decf (damage-of exit) door-damage)

         (cond
           ((plusp (damage-of exit))
            (act ch
                 :subject-emit (format nil "You slam yourself against the ~a" door-str)
                 :place-emit (format nil "$n throws $mself against the ~a in an attempt to break it." door-str))
            (when (and dest-room (people-of dest-room) other-side)
              (act (first (people-of dest-room))
                   :all-emit (format nil "You hear a loud crash against the ~a ~a"
                                     (or (name-of other-side) "door")
                                     (aref +to-dirs+ (aref +rev-dir+ dir)))))
            (let* ((damage-ratio (/ (damage-of exit)
                                    (maxdam-of exit)))
                   (damage-desc (format nil "The ~a looks ~a.~%" door-str
                                        (cond
                                          ((> damage-ratio 99/100)
                                           "untouched")
                                          ((> damage-ratio 90/100)
                                           "virtually unharmed")
                                          ((> damage-ratio 75/100)
                                           "pretty scratched up")
                                          ((> damage-ratio 50/100)
                                           "in poor shape")
                                          ((> damage-ratio 25/100)
                                           "completely battered")
                                          (t
                                           "on the verge of breaking")))))
              (act ch
                   :subject-emit damage-desc
                   :place-emit damage-desc)))
           (t
            (act ch
                 :subject-emit (format nil "The ~a gives way under your powerful bash!" door-str)
                 :place-emit (format nil "$n bashes the ~a open with a powerful blow!" door-str))
            (setf (exit-info-of exit) (logandc2 (exit-info-of exit) +ex-closed+))
            (setf (exit-info-of exit) (logandc2 (exit-info-of exit) +ex-locked+))
            (when (and dest-room (people-of dest-room) other-side)
              (act (first (people-of dest-room))
                   :all-emit (format nil "The ~a is bashed open from the other side!!"
                                     (or (name-of other-side) "door"))))))

         (damage-creature ch ch ch-damage nil +type-pound+ nil)
         (when (and (not (is-dead ch))
                    (not (plusp (damage-of exit)))
                    (> (random-range 0 20)
                       (dex-of ch)))
           (act ch :all-emit "$n stagger$% and fall$% down.")
           (setf (position-of ch) +pos-sitting+)
           (gain-skill-proficiency ch +skill-break-door+)))))))