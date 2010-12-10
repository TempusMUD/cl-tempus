(in-package :tempus)

(defun update-instant-affects (ch)
  (let ((expired (remove-if
                  (lambda (af)
                    (decf (duration-of af))
                    (plusp (duration-of af)))
                  (remove-if-not 'is-instant-of (affected-of ch)))))
    (dolist (af expired)
      (affect-remove ch af))
    expired))

(defun perform-creature-falling (ch)
  (let ((fall-to (real-room (to-room-of (exit ch +down+)))))
    (assert fall-to nil "Invalid room to fall into at ~a!" (in-room-of ch))
    ;; Handle actual falling
    (act ch :all-emit "$n fall$% downward through the air!")
    (char-from-room ch t)
    (char-to-room ch fall-to)
    (look-at-room ch fall-to nil)
    (act ch :place-emit "$n falls in from above!")
    (incf (fall-count-of ch))

    ;; Handle hitting the ground
    (when (or (not (room-is-open-air fall-to))
              (not (exit ch +down+))
              (logtest (exit-info-of (exit ch +down+))
                       (logior +ex-closed+ +ex-nopass+))
              (not (eql fall-to
                        (to-room-of (exit ch +down+)))))
      (let ((dam (dice (+ 2 (expt (fall-count-of ch) 2)) 12)))
        (cond
          ((or (room-is-watery fall-to)
               (eql (terrain-of fall-to) +sect-fire-river+)
               (eql (terrain-of fall-to) +sect-pitch-pit+)
               (eql (terrain-of fall-to) +sect-pitch-sub+))
           (setf dam (floor dam 2))
           (when (aff3-flagged ch +aff3-gravity-well+)
             (setf dam (* dam 4)))
           (act ch :all-emit "$n make$% an enormous splash!"))
          (t
           (act ch :all-emit "$n hit$% the ground hard!")))

        ;; TODO: Search with falling trigger

        (when (is-monk ch)
          (decf dam (floor (* (level-of ch) dam) 100)))

        (setf (position-of ch) +pos-resting+)

        (damage-creature nil ch dam nil +type-falling+
                         +wear-random+)))))

(defun add-radiation-sickness (ch level)
  (let ((af (affected-by-spell ch +type-rad-sickness+)))
    (cond
      (af
       (setf (duration-of af) (min (1+ (duration-of af)) 100))
       (setf (level-of af) (min (1+ (level-of af)) 50)))
      (t
       (affect-to-char ch
                       (make-instance 'affected-type
                                      :kind +type-rad-sickness+
                                      :duration (min level 100)
                                      :modifier (floor level -16)
                                      :level level
                                      :location +apply-con+
                                      :bitvector 0
                                      :aff-index 0))))))

(defun terrain-has-air (terrain)
  (not (member terrain (list +sect-water-noswim+
                             +sect-underwater+
                             +sect-freespace+
                             +sect-pitch-sub+
                             +sect-elemental-ooze+
                             +sect-elemental-water+
                             +sect-deep-ocean+))))

(defun terrain-has-water (terrain)
  (or (= terrain +sect-water-noswim+)
      (= terrain +sect-water-swim+)
      (= terrain +sect-underwater+)
      (= terrain +sect-elemental-water+)
      (= terrain +sect-deep-ocean+)))

(defun terrain-is-open-air (terrain)
  (or (= terrain +sect-flying+)
      (= terrain +sect-elemental-air+)
      (= terrain +sect-elemental-radiance+)
      (= terrain +sect-elemental-lightning+)
      (= terrain +sect-elemental-vacuum+)))

(defun needs-to-breathe (ch)
  (and (not (aff3-flagged ch +aff3-nobreathe+))
       (not (is-undead ch))
       (or (is-pc ch)
           (not (<= 16100 (vnum-of ch) 16999)))))

(defun can-travel-terrain (ch terrain)
  (or (immortalp ch)
      (and (or (not (is-race ch +race-fish+))
               (terrain-has-water terrain))
           (or (/= terrain +sect-water-noswim+)
               (aff-flagged ch +aff-waterwalk+)
               (>= (position-of ch) +pos-flying+)
               (and (is-elemental ch) (is-class ch +class-water+))
               (find +item-boat+ (carrying-of ch) :key 'kind-of)
               (find +item-boat+ (remove-if #'null (equipment-of ch)) :key 'kind-of))
           (or (not (terrain-has-water terrain))
               (or
                (aff-flagged ch +aff-waterbreath+)
                (and (is-elemental ch) (is-class ch +class-water+))
                (is-race ch +race-fish+)))
           (or (terrain-has-air terrain)
               (not (needs-to-breathe ch)))
           (or (not (terrain-is-open-air terrain))
               (aff-flagged ch +aff-inflight+)
               (and (is-elemental ch) (is-class ch +class-air+))
               (find +item-wings+ (remove-if #'null (equipment-of ch)) :key 'kind-of)))))

(defun update-creature (ch)
  (assert (in-room-of ch) nil
          "Updating a creature that fell out of the world! (~a)" ch)
  (assert (/= (position-of ch) +pos-dead+) nil
          "Updating a corpse! (~a)" ch)

  (when (aff3-flagged ch +aff3-inst-aff+)
    (update-instant-affects ch))

  (when (and (is-npc ch)
             (not (fighting-of ch))
             (not (zerop (wait-state-of ch))))
    (setf (wait-state-of ch) (max 0 (- (wait-state-of ch) +fire-tick+))))

  (when (or (and (is-npc ch)
                 (zone-flagged (zone-of (in-room-of ch)) +zone-frozen+))
            (aff2-flagged ch +aff2-petrified+))
    (return-from update-creature))

  ;; Creature is flying with gravity well
  (when (and (aff3-flagged ch +aff3-gravity-well+)
             (= (position-of ch) +pos-flying+)
             (or (null (exit ch +down+))
                 (not (room-is-open-air (in-room-of ch)))
                 (logtest (exit-info-of (exit ch +down+)) +ex-closed+))
             (not (zone-is-nograv (zone-of (in-room-of ch)))))
    (act ch :all-emit "The inexorable force of gravity slams $n to the ground!")
    (setf (position-of ch) +pos-resting+)
    (wait-state ch 1)
    (let ((af (affected-by-spell ch +spell-gravity-well+)))
      (damage-creature (or (when af (owner-of af)) ch) ch
                       (dice 6 5)
                       nil
                       +type-falling+
                       +wear-random+)
      (when (is-dead ch)
        (return-from update-creature))))

  ;; Creature is flying but unable to continue
  (when (and (eql (position-of ch) +pos-flying+)
             (not (aff-flagged ch +aff-inflight+))
             (not (immortalp ch)))
    (send-to-char ch "You can no longer fly!~%")
    (setf (position-of ch) +pos-standing+))

  ;; Creature is in open air and not flying
  (when (and (exit ch +down+)
             (< (position-of ch) +pos-flying+)
             (not (logtest (exit-info-of (exit ch +down+))
                           +ex-closed+))
             (or (not (fighting-of ch))
                 (not (aff-flagged ch +aff-inflight+)))
             (room-is-open-air (in-room-of ch))
             (not (zone-is-nograv (zone-of (in-room-of ch))))
             (or (not (mounted-of ch))
                 (not (aff-flagged (mounted-of ch) +aff-inflight+)))
             (not (eql (in-room-of ch)
                       (exit ch +down+))))
    (cond
      ((and (aff-flagged ch +aff-inflight+)
            (awakep ch)
            (not (aff3-flagged ch +aff3-gravity-well+)))
       (send-to-char ch "You realize you are about to fall and resume your flight!~%")
       (setf (position-of ch) +pos-flying+))
      (t
       (perform-creature-falling ch))))

  (when (is-dead ch) (return-from update-creature))

  ;; Comfortable rooms
  (when (room-flagged (in-room-of ch) +room-comfort+)
    (setf (hitp-of ch) (min (max-hitp-of ch) (1+ (hitp-of ch))))
    (setf (mana-of ch) (min (max-mana-of ch) (1+ (mana-of ch))))
    (setf (move-of ch) (min (max-move-of ch) (1+ (move-of ch)))))

  ;; Regen
  (when (or (aff-flagged ch +aff-regen+)
            (is-troll ch)
            (is-vampire ch))
    (setf (hitp-of ch)
          (min (max-hitp-of ch)
               (+ 1 (hitp-of ch)
                  (floor
                   (* (random-range 0 100) (con-of ch))
                   125)))))

  ;; Mana tap
  (when (aff3-flagged ch +aff3-mana-tap+)
    (setf (mana-of ch)
          (min (max-mana-of ch)
               (+ 1 (mana-of ch)
                  (random-range 0 (floor (wis-of ch) 4))))))

  ;; Mana leak
  (when (aff3-flagged ch +aff3-mana-leak+)
    (setf (mana-of ch)
          (max 0
               (- (mana-of ch) 1
                  (random-range 0 (floor (- 25 (wis-of ch)) 4))))))

  ;; Energy tap
  (when (aff3-flagged ch +aff3-energy-tap+)
    (setf (move-of ch)
          (min (max-move-of ch)
               (+ 1 (move-of ch)
                  (random-range 0 (floor (con-of ch) 4))))))

  ;; Energy leak
  (when (aff3-flagged ch +aff3-energy-leak+)
    (setf (move-of ch)
          (max 0
               (- (move-of ch) 1
                  (random-range 0 (floor (- 25 (wis-of ch)) 4))))))

  ;; Nanite reconstruction
  (when (affected-by-spell ch +skill-nanite-reconstruction+)
    (let ((obj-found nil) (obj-repaired nil))
      (loop
         for obj across (implants-of ch)
         when obj
         do
           (setf obj-found t)
           (when (and (/= (max-dam-of obj) -1)
                      (/= (damage-of obj) -1)
                      (not (is-obj-stat2 obj +item2-broken+))
                      (< (damage-of obj) (max-dam-of obj)))
             (setf obj-repaired t)
             (setf (damage-of obj) (max (max-dam-of obj)
                                        (+ (damage-of obj)
                                           (floor (get-skill-bonus ch +skill-nanite-reconstruction+)
                                                  (random-range 33 50)))))))
      (when (or (not obj-found) (not obj-repaired))
        (if obj-found
            (send-to-char ch "NOTICE: Implants not found.  Nanite reconstruction halted.~%")
            (send-to-char ch "NOTICE: Implants reconstruction complete.  Nanite reconstruction halted.~%")))))

  ;; Irresistable Dance
  (when (and (affected-by-spell ch +song-irresistable-dance+)
             (zerop (random-range 0 4)))
    (let ((tuple (random-elt '(("You dance around to the rhythm pounding in your head!"
                                "$n dances uncontrollably to the rhythm pounding in $s head!")
                               ("You dance around to the rhythm pounding in your head!"
                                "$n dances uncontrollably to the rhythm pounding in $s head!")
                               ("You dance around uncontrollably!"
                                "$n dances around as if on strings!")
                               ("You almost break your leg with your violent dance!"
                                "$n almost breaks $s leg with $s violent dance!")
                               ("Dance you fool!"
                                "$n skanks about like a fool!")))))
      (act ch
           :subject-emit (first tuple)
           :place-emit (second tuple))))

  ;; Signed the Unholy Compact and sleeping
  (when (and (plr2-flagged ch +plr2-soulless+)
             (eql (position-of ch) +pos-sleeping+)
             (zerop (random-range 0 5)))
    (act ch
         :subject-emit "The tortured cries of hell wake you from your nightmares.~%"
         :place-emit "$n bolts upright, screaming in fear!")
    (setf (position-of ch) +pos-sitting+))

  ;; Unholy Compact sellouts
  (when (and (plr2-flagged ch +plr2-soulless+)
             (is-good ch)
             (zerop (random-range 0 9)))
    (let ((dam (dice 2 3)))
      (when (> (1+ (hitp-of ch)) dam)
        (damage-creature ch ch dam nil +type-anguish+ +wear-random+))))

  (when (is-dead ch) (return-from update-creature))

  ;; Affected by sleep spell
  (when (and (aff-flagged ch +aff-sleep+)
             (> (position-of ch) +pos-sleeping+))
    (act ch
         :subject-emit "You suddenly fall into a deep sleep."
         :place-emit "$n suddenly falls asleep where $e stands.")
    (setf (position-of ch) +pos-sleeping+))

  ;; Self-destructing
  (when (aff3-flagged ch +aff3-self-destruct+)
    (cond
      ((plusp (meditate-timer-of ch))
       (send-to-char ch "Self-destruct T-minus dd and counting.~%" (meditate-timer-of ch))
       (decf (meditate-timer-of ch)))
      ((not (is-cyborg ch))
       (errlog "~a tried to self destruct at [~d]" (name-of ch) (number-of (in-room-of ch))))
      (t
       (engage-self-destruct ch))))

  (when (is-dead ch) (return-from update-creature))

  ;; Creature is poisoned (3)
  (when (and (has-poison-3 ch)
             (immortalp ch))
    (let* ((af (affected-by-spell ch +spell-poison+))
           (attacker (when af (gethash (owner-of af) *character-map*))))
      (damage-creature attacker ch (+ (dice 4 3)
                                      (if (affected-by-spell ch +spell-metabolism+)
                                          (dice 4 11)
                                          0))
                       nil +spell-poison+ +wear-random+)))

  (when (is-dead ch) (return-from update-creature))

  ;; Gravity well
  (when (and (aff3-flagged ch +aff3-gravity-well+)
             (zerop (random-range 0 9))
             (not (zone-is-nograv (zone-of (in-room-of ch)))))
    (let* ((af (affected-by-spell ch +spell-poison+))
           (attacker (when af (gethash (owner-of af) *character-map*))))
      (damage-creature attacker ch (random-range 5 (if af (level-of af) (level-of ch)))
                       nil +type-pressure+ +wear-random+)))

  (when (is-dead ch) (return-from update-creature))

  ;; Psychic crush
  (when (aff3-flagged ch +aff3-psychic-crush+)
    (let* ((af (affected-by-spell ch +spell-psychic-crush+))
           (attacker (when af (gethash (owner-of af) *character-map*))))
      (damage-creature attacker ch (if (mag-savingthrow ch (if af (level-of af) 50) +saving-psi+) 0 (dice 4 20))
                       nil +spell-psychic-crush+ +wear-head+)))

  (when (is-dead ch) (return-from update-creature))

  ;; Stigmata
  (let ((af (affected-by-spell ch +spell-psychic-crush+)))
    (when af
      (let ((attacker (gethash (owner-of af) *character-map*)))
        (damage-creature attacker ch
                         (if (mag-savingthrow ch (level-of af) +saving-spell+) 0 (dice 3 (level-of af)))
                         nil +spell-stigmata+ +wear-face+))))

  (when (is-dead ch) (return-from update-creature))

  ;; Entropy field
  (let ((af (affected-by-spell ch +spell-entropy-field+)))
    (when (and af
               (zerop (random-range 0 9))
               (mag-savingthrow ch (level-of af) +saving-phy+))
      (let ((attacker (gethash (owner-of af) *character-map*)))
        (setf (mana-of ch) (max 0
                                (- (mana-of ch)
                                   (- 13 (random-range 0 (floor (wis-of ch) 2))))))
        (setf (move-of ch) (max 0
                                (- (move-of ch)
                                   (- 13 (random-range 0 (floor (str-of ch) 2))))))
        (damage-creature attacker ch
                         (- 13 (random-range 0 (floor (con-of ch) 2)))
                         nil +spell-stigmata+ +wear-face+))))

  (when (is-dead ch) (return-from update-creature))

  ;; Acidity
  (when (aff3-flagged ch +aff3-acidity+)
    (let* ((af (affected-by-spell ch +spell-acidity+))
           (attacker (when af (gethash (owner-of af) *character-map*))))
      (damage-creature attacker ch
                       (if (mag-savingthrow ch (if af (level-of af) 50) +saving-phy+) 0 (dice 2 10))
                       nil +type-acid-burn+ +wear-random+)))

  (when (is-dead ch) (return-from update-creature))

  ;; Motor spasm
  (let ((af (affected-by-spell ch +spell-motor-spasm+)))
    (when (and af
               (not (mob-flagged ch +mob-nobash+))
               (< (position-of ch) +pos-flying+)
               (not (immortalp ch)))
      (let ((obj nil))
        (when (and (carrying-of ch)
                   (> (+ (random-range 0 (+ 3 (floor (level-of af) 2))) 3) (dex-of ch)))
          (loop
             with count = 0
             for possible-obj in (carrying-of ch)
             when (and (can-see-object ch possible-obj)
                       (not (is-obj-stat possible-obj +item-nodrop+)))
             do
               (incf count)
               (when (= (random-range 1 count) 1)
                 (setf obj possible-obj)))
          (when obj
            (act ch
                 :subject-emit "Your muscles are seized in an uncontrollable spasm!"
                 :place-emit "$n begins spasming uncontrollably.")
            (perform-single-drop ch obj :drop)))
        (when (and (null obj)
                   (> (random-range 0 (+ 12 (floor (level-of af) 2))) (dex-of ch))
                   (> (position-of ch) +pos-sitting+))
          (act ch
               :subject-emit "You fall to the ground in agony as your muscles spasm!"
               :place-emit "$n begins spasming uncontrollably and falls to the ground.")
          (setf (position-of ch) +pos-sitting+)))))

  ;; Hyperscanning increment
  (let ((af (affected-by-spell ch +skill-hyperscan+)))
    (when af
      (cond
        ((< (move-of ch) 10)
         (send-to-char ch "Hyperscanning device shutting down.~%")
         (affect-remove ch af))
        (t
         (decf (move-of ch))))))

  ;; Lich's lyrics rotting flesh
  (let ((af (affected-by-spell ch +song-lichs-lyrics+)))
    (when (and af (zerop (random-range 0 9)))
      (let ((attacker (when af (gethash (owner-of af) *character-map*)))
            (dam 0))
        (cond
          ((or (> (+ (floor (con-of ch) 2) 85) (random-range 0 100))
               (aff2-flagged ch +aff2-petrified+))
           (setf dam (+ (floor (level-of af) 8) (dice 2 5)))
           (act ch
                :subject-emit "You feel your life force being drawn away!"
                :place-emit "$n begins to pale as $s life force fades."))
          (t
           (setf dam (+ (level-of af) (dice 4 5)))
           (act ch
                :subject-emit "A large chunk of your decaying flesh rots off and falls to the ground!"
                :place-emit "A large chunk of $n's decaying flesh rots off and falls to the ground!")
           (obj-to-room (make-object :unknown +song-lichs-lyrics+
                                     :name (format nil "a decaying hunk of ~a's flesh" (name-of ch))
                                     :aliases "decaying flesh hunk"
                                     :line-desc (format nil "a decaying hunk of ~a's flesh is lying here."
                                                        (name-of ch))
                                     :kind +item-food+
                                     :wear-flags +item-wear-take+
                                     :extra-flags (logior +item-nodonate+ +item-nosell+)
                                     :extra2-flags +item2-body-part+
                                     :value0 3
                                     :value1 3
                                     :weight 2
                                     :timer (if (is-npc ch) +max-npc-corpse-time+ +max-pc-corpse-time+))
                        (in-room-of ch))
           (when (and attacker (eql (in-room-of attacker) (in-room-of ch)))
             (setf (hitp-of attacker) (min (max-hitp-of attacker) (+ (hitp-of attacker) (floor dam 4))))
             (act attacker :target ch
                  :subject-emit "You absorb some of $N's life force!"))
           (damage-creature attacker ch dam nil +song-lichs-lyrics+ +wear-random+))))))

  (when (is-dead ch) (return-from update-creature))

  ;; Burning character
  (cond
    ((aff2-flagged ch +aff2-ablaze+)
     (cond
       ((room-is-watery (in-room-of ch))
        ;; Water extinguishes the flames
        (act ch
             :subject-emit "The flames on your body sizzle out and die, leaving you in a cloud of steam."
             :place-emit "The flames on $n sizzle and die, leaving a cloud of steam.")
        (affect-from-char ch +spell-ablaze+))
       ((eql (terrain-of (in-room-of ch)) +sect-freespace+)
        ;; Terrains without oxygen kill the fire
        (act ch
             :subject-emit "The flames on your body die in the absence of oxygen."
             :place-emit "The flames on $n die in the absence of oxygen.")
        (affect-from-char ch +spell-ablaze+))
       ((and (not (char-withstands-fire ch))
             (zerop (random-range 0 2)))
        (let* ((af (affected-by-spell ch +spell-ablaze+))
               (attacker (when af (gethash (owner-of af) *character-map*))))
          (damage-creature attacker ch
                           (if (room-flagged (in-room-of ch) +room-flame-filled+)
                               (dice 8 7)
                               (dice 5 5))
                           nil
                           +type-ablaze+
                           +wear-random+)))))
    ((or (and (room-flagged (in-room-of ch) +room-flame-filled+)
              (or (not (char-withstands-fire ch))
                  (room-flagged (in-room-of ch) +room-godroom+)))
         (and (is-vampire ch) (room-is-sunny (in-room-of ch))))
     (act ch
          :subject-emit "Your body suddenly bursts into flames!"
          :place-emit "$n suddenly bursts into flames!")
     (affect-to-char ch (make-instance 'affected-type
                                       :kind +spell-ablaze+
                                       :duration -1
                                       :modifier 0
                                       :location 0
                                       :level 0
                                       :bitvector +aff2-ablaze+
                                       :aff-index 2))
     (damage-creature nil ch (dice 4 5) nil +type-ablaze+ +wear-random+)))

  (when (is-dead ch) (return-from update-creature))

  (when (and (room-flagged (in-room-of ch) +room-ice-cold+)
             (not (char-withstands-cold ch)))
    (damage-creature nil ch (dice 4 5) nil +type-freezing+ +wear-random+))

  (when (is-dead ch) (return-from update-creature))

  (when (and (room-flagged (in-room-of ch) +room-holyocean+) (is-evil ch))
    (damage-creature nil ch (dice 4 5) nil +type-holyocean+ +wear-random+))

  (when (is-dead ch) (return-from update-creature))

  (when (and (or (eql (terrain-of (in-room-of ch)) +sect-pitch-pit+)
                 (eql (terrain-of (in-room-of ch)) +sect-pitch-sub+))
             (not (char-withstands-heat ch)))
    (damage-creature nil ch (dice 4 3) nil +type-boiling-pitch+ +wear-random+))

  (when (is-dead ch) (return-from update-creature))

  (when (and (room-flagged (in-room-of ch) +room-radioactive+)
             (not (room-flagged (in-room-of ch) +room-peaceful+))
             (not (char-withstands-radiation ch)))

    (cond
      ((affected-by-spell ch +skill-radionegation+)
       (if (> (move-of ch) 10)
           (setf (move-of ch) (max 10 (- (move-of ch) (dice 2 7))))
           (add-radiation-sickness ch 10))
       (damage-creature nil ch (max (1- (hitp-of ch)) (dice 1 7)) nil +type-rad-sickness+ +wear-random+))
      (t
       (if (> (move-of ch) 5)
           (setf (move-of ch) (max 5 (- (move-of ch) (dice 2 7))))
           (add-radiation-sickness ch 20))
       (damage-creature nil ch (max (1- (hitp-of ch)) (dice 2 7)) nil +type-rad-sickness+ +wear-random+))))

  (when (is-dead ch) (return-from update-creature))

  ;; The crushing depths of the deep ocean
  (when (eql (terrain-of (in-room-of ch)) +sect-deep-ocean+)
    (damage-creature nil ch (dice 8 10) nil +type-crushing-depth+ +wear-random+))

  (when (is-dead ch) (return-from update-creature))

  ;; Airless rooms
  (cond
    ((and (not (room-has-air (in-room-of ch)))
          (not (can-travel-terrain ch (terrain-of (in-room-of ch))))
          (not (room-flagged (in-room-of ch) +room-dock+))
          (not (immortalp ch)))

     (incf (breath-count-of ch))

     (when (> (breath-count-of ch) (+ (floor (level-of ch) 10) 2))
       (damage-creature nil ch (dice 4 5) nil
                        (if (or (eql (terrain-of (in-room-of ch)) +sect-freespace+)
                                (eql (terrain-of (in-room-of ch)) +sect-elemental-earth+))
                            +type-suffocating+
                            +type-drowning+)
                        +wear-random+)

       (when (is-dead ch) (return-from update-creature)))

     (when (and (aff-flagged ch +aff-inflight+)
                (< (position-of ch) +pos-flying+)
                (eql (terrain-of (in-room-of ch)) +sect-water-noswim+))
       (do-fly ch)))
    (t
     (when (plusp (breath-count-of ch))
       (send-to-char ch "You breath in the sweet air.~%"))
     (setf (breath-count-of ch) 0)))


  (when (is-dead ch) (return-from update-creature))

  ;; Sleeping gas
  (when (and (room-flagged (in-room-of ch) +room-sleep-gas+)
             (needs-to-breathe ch)
             (> (position-of ch) +pos-sleeping+)
             (not (pref-flagged ch +pref-nohassle+)))
    (cond
      ((aff-flagged ch +aff-adrenaline+)
       (send-to-char ch "You feel strangely sleepy, but you easily fight it off.~%"))
      ((mag-savingthrow ch 50 +saving-chem+)
       (send-to-char ch "You feel strangely sleepy, but you fight off the affects.~%"))
      (t
       (act ch
            :subject-emit "You suddenly feel very sleepy and collapse where you stand."
            :place-emit "$n suddenly falls asleep and collapses!")
       (setf (position-of ch) +pos-sleeping+)
       (wait-state ch (rl-sec 4)))))

  ;; Poison gas
  (when (and (room-flagged (in-room-of ch) +room-poison-gas+)
             (needs-to-breathe ch)
             (not (pref-flagged ch +pref-nohassle+)))
    (unless (has-poison-3 ch)
      (act ch
           :subject-emit "Your lungs burn as you inhale a poisonous gas!"
           :place-emit "$n begins choking and sputtering!"))
    (cond
      ((has-poison-2 ch)
       (call-magic nil ch nil nil +spell-poison+ 60 +cast-chem+))
      ((has-poison-1 ch)
       (call-magic nil ch nil nil +spell-poison+ 39 +cast-chem+))
      ((has-poison-3 ch)
       (call-magic nil ch nil nil +spell-poison+ 10 +cast-chem+))))

  (when (is-dead ch) (return-from update-creature))

  ;; Decaying animated corpses
  (when (and (is-npc ch)
             (= (vnum-of ch) +zombie-vnum+)
             (room-is-sunny (in-room-of ch)))
    (damage-creature nil ch (dice 4 5) nil +spell-ablaze+ +wear-random+))

  (when (is-dead ch) (return-from update-creature))

  ;; Hunting mobiles
  (when (and (hunting-of ch)
             (not (aff-flagged ch +aff-blind+))
             (> (position-of ch) +pos-sitting+)
             (zerop (wait-state-of ch)))
    (cond
      ((or (< (hitp-of ch) (min 500 (* (max-hitp-of ch) 0.80)))
           (> (floor (- 100 (* (hitp-of ch) 100)) (max-hitp-of ch))
              (+ (morale-of ch) (random-range -5 (+ 10 (floor (int-of ch) 4))))))
       (when (eql (in-room-of ch) (in-room-of (hunting-of ch)))
         (perform-flee ch)))
      (t
       (hunt-victim ch)))))

(defun breath-threshold (ch)
  (+ (floor (level-of ch) 32) 2))

(defun update-creatures ()
  (dolist (ch (copy-list *characters*))
    (unless (eql (position-of ch) +pos-dead+)
      (with-simple-restart (continue "Continue from signal in creature updates")
        (if *break-on-error*
            (update-creature ch)
            (handler-bind ((error (lambda (str)
                                    (errlog "System error: ~a" str))))
              (update-creature ch)))))))

(defmacro while-alive ((ch-expr) &body body)
  (let ((ch-sym (gensym "CH-")))
    `(let ((,ch-sym ,ch-expr))
       (block while-alive
         ,@(loop for form in body
              append (list form
                           `(when (is-dead ,ch-sym) (return-from while-alive))))))))

(defun single-mobile-activity (ch)
  ;; Utility mobs don't do anything
  (when (and (is-npc ch) (mob-flagged ch +mob-utility+))
    (return-from single-mobile-activity))

  (let ((cur-class (if (and (is-remort ch) (zerop (random 3)))
                       (remort-char-class-of ch)
                       (char-class-of ch))))

    ;; Second level of poisoning
    (when (and (has-poison-2 ch)
               (not (immortalp ch)))
      (let ((damager (let ((af (affected-by-spell ch +spell-poison+)))
                       (when af
                         (get-char-in-world-by-idnum (owner-of af))))))
        (damage-creature damager ch (+ (dice 4 3)
                                       (if (affected-by-spell ch +spell-metabolism+)
                                           (dice 4 11) 0))
                         nil +spell-poison+ +wear-random+)))
    ;; Bleed
    (when (and (char-has-blood ch)
               (< (hitp-of ch) (+ (floor (max-hitp-of ch) 8)
                                  (random (max 1 (floor (max-hitp-of ch) 16))))))
      (add-blood-to-room (in-room-of ch) 1))
    ;; Zen of Motion Effect
    (when (and (is-neutral ch)
               (affected-by-spell ch +zen-motion+))
      (setf (move-of ch) (min (max-move-of ch)
                              (+ (move-of ch)
                                 (random (max 1
                                              (floor (check-skill ch +zen-motion+)
                                                     8)))))))
    ;; Deplete scuba tanks
    (let* ((mask (aref (equipment-of ch) +wear-face+))
           (tank (and mask (aux-obj-of mask))))
      (when (and mask
                 (is-obj-kind mask +item-scuba-mask+)
                 (not (car-closed mask))
                 tank
                 (is-obj-kind tank +item-scuba-tank+)
                 (plusp (obj-val-of tank 1))
                 (plusp (obj-val-of tank 0)))
        (decf (obj-val-of tank 1))
        (cond
          ((zerop (obj-val-of tank 1))
           (act ch :item tank
                :subject-emit "A warning indicator reads: $p fully depleted."))
          ((= (obj-val-of tank 1) 5)
           (act ch :item tank
                :subject-emit "A warning indicator reads: $p air level low.")))))

    (cond
      ((or (fighting-of ch) (aff2-flagged ch +aff2-petrified+))
       ;; nothing below this affects fighting or petrified characters
       nil)

      ((and (is-neutral ch)
            (eql (position-of ch) +pos-sitting+)
            (aff2-flagged ch +aff2-meditate+))
       ;; Meditate
       (perform-monk-meditate ch))

      ((and (is-npc ch)
            (not (mob2-flagged ch +mob2-mount+))
            (not (aff-flagged ch +aff-sleep+))
            (< (wait-of ch) 30)
            (>= (position-of ch) +pos-sleeping+)
            (< (position-of ch) (default-pos-of (shared-of ch)))
            (or (<= (default-pos-of (shared-of ch)) +pos-standing+)
                (< (position-of ch) +pos-standing+)))
       ;; Check if we've gotten knocked down
       (cond
         ((= (default-pos-of (shared-of ch)) +pos-sitting+)
          (act ch :all-emit "$n sit$% up.")
          (setf (position-of ch) +pos-sitting+))
         ((or (aff3-flagged ch +aff3-gravity-well+)
              (< (random-range 1 20) (str-of ch)))
          (act ch :all-emit "$n stand$% up.")
          (setf (position-of ch) +pos-standing+))))

      ((or (not (awakep ch)) (plusp (wait-of ch)))
       ;; Nothing below this affects characters who are asleep or in a wait state
       nil)

      ;; Barbs go BERSERK
      ((and (not (immortalp ch))
            (aff2-flagged ch +aff2-berserk+)
            (not (room-flagged (in-room-of ch)
                               +room-peaceful+)))
       (perform-barb-berserk ch))

      ;; Drunk affects
      ((and (> (get-condition ch +drunk+) (con-of ch))
            (randomly-true 10))
       (act ch :all-emit "$n burp$% loudly."))
      ((and (> (get-condition ch +drunk+) (floor (con-of ch) 2))
            (randomly-true 10))
       (act ch :all-emit "$n hiccup$%."))

      ((or (is-pc ch) (link-of ch))
       ;; Nothing below this affects PCs
       nil)

      ((and (room-flagged (in-room-of ch) +room-holyocean+)
            (is-evil ch)
            (< (position-of ch) +pos-flying+))
       ;; Attempt to flee from oceans of holy water
       (perform-flee ch))

      ((and (room-flagged (in-room-of ch)
                          +room-ice-cold+)
            (not (char-withstands-cold ch))
            (can-cast-spell ch ch +spell-endure-cold+ (spell-mana-cost ch +spell-endure-cold+)))
       ;; Attempt to keep from freezing
       (cast-spell ch ch nil 0 +spell-endure-cold+))

      ((and (aff2-flagged ch +aff2-ablaze+)
            (not (char-withstands-fire ch)))
       ;; Attempt to extinguish the flames if on fire
       (if (can-cast-spell ch ch +spell-prot-from-fire+ (spell-mana-cost ch +spell-prot-from-fire+))
           (cast-spell ch ch nil 0 +spell-prot-from-fire+)
           (perform-extinguish ch ch)))

      ((and (not (aff-flagged ch +aff-hide+))
            (aff-flagged (proto-of (shared-of ch)) +aff-hide+))
       ;; Attempt to re-hide
       (perform-hide ch))

      ((and (randomly-true 50) (rest (people-of (in-room-of ch))))
       ;; Mobiles looking at people
       (let ((target (random-elt (remove-if-not (lambda (tch)
                                                  (and (not (eql tch ch))
                                                       (is-visible-to tch ch)))
                                                (people-of (in-room-of ch))))))
         (when target
           (cond
             ((eql cur-class +class-predator+)
              (act ch :target target :all-emit "$n growl$% at $N."))
             ((or (and (is-evil ch) (is-good target))
                  (and (is-good ch) (is-evil target)))
              (if (< (level-of ch) (- 10 (level-of target)))
                  (act ch :target target :all-emit "$n look$% warily at $N.")
                  (act ch :target target :all-emit "$n growl$% at $N.")))
             ((eql cur-class +class-thief+)
              (act ch :target target :all-emit "$n glance$% sidelong at $N."))
             ((and (or (and (eql (sex-of ch) 'male) (eql (sex-of target) 'female))
                       (and (eql (sex-of ch) 'female) (eql (sex-of target) 'male)))
                   (randomly-true 4))
              (act ch :target target :all-emit "$n stare$% dreamily at $N."))
             (t
              (act ch :target target :all-emit "$n look$% at $N."))))))

      ;; Scavenger (picking up objects)
      ((and (mob-flagged ch +mob-scavenger+)
            (contents-of (in-room-of ch)))
       ;; pick up most valuable item in room
       (let ((obj (first (sort (remove-if-not (lambda (obj)
                                                (and (is-visible-to obj ch)
                                                     (can-take-obj ch obj t nil)))
                                              (contents-of (in-room-of ch)))
                               #'> :key 'cost-of))))
         (when obj
           (get-from-room ch (list obj) ""))))

      ;; Drink from fountains
      ((and (contents-of (in-room-of ch))
            (randomly-true 100)
            (not (or (is-undead ch)
                     (is-dragon ch)
                     (is-golem ch)
                     (is-elemental ch))))
       (let ((obj (random-elt (remove-if-not (lambda (obj)
                                               (and (is-visible-to obj ch)
                                                    (is-obj-kind obj +item-fountain+)
                                                    (plusp (aref (value-of obj) 1))))
                                             (contents-of (in-room-of ch))))))
         (when obj
           (act ch :item obj :all-emit "$n drinks from $p."))))

      ((and (not (mob-flagged ch +mob-sentinel+))
            (not (and (or (mob-flagged ch +mob-pet+)
                          (mob2-flagged ch +mob2-familiar+))
                      (master-of ch)))
            (>= (position-of ch) +pos-standing+)
            (not (aff2-flagged ch +aff2-mounted+)))

       ;; Mob movement
       (let* ((door (random-range 0 (if (or (is-tarrasque ch)
                                            (> (length (people-of (in-room-of ch))) 10))
                                        (1- +num-of-dirs+)
                                        20)))
              (other-room (when (and (< door +num-of-dirs+)
                                     (exit ch door))
                            (real-room (to-room-of (exit ch door))))))
         (when (and other-room
                    (can-go ch door)
                    (not (eql other-room (in-room-of ch)))
                    (not (room-flagged other-room +room-nomob+))
                    (not (room-flagged other-room +room-death+))
                    (not (logtest (exit-info-of (exit ch door))
                                  +ex-nomob+))
                    (char-likes-room ch other-room)
                    (or (not (mob2-flagged ch +mob2-stay-sect+))
                        (eql (terrain-of (in-room-of ch))
                             (terrain-of other-room)))
                    (or (not (mob-flagged ch +mob-stay-zone+))
                        (eql (zone-of (in-room-of ch))
                             (zone-of other-room)))
                    (< (length (people-of other-room)) 10))
           (perform-move ch door :norm t)))))))

(defun mobile-activity ()
  (dolist (ch (copy-list *characters*))
    (unless (or (eql (position-of ch) +pos-dead+)
                (is-pc ch))
      (with-simple-restart (continue "Continue from signal in mobile activity")
        (if *production-mode*
            (handler-bind ((error (lambda (str)
                                    (errlog "System error: ~a" str))))
              (single-mobile-activity ch))
            (single-mobile-activity ch))))))

(defun perform-monk-meditate (ch)
  nil)

(defun mobile-battle-activity (ch)
  nil)
