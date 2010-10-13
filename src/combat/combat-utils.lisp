(in-package #:tempus)

(defun apply-soil-to-char (ch obj kind pos)
  (when (eql pos +wear-random+)
    (setf pos (random-elt (loop
                             for idx upto +num-wears+
                             when (and (not (illegal-soilpos pos))
                                       (not (and (null (get-eq ch idx)) (char-soiled ch idx kind)))
                                       (not (and (get-eq ch idx) (obj-soiled (get-eq ch idx) kind))))
                             collect idx))))

  (unless (illegal-soilpos pos)
    (let ((eq (get-eq ch pos)))
      (cond
        ((and eq
              (or (eql eq obj) (null obj))
              (not (is-obj-stat2 eq +item2-nosoil+))
              (not (obj-soiled eq kind)))
         (setf (ldb (byte kind 1) (soilage-of eq)) 1))
        ((not (char-soiled ch pos kind))
         (setf (ldb (byte kind 1) (soilage-of ch)) 1)))))

  (when (and (eql kind +soil-blood+)
             obj
             (eql (vnum-of obj) +blood-vnum+))
    (incf (timer-of obj) (max 1 (- (timer-of obj) 5))))

  pos)

(defparameter +corpse-descs+
  '(("The bruised up ~a of ~a ~a lying here." "bruised" +type-hit+ +skill-bash+ +skill-pistolwhip+)
    ("The bloody, swollen ~a of ~a ~a lying here." "stung" +type-sting+)
    ("The scarred ~a of ~a ~a lying here." "scarred" +type-whip+)
    ("The chopped up ~a of ~a ~a lying here." "chopped up" +type-slash+ +type-chop+ +spell-blade-barrier+)
    ("The perforated ~a of ~a ~a lying here." "perforated" +song-wounding-whispers+)
    ("The legless ~a of ~a ~a lying here." "legless" +skill-hamstring+)
    ("The chewed up looking ~a of ~a ~a lying here." "chewed up" +skill-bite+ +type-bite+)
    ("The sniped ~a of ~a ~a lying here." "sniped" +skill-snipe+)
    ("The battered ~a of ~a ~a lying here." "battered" +type-bludgeon+ +type-pound+ +type-punch+)
    ("The crushed ~a of ~a ~a lying here." "crushed" +type-crush+ +spell-psychic-crush+)
    ("The shredded ~a of ~a ~a lying here." "shredded" +type-claw+ +skill-claw+)
    ("The mauled ~a of ~a ~a lying here." "mauled" +type-maul+)
    ("The ~a of ~a ~a lying here, badly thrashed." "thrashed" +type-thrash+)
    ("The backstabbed ~a of ~a ~a lying here." "backstabbed" +skill-backstab+)
    ("The bloody ~a of ~a ~a lying here, full of holes." +type-pierce+ +type-stab+ +type-egun-particle+)
    ("The gored ~a of ~a ~a lying here in a pool of blood." "gored" +type-gore-horns+)
    ("The trampled ~a of ~a ~a lying here." "trampled" +type-trampling+)
    ("The lashed ~a of ~a ~a lying here." "lashed" +type-tail-lash+)
    ("A bloody pile of bones is lying here." "digested" +type-swallow+)
    ("The blasted ~a of ~a ~a lying here." "blasted" +type-blast+ +spell-magic-missile+ +skill-energy-weapons+ +spell-symbol-of-pain+ +spell-disruption+ +spell-prismatic-spray+ +skill-discharge+ +type-egun-laser+)
    ("The shot up ~a of ~a ~a lying here." "shot up" +skill-proj-weapons+)
    ("The pierced ~a of ~a ~a lying here." "pierced" +skill-archery+)
    ("The charred ~a of ~a ~a lying here." "charred" +spell-burning-hands+ +spell-call-lightning+ +spell-fireball+ +spell-flame-strike+ +spell-lightning-bolt+ +spell-microwave+ +spell-fire-breath+ +spell-lightning-breath+ +spell-fire-elemental+ +type-ablaze+ +spell-meteor-storm+ +spell-fire-shield+ +spell-hell-fire+ +type-flamethrower+ +spell-electric-arc+ +type-egun-plasma+)
    ("The smoking ~a of ~a ~a lying here." "smoking" +skill-energy-field+ +skill-self-destruct+ +type-egun-ion+)
    ("The scorched ~a of ~a ~a lying here." "scorched" +type-boiling-pitch+)
    ("The scalded ~a of ~a ~a lying here." "scalded" +spell-steam-breath+)
    ("The ~a of ~a ~a lying here, blasted and smoking." "blasted" +javelin-of-lightning+ +type-egun-lightning+)
    ("The frozen ~a of ~a ~a lying here." "frozen" +spell-cone-cold+ +spell-chill-touch+ +type-freezing+ +spell-hell-frost+ +spell-frost-breath+)
    ("The smashed ~a of ~a ~a lying here." "smashed" +spell-spirit-hammer+ +spell-earth-elemental+)
    ("The ripped apart ~a of ~a ~a lying here." "ripped apart" +spell-air-elemental+ +type-rip+)
    ("The drenched ~a of ~a ~a lying here." "drenched" +spell-water-elemental+)
    ("The radioactive ~a of ~a ~a lying here." "radioactive" +spell-gamma-ray+ +spell-halflife+ +type-egun-gamma+)
    ("The sizzling ~a of ~a ~a lying here, dripping acid." "sizzling" +spell-acidity+)
    ("The ~a of ~a lies here, stinking of chlorine gas." "chlorinated" +spell-gas-breath+)
    ("The burned up ~a of ~a ~a lying here, finally still." "burned" +skill-turn+)
    ("The splattered ~a of ~a ~a lying here." "splattered" +type-falling+)
    ("The shattered, twisted ~a of ~a ~a lying here." "shattered" +skill-piledrive+)
    ("The smoking ~a of ~a ~a lying here." "smoking" +spell-taint+ +type-taint-burn+)
    ("The headless ~a of ~a ~a lying here." "headless" +skill-behead+ +skill-pele-kick+ +skill-clothesline+)
    ("The maimed ~a of ~a ~a lying here." "maimed" +skill-lunge-punch+)
    ("The run through ~a of ~a ~a lying here." "impaled" +skill-impale+)
    ("The waterlogged ~a of ~a ~a lying here." "drowned" +type-drowning+)))

(defun generate-corpse-desc (ch attacktype)
  (let ((isare "is")
        (corpse-name "corpse")
        (adj nil)
        (aliases "")
        (ldesc "The ~a of ~a ~a"))

    (when (or (is-robot ch)
              (is-plant ch)
              (= attacktype +type-falling+))
      (setf isare "are")
      (setf corpse-name "remains")
      (setf aliases "remains"))

    (when (= attacktype +type-swallow+)
      (setf corpse-name "bones")
      (setf aliases "bones"))

    (when (aff2-flagged ch +aff2-petrified+)
      (setf aliases (concatenate 'string aliases " stone"))
      (setf corpse-name (concatenate 'string "stone " corpse-name)))

    (when (and (search "headless" (name-of ch))
               (or (= attacktype +skill-behead+)
                   (= attacktype +skill-pele-kick+)
                   (= attacktype +skill-clothesline+)))
      (setf attacktype +type-hit+))

    (let ((special-corpse (find-if (lambda (rec)
                                     (member attacktype (mapcar #'symbol-value (cddr rec))))
                                   +corpse-descs+)))
      (when special-corpse
         (setf ldesc (format nil (first special-corpse)
                             corpse-name
                             (name-of ch)
                             isare))
         (setf adj (second special-corpse))))

    (values (format nil "the ~a ~a of ~a"
                    adj
                    corpse-name
                    (name-of ch))
            (format nil "~(~a ~a ~a~)" corpse-name adj (aliases-of ch))
            ldesc)))

(defun make-corpse (ch killer attacktype)
  (multiple-value-bind (name aliases ldesc)
      (generate-corpse-desc ch attacktype)
    (let ((corpse (make-object :unknown 0
                               :kind +item-container+
                               :name name
                               :aliases aliases
                               :line-desc ldesc
                               :wear-flags +item-wear-take+
                               :extra-flags +item-nodonate+
                               :weight (weight-of ch)
                               :max-dam (if (is-npc ch) 100 -1)
                               :damage (if (is-npc ch) 100 -1)
                               :timer 5
                               :value0 0
                               :value1 2
                               :value2 (if (is-npc ch)
                                           (- (vnum-of ch))
                                           (idnum-of ch))
                               :value3 (cond
                                         ((null killer)   0)
                                         ((is-npc killer) (- (vnum-of killer)))
                                         (t               (idnum-of killer)))
                               :material (cond
                                           ((aff2-flagged ch +aff2-petrified+)
                                            +mat-stone+)
                                           ((is-robot ch)
                                            +mat-metal+)
                                           ((is-skeleton ch)
                                            +mat-bone+)
                                           ((is-pudding ch)
                                            +mat-pudding+)
                                           ((is-slime ch)
                                            +mat-slime+)
                                           ((is-plant ch)
                                            +mat-vegetable+)
                                           (t
                                            +mat-flesh+)))))
      (push corpse *object-list*)
      (obj-to-room corpse (in-room-of ch))
      corpse)))

(defun update-pos (victim)
  (cond
    ((<= (hitp-of victim) -11)
     (setf (position-of victim) +pos-dead+))
    ((<= (hitp-of victim) -6)
     (setf (position-of victim) +pos-mortallyw+))
    ((<= (hitp-of victim) -3)
     (setf (position-of victim) +pos-incap+))
    ((<= (hitp-of victim) 0)
     (setf (position-of victim) +pos-stunned+))
    ((= (position-of victim) +pos-sleeping+)
     ;; wake them up from their nap
     (setf (position-of victim) +pos-resting+))
    ((and (or (= (position-of victim) +pos-standing+)
              (= (position-of victim) +pos-flying+))
          (fighting-of victim))
     ;; if everything is normal and they're fighting, set them fighting
     (setf (position-of victim) +pos-fighting+))
    ((and (> (position-of victim) +pos-stunned+)
          (< (position-of victim) +pos-fighting+)
          (fighting-of victim))
     ;; if they're alive, not stunned, in a fight, and not +pos-fighting+
     (when (and (is-npc victim) (zerop (wait-state-of victim)))
       (cond
         ((< (position-of victim) +pos-fighting+)
          (when (or (not (aff3-flagged victim +aff3-gravity-well+))
                    (< (random-range 1 20) (str-of victim)))
            (setf (position-of victim) +pos-fighting+)
            (when (= (position-of victim) +pos-fighting+)
              (act victim :place-emit "$n scrambles to $s feet!")))
          (wait-state victim +pulse-violence+))
         (t
          (setf (position-of victim) +pos-fighting+)))))
    ((is-pc victim)
     ;; handle players or waiting mobs
     (when (= (position-of victim) +pos-stunned+)
       ;; wear off being stunned
       (setf (position-of victim) +pos-resting+)))
    ((and (in-room-of victim)
          (room-is-open-air (in-room-of victim))
          (not (aff3-flagged victim +aff3-gravity-well+))
          (/= (position-of victim) +pos-flying+))
     ;; set flying if in open air
     (setf (position-of victim) +pos-flying+))
    ((and (aff3-flagged victim +aff3-gravity-well+)
          (>= (random-range 1 20) (str-of victim)))
     ;; overcome by gravity well
     nil)
    ((and (< (position-of victim) +pos-fighting+) (fighting-of victim))
     ;; getting up while fighting
     (act victim :place-emit "$n scrambles to $s feet!")
     (setf (position-of victim) +pos-fighting+)
     (wait-state victim +pulse-violence+))
    ((< (position-of victim) +pos-fighting+)
     ;; getting up normally
     (act victim :place-emit "$n stands up.")
     (setf (position-of victim) +pos-standing+)
     (wait-state victim +pulse-violence+))))

(defun char-class-race-hit-bonus (ch victim)
  (+
   ;; Height modifiers
   (if (and (is-dwarf ch)
              (or (is-ogre victim)
                  (is-troll victim)
                  (is-giant victim)
                  (> (height-of victim)
                     (* 2 (height-of ch)))))
         1 0)
   ;; Dwarven dislike of water or heights
   (if (and (is-dwarf ch)
            (or (room-is-watery (in-room-of ch))
                (room-is-open-air (in-room-of ch))))
       -1 0)
   ;; Thieves operating in the dark
   (if (and (is-thief ch)
            (room-is-dark (in-room-of ch)))
       1 0)
   ;; Rangers like being outside
   (if (and (is-ranger ch)
            (or (eql (terrain-of (in-room-of ch)) +sect-forest+)
                (and (not (eql (terrain-of (in-room-of ch)) +sect-city+))
                     (not (eql (terrain-of (in-room-of ch)) +sect-inside+))
                     (outsidep ch))))
       1 0)
   ;; Tabaxi in their native habitat
   (if (and (is-tabaxi ch)
            (eql (terrain-of (in-room-of ch))
                 +sect-forest+))
       1 0)))

(defun calculate-thaco (ch victim weapon)
  (let ((thaco (min (thaco (char-class-of ch) (level-of ch))
                    (thaco (remort-char-class-of ch) (level-of ch)))))
    (if (and weapon (is-energy-gun weapon))
        (decf thaco (getf (aref +dex-app+ (dex-of ch)) :tohit))
        (decf thaco (getf (aref +str-app+ (str-of ch)) :tohit)))

    (cond
      ((<= (hitroll-of ch)
           5)
       (decf thaco (hitroll-of ch)))
      ((<= (hitroll-of ch)
           50)
       (decf thaco (+ 5 (floor (- (hitroll-of ch)
                                  5)
                               3))))
      (t
       (decf thaco 20)))

    (when (awakep victim)
      (decf thaco (getf (aref +dex-app+ (dex-of victim)) :defensive)))

    (when (is-drow ch)
      (when (room-is-sunny (in-room-of ch))
        (incf thaco 10))
      (when (room-is-dark (in-room-of ch))
        (decf thaco 5)))

    (when weapon
      (let ((spec (assoc (vnum-of weapon) (weap-spec-of ch))))
        (when spec
          (decf (second spec))))

      (when (and (is-evil victim) (is-obj-stat weapon +item-bless+))
        (decf thaco 1))
      (when (and (is-good victim) (is-obj-stat weapon +item-damned+))
        (decf thaco 1)))

    (let ((weapon-weight (weight-of weapon))
          (wield-weight (getf (aref +str-app+ (str-of ch)) :wield-w)))
      (when (> weapon-weight wield-weight)
        (incf thaco 2))
      (when (and (is-mage ch)
                 (> weapon-weight
                    (+ (floor (* (level-of ch)
                                 wield-weight)
                              100)
                       (floor wield-weight 2))))
        (incf thaco (floor weapon-weight 4)))
      (when (and (is-thief ch)
                 (> weapon-weight (+ 12 (floor (str-of ch)
                                               4))))
        (incf thaco (floor weapon-weight 8)))

      (when (is-barb ch)
        (incf thaco (floor (- (learned ch)
                              (weapon-proficiency ch weapon))
                           8)))

      (when (is-energy-gun weapon)
        (incf thaco (floor (- (learned ch)
                              (skill-of ch +skill-energy-weapons+))
                           8))
        (when (< (skill-of ch +skill-shoot+) 80)
          (incf thaco (floor (- (learned ch)
                                (skill-of ch +skill-shoot+))
                             20))))

      (when (and (get-eq ch +wear-wield-2+)
                 (< (skill-of ch +skill-second-weapon+)
                    (learned ch))
                 (not (affected-by-spell ch +skill-neural-bridging+)))
        (decf thaco (floor (- (learned ch)
                              (skill-of ch +skill-second-weapon+))
                           (if (eql weapon (get-eq ch +wear-wield-2+))
                               5
                               10))))

      (when (or (and (is-evil ch) (aff-flagged victim +aff-protect-evil+))
                (and (is-good ch) (aff-flagged victim +aff-protect-good+))
                (and (is-undead ch) (aff2-flagged victim +aff2-protect-undead+)))
        (incf thaco 2))

      (when (> (carry-items-of ch)
               (* (can-carry-items ch) 8/10))
        (incf thaco 1))

      (incf thaco (if (plusp (can-carry-weight ch))
                      (floor (* (+ (carry-weight-of ch) (worn-weight-of ch)) 2)
                             (can-carry-weight ch))
                      10))
      (when (and (aff2-flagged ch +aff2-displacement+)
                 (not (aff2-flagged victim +aff2-true-seeing+)))
        (decf thaco 2))
      (when (aff-flagged ch +aff-blur+)
        (decf thaco 1))
      (unless (can-see-creature victim ch)
        (decf thaco 3))

      (unless (can-see-creature ch victim)
        (incf thaco 3))
      (when (plusp (get-condition ch +drunk+))
        (incf thaco 2))
      (when (is-sick ch)
        (incf thaco 2))
      (when (and (aff2-flagged victim +aff2-displacement+)
                 (not (aff2-flagged ch +aff2-true-seeing+)))
        (incf thaco 2))
      (when (aff2-flagged victim +aff2-evade+)
        (incf thaco (floor (get-skill-bonus victim +skill-evasion+) 6)))
      (when (and (is-pc ch)
                 (room-is-watery (in-room-of ch)))
        (incf thaco 4))

      (decf thaco (pin (- +pos-fighting+ (position-of victim))
                       0 5))
      (decf thaco (char-class-race-hit-bonus ch victim))

      thaco)))
