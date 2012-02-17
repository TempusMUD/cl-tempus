(in-package #:tempus)

(defvar *spell-info* (make-array 1000 :initial-element nil))

(defun spell-to-str (idnum)
  (name-of (aref *spell-info* idnum)))
(defun str-to-spell (str)
  (find str *spell-info* :test #'string-equal :key #'name-of))

(defun spell-flagged (spellnum flag)
  (logtest (routines-of (aref *spell-info* spellnum)) flag))

(defmacro define-spell-predicate (name flag)
  `(defun ,name (spell)
     (and (<= spell +top-spell-define+)
          (logtest (routines-of (aref *spell-info* spell))
                   ,flag))))

(define-spell-predicate spell-is-psionic +mag-psionic+)
(define-spell-predicate spell-is-physics +mag-physics+)
(define-spell-predicate spell-is-bardic +mag-bard+)
(define-spell-predicate spell-is-magic +mag-magic+)
(define-spell-predicate spell-is-mercenary +mag-mercenary+)
(define-spell-predicate spell-is-divine +mag-divine+)
(define-spell-predicate spell-is-good +mag-good+)
(define-spell-predicate spell-is-evil +mag-evil+)
(defun spell-is-unpleasant (spell)
  (and (<= spell +top-spell-define+)
       (logtest (targets-of (aref *spell-info* spell))
                +tar-unpleasant+)))

(defun spell-gen (spell class)
  (aref (min-gen-of (aref *spell-info* spell)) class))

(defun spell-level (spell class)
  (aref (min-level-of (aref *spell-info* spell)) class))

(defun calc-able-to-learn (skill char-class remort-char-class level gen)
  (let ((info (aref *spell-info* skill)))
    (or (and (>= level (aref (min-level-of info) char-class))
             (>= gen (aref (min-gen-of info) char-class))))
        (and (plusp gen)
             (>= level (aref (min-level-of info) remort-char-class)))))

(defun able-to-learn (ch skill)
  (calc-able-to-learn skill
                      (char-class-of ch)
                      (remort-char-class-of ch)
                      (level-of ch)
                      (remort-gen-of ch)))

(defun load-corpse-owner (obj)
  (if (minusp (corpse-idnum obj))
      (real-mobile-proto (- (corpse-idnum obj)))
      (load-player-from-xml (corpse-idnum obj))))

(defun spell-add-affect (spell caster target duration level saved location modifier)
  (when (or (not (spell-is-unpleasant spell))
            (not (violentp spell))
            saved)
    (affect-to-char target
                    (make-instance 'affected-type
                                   :kind spell
                                   :owner (idnum-of caster)
                                   :duration duration
                                   :level level
                                   :modifier modifier
                                   :location location))))

(defun spell-set-affbit (spell caster target duration level saved idx bit)
  (when (or (not (spell-is-unpleasant spell))
            (not (violentp spell))
            saved)
  (affect-to-char target
                  (make-instance 'affected-type
                                 :kind spell
                                 :owner (idnum-of caster)
                                 :duration duration
                                 :level level
                                 :aff-index idx
                                 :bitvector bit))))

(defun spell-damage (spell caster target saved amt kind)
  (when (>= (check-skill caster spell) 50)
    (when (> (check-skill caster spell) 100)
      (incf amt (floor (* amt (- (check-skill caster spell) 100)) 100)))
    (cond
      ;; int bonus for mages
      ((and caster
            (spell-is-magic spell)
            (is-mage caster))
       (incf amt (floor (* amt (- (int-of caster) 10)) 45)))
      ;; wis bonus for clerics
      ((and caster
            (spell-is-divine spell)
            (is-cleric caster))
       (incf amt (floor (* amt (- (wis-of caster) 10)) 45)))
      ;; cha bonus for bards
      ((and caster
            (spell-is-bardic spell)
            (is-bard caster))
       (incf amt (floor (* amt (- (cha-of caster) 10)) 45))
       ;; fortissimo makes bard songs more powerful
       (let ((af (affected-by-spell caster +song-fortissimo+)))
         (incf amt (floor (* amt (level-of af)) 100))))))

  ;; Divine attacks modified by caster alignment
  (when (and (spell-is-divine spell)
             caster)
    (cond
      ((is-good caster)
       (setf amt (floor (* amt 3) 4)))
      ((is-evil caster)
       (incf amt (floor (* amt (abs (alignment-of caster)))
                        4000))
       (when (is-soulless caster)
         (incf amt (floor amt 4)))
       (when (is-good target)
         (incf amt (floor (* amt (abs (alignment-of target))) 4000))))))

  (when saved
    (setf amt (floor amt 2)))

  (damage-creature caster target amt nil kind nil))

(defun spell-restore-hitp (spell caster target amt)
  (cond
    ((affected-by-spell target +spell-blackmantle+)
     (send-to-char target "Your blackmantle absorbs the healing!~%")
     0)
    (t
     ;; divine healing spells affected by alignment
     (when (and caster
                (plusp amt)
                (spell-is-divine spell))
       (incf amt (floor (* amt (abs (alignment-of caster))) 3000))
       (when (is-evil caster)
         (setf amt (floor amt 2))))

     (setf amt (min (- (max-hitp-of target) (hitp-of target)) amt))

     (incf (hitp-of target) amt)
     amt)))

(defun spell-restore-mana (target amt)
  (setf amt (min (- (max-mana-of target) (mana-of target)) amt))
  (incf (mana-of target) amt))

(defun spell-restore-move (target amt)
  (setf amt (min (- (max-move-of target) (move-of target)) amt))
  (incf (move-of target) amt))

(defun spell-alter-align (target amt)
  (setf (alignment-of target) (pin (+ (alignment-of target) amt) -1000 1000)))

(defun spell-extinguish (target)
  (extinguish target)
  (act target
       :subject-emit "The flames on your body sizzle out and die."
       :place-emit "The flames on $n's body sizzle out and die."))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun build-ability-macro (kind name body)
    (let ((func-name (intern (format nil "~a-~a" kind name)))
          (id-name (intern (format nil "+~a-~a+" kind name)))
          (body-syms (alexandria:flatten body)))
      `(progn
         (defun ,func-name (caster target level saved)
           (declare (ignorable caster target level saved))
           (let (,@(when (or (member 'affect body-syms)
                             (member 'set-affbit body-syms))
                         `((duration (floor level 4))))
                 ,@(when (member 'to-caster body-syms) `((caster-emit nil)))
                 ,@(when (member 'to-target body-syms) `((target-emit nil)))
                 ,@(when (member 'to-room body-syms) `((room-emit nil))))
             (macrolet ((duration (new-duration)
                          `(setf duration ,new-duration))
                        (affect (&optional (location +apply-none+) (modifier 0))
                          `(spell-add-affect ,,id-name caster target duration level saved ,location ,modifier))
                        (set-affbit (idx bit)
                          `(spell-set-affbit ,,id-name caster target duration level saved ,idx ,bit))
                        (damage (amt &optional kind)
                          `(spell-damage ,,id-name caster target saved ,amt ,(or kind ,id-name)))
                        (restore-hitp (amt)
                          `(spell-restore-hitp ,,id-name caster target ,amt))
                        (restore-mana (amt)
                          `(spell-restore-mana target ,amt))
                        (restore-move (amt)
                          `(spell-restore-move target ,amt))
                        (alter-alignment (amt)
                          `(spell-alter-align target ,amt))
                        (extinguish-fire ()
                          `(spell-extinguish target))
                        (to-caster (str)
                          `(setf caster-emit ,str))
                        (to-target (str)
                          `(setf target-emit ,str))
                        (to-room (str)
                          `(setf room-emit ,str)))
               ,@body
               ,@(when (member 'to-caster body-syms)
                       `((when (and caster-emit caster)
                           (act target :target caster :target-emit caster-emit))))
               ,@(when (member 'to-target body-syms)
                       `((when target-emit
                           (act target :target caster :subject-emit target-emit))))
               ,@(when (member 'to-room body-syms)
                       `((when room-emit
                           (act target :target caster :place-emit room-emit)))))))
         (when (null (aref *spell-info* ,id-name))
           (setf (aref *spell-info* ,id-name) (make-instance 'spell-info)))
         (setf (func-of (aref *spell-info* ,id-name)) (function ,func-name))))))

(defmacro define-spell (name () &body body)
  (build-ability-macro "SPELL" name body))

(defmacro define-song (name () &body body)
  (build-ability-macro "SONG" name body))

(define-spell magic-missile ()
  (damage (+ 1
             (floor level 4)
             (min (floor level 5) 3))))

(define-spell chill-touch ()
  (duration 4)
  (affect +apply-str+ (- (1+ (floor level 16))))
  (damage (+ 1
             (floor level 2)
             (min (floor level 8) 3)))
  (to-target "You feel your strength wither!"))

(define-spell burning-hands ()
  (damage (+ (dice level 6)
             (floor level 2))))

(define-spell shocking-grasp ()
  (damage (+ (dice level 4)
             (floor level 2))))

(define-spell color-spray ()
  (damage (+ (dice level 8)
             (floor level 2))))

(define-spell lightning-bolt ()
  (damage (+ (dice level 10)
             (floor level 2))))

(define-spell fireball ()
  (damage (+ (dice level 10)
             (floor level 2))))

(define-spell cone-cold ()
  (extinguish-fire)
  (damage (+ (dice level 12)
             (floor level 2))))

(define-spell prismatic-spray ()
  (damage (+ (dice level 16)
             (floor level 2))))

(define-spell meteor-storm ()
  (damage (+ (dice level 10)
             (floor level 2))))

(define-spell hailstorm ()
  (extinguish-fire)
  (damage (+ (dice level 9) 10)))

(define-spell microwave ()
  (damage (+ (dice 9 8)
             (floor level 2))))

(define-spell oxidize ()
  (damage (+ (dice level 4)
             (* level 2))))

(define-spell electric-arc ()
  (damage (+ (dice (get-skill-bonus caster +spell-electric-arc+) 5)
             (* level 8))))

(define-spell disruption ()
  (damage (+ (dice level 6)
             (* level 2))))

(define-spell call-lightning ()
  (damage (+ (dice level 7)
             (floor level 2))))

(define-spell harm ()
  (let ((dam (+ (dice level 6)
                (* level 2))))
    (if (and (is-good caster) (is-good target))
        (damage (floor dam 2))
        (damage dam))))

(define-spell energy-drain ()
  (if (<= (level-of target) 2)
      (damage 100)
      (damage (dice level 10))))

(define-spell flame-strike ()
  (damage (+ (dice level 8)
             level)))

(define-spell spirit-hammer ()
  (damage (+ (dice level 4)
             level)))

(define-spell icy-blast ()
  (damage (+ (dice level 11)
             (floor level 4))))

(define-spell fire-breath ()
  (damage (+ (dice level 15) level)))

(define-spell frost-breath ()
  (damage (+ (dice level 7) level)))

(define-spell acid-breath ()
  (damage (+ (dice level 15) level)))

(define-spell lightning-breath ()
  (damage (+ (dice level 15) level)))

(define-spell shadow-breath ()
  (damage (+ (dice level 9) level)))

(define-spell steam-breath ()
  (damage (+ (dice level 15) level)))

(define-spell zhengis-fist-of-annihilation ()
  (damage (+ (dice level 25) level)))

(define-spell psychic-surge ()
  (unless (affected-by-spell caster +spell-psychic-surge+)
    (damage (+ (dice 3 7) (* 4 level)))
    (remove-all-combat target)
    (remove-combat caster target)
    (setf (position-of target) +pos-stunned+)
    (wait-state target (rl-sec 5))
    (duration 1)
    (affect)))

(define-spell ego-whip ()
  (damage (+ (dice 5 9) level))
  (when (and (> (position-of target) +pos-sitting+)
             (> (random-range 5 25)
                (dex-of target)))
    (to-target "You are knocked to the ground by the psychic attack!")
    (to-room "$n is knocked to the ground by the psychic attack!")
    (setf (position-of target) +pos-sitting+)
    (wait-state target (rl-sec 2))))

(define-spell earthquake ()
  (damage (+ (dice (floor level 2) 14) (* 2 level))))

(define-spell fission-blast ()
  (damage (+ (dice level 8) level)))

(define-song sonic-disruption ()
  (damage (+ (dice (floor level 2) 7) (* 3 level))))

(define-song dirge ()
  (damage (+ (dice (floor level 4) (floor level 4)) (* 4 level))))

(define-spell armor ()
  (duration 24)
  (affect +apply-ac+ (+ (floor level 4) 20))
  (to-target "You feel someone protecting you."))

(define-spell barkskin ()
  (duration (dice 4 (1+ (floor level 8))))
  (when (affected-by-spell target +spell-stoneskin+)
    (affect-from-char target +spell-stoneskin+))
  (when (affected-by-spell target +spell-thorn-skin+)
    (affect-from-char target +spell-thorn-skin+))
  (affect +apply-ac+ -10)
  (to-target "Your skin tightens up and hardens."))

(define-spell stoneskin ()
  (duration(dice 4 (1+ (floor level 8))))
  (when (affected-by-spell target +spell-barkskin+)
    (affect-from-char target +spell-stoneskin+))
  (when (affected-by-spell target +spell-thorn-skin+)
    (affect-from-char target +spell-thorn-skin+))
  (affect +apply-ac+ -10)
  (to-target "Your skin hardens to a rock-like shell.")
  (to-room "$n's skin turns a pale, rough grey."))

(define-spell thorn-skin ()
  (when (affected-by-spell target +spell-barkskin+)
    (affect-from-char target +spell-barkskin+))
  (when (affected-by-spell target +spell-thorn-skin+)
    (affect-from-char target +spell-thorn-skin+))
  (duration (dice 3 (1+ (floor level 4))))
  (affect +apply-ac+
          (- (+ 5 (floor level 10))))
  (to-target "Large thorns erupt from your skin!")
  (to-room "Large thorns erupt from $n's skin!")
  (damage-creature caster target (- 150 (floor level 2)) nil
                   +spell-thorn-skin+ +wear-random+))

(define-spell pray ()
  (duration (+ 4 (floor level 16)))
  (affect +apply-hitroll+ (+ 3 (floor level 8)))
  (affect +apply-saving-spell+ (- (+ 3 (floor level 16))))
  (if (is-good target)
    (to-target "You feel extremely righteous")
    (to-target "You feel a dark power enter your soul.")))

(define-spell blindness ()
  (cond
    ((mob-flagged target +mob-noblind+)
     (to-caster "You fail.~%"))
    (t
     (duration 2)
     (affect +apply-hitroll+ -4)
     (affect +apply-ac+ 40)
     (set-affbit 1 +aff-blind+)
     (to-target "You have been blinded!")
     (to-room "$n seems to be blinded!"))))

(define-spell breathe-water ()
  (duration (+ 10 level))
  (set-affbit 1 +aff-waterbreath+)
  (to-target "You are now able to breathe underwater."))

(define-spell spirit-track ()
  (duration level)
  (affect)
  (to-target "You can now sense trails to other creatures."))

(define-spell word-stun ()
  (cond
    ((mob2-flagged target +mob2-nostun+)
     (to-caster "You fail the stun.~%"))
    (t
     (remove-all-combat target)
     (setf (position-of target) +pos-stunned+)
     (wait-state target (* 2 +pulse-violence+))
     (duration 1)
     (affect +apply-int+ -1)
     (to-target "You have been stunned!")
     (to-room "$n suddenly looks stunned!"))))

(define-spell blur ()
  (duration (+ 2 (floor level 4)))
  (affect +apply-ac+ -10)
  (set-affbit 1 +aff-blur+)
  (to-target "Your image suddenly starts to blur and shift.")
  (to-room "The image of $n suddenly starts to blur and shift."))

(define-spell curse ()
  (duration (+ 1 (floor level 2)))
  (affect +apply-hitroll+ (- (+ 1 (floor level 8))))
  (affect +apply-damroll+ (- (+ 1 (floor level 8))))
  (set-affbit 1 +aff-curse+)
  (to-target "You feel very uncomfortable.")
  (to-room "$n briefly glows with a sick red light!"))

(define-spell detect-align ()
  (duration (+ 12 level))
  (set-affbit 1 +aff-detect-align+)
  (to-target "Your eyes tingle."))

(define-spell detect-invis ()
  (duration (+ 12 level))
  (set-affbit 1 +aff-detect-invis+)
  (to-target "Your eyes tingle."))

(define-spell detect-magic ()
  (duration (+ 12 level))
  (set-affbit 1 +aff-detect-magic+)
  (to-target "Your eyes tingle."))

(define-spell detect-poison ()
  (duration (+ 12 level))
  (set-affbit 2 +aff3-detect-poison+)
  (to-target "Your eyes tingle."))

(define-spell detect-scrying ()
  (duration level)
  (affect)
  (to-target "You are now aware."))

(define-spell displacement ()
  (duration (if (> level 48) 6 4))
  (set-affbit 2 +aff2-displacement+)
  (to-target "Your image will now be displaced from its actual position."))

(define-spell endure-cold ()
  (duration 24)
  (set-affbit 2 +aff2-endure-cold+)
  (to-target "You can now endure the coldest of cold."))

(define-spell fire-shield ()
  (duration (+ 6 (floor level 8)))
  (affect +apply-ac+ -8)
  (set-affbit 2 +aff2-fire-shield+)
  (to-target "A sheet of flame appears before your body.")
  (to-room "A sheet of flame appears before $n!"))

(define-spell haste ()
  (duration (floor level 4))
  (set-affbit 2 +aff2-haste+)
  (to-target "You start moving FAST."))

(define-spell infravision ()
  (duration (+ 12 (floor level 4)))
  (set-affbit 1 +aff-infravision+)
  (to-target "Your eyes glow red.")
  (to-room "$n's eyes glow red."))

(define-spell divine-illumination ()
  (duration (+ 8 level))
  (set-affbit 2 +aff2-divine-illumination+)
  (cond
    ((is-good caster)
     (to-target "You are surrounded with a soft holy light.")
     (to-room "$n is surrounded by a soft holy light."))
    ((is-evil caster)
     (to-target "You are surrounded with an unholy light.")
     (to-room "$n is surrounded by an unholy light."))
    (t
     (to-target "You are surrounded with a sickly light.")
     (to-room "$n is surrounded by a sickly light."))))

(define-spell glowlight ()
  (duration (+ 8 level))
  (set-affbit 1 +aff-glowlight+)
  (to-target "The area around you is illuminated with ghostly light.")
  (to-room "A ghostly light appears around $n."))

(define-spell invisible ()
  (duration (+ 12 (floor level 4)))
  (affect +apply-ac+ -20)
  (set-affbit 1 +aff-invisible+)
  (to-target "You vanish.")
  (to-room "$n slowly fades out of existence."))

(define-spell greater-invis ()
  (duration (+ 3 (floor level 8)))
  (if (aff-flagged target +aff-invisible+)
      (affect +apply-ac+ -20)
      (affect +apply-ac+ -4))
  (set-affbit 1 +aff-invisible+)
  (to-target "You vanish.")
  (to-room "$n slowly fades out of existence."))

(define-spell invis-to-undead ()
  (duration (+ 3 (floor level 8)))
  (set-affbit 2 +aff2-invis-to-undead+)
  (to-target "The undead can no longer see you."))

(define-spell animal-kin ()
  (duration (+ 12 (floor level 4)))
  (set-affbit 2 +aff2-animal-kin+)
  (to-target "You feel a strong kinship with animals."))

(define-spell magical-prot ()
  (duration (+ 3 (floor level 4)))
  (affect +apply-saving-spell+ (+ (- (floor level 8)) 1))
  (to-target "You are now protected somewhat against the forces of magic.")
  (to-room "A shimmering aura appears around $n's body, then dissipates."))

(define-spell petrify ()
  (duration level)
  (set-affbit 2 +aff2-petrified+)
  (to-target "You feel petrified as your body TURNS TO STONE!")
  (to-room "$n suddenly turns to stone and stops in $s tracks!"))

(define-spell gas-breath ()
  (when (needs-to-breathe target)
    (duration level)
    (affect +apply-str+ -2)
    (damage (+ (dice level 15) level))
    (cond
      ((> level (+ 40 (random-range 0 8)))
       (set-affbit 3 +aff3-poison-3+))
      ((> level (+ 30 (random-range 0 9)))
       (set-affbit 3 +aff3-poison-2+))
      (t
       (set-affbit 1 +aff-poison+)))
    (to-target "You inhale the vapors and get violently sick!")
    (to-room "$n gets violently ill from inhaling the vapors!")))

(define-spell poison ()
  (unless (is-undead target)
    (duration level)
    (affect +apply-str+ -2)
    (cond
      ((> level (+ 40 (random-range 0 8)))
       (set-affbit 3 +aff3-poison-3+))
      ((> level (+ 30 (random-range 0 9)))
       (set-affbit 3 +aff3-poison-2+))
      (t
       (set-affbit 1 +aff-poison+)))
    (to-target "You get violently sick!")
    (to-room "$n gets violently ill!")))

(define-spell prismatic-sphere ()
  (duration (+ 2 (int-of caster)))
  (set-affbit 3 +aff3-prismatic-sphere+)
  (to-target "A prismatic sphere of light appears around you!")
  (to-room "A prismatic sphere of light appears, surrounding $n!"))

(define-spell sickness ()
  (unless (is-sick target)
    (duration (- (dice level 8) (* (con-of target) 2)))
    (affect +apply-hitroll+ (- (floor level 5)))
    (affect +apply-damroll+ (- (floor level 5)))
    (set-affbit 3 +aff3-sickness+)))

(define-spell shroud-obscurement ()
  (duration (+ 10 (floor level 2)))
  (set-affbit 3 +aff3-shroud-obscurement+)
  (to-target "An obscuring shroud forms in the space around you.")
  (to-room "An obscuring shroud forms around $n."))

(define-spell slow ()
  (duration (+ 1 (floor level 4)))
  (affect +apply-dex+ (- (random-range 0 (floor level 16))))
  (set-affbit 2 +aff2-slow+)
  (to-target "Your movements slow to a tortured crawl."))

(define-spell prot-from-evil ()
  (duration 12)
  (cond
    ((is-evil target)
     (affect (random-elt (list +apply-str+
                                         +apply-int+
                                         +apply-con+
                                         +apply-cha+
                                         +apply-hitroll+
                                         +apply-damroll+))
             (- (floor level 8)))
     (to-target "You feel terrible!"))
    (t
     (set-affbit 1 +aff-protect-evil+)
     (to-target "You feel invulnerable against the forces of evil!"))))

(define-spell prot-from-good ()
  (duration 12)
  (cond
    ((is-good target)
     (affect (random-elt (list +apply-str+
                                         +apply-int+
                                         +apply-con+
                                         +apply-cha+
                                         +apply-hitroll+
                                         +apply-damroll+))
             (- (floor level 8)))
     (to-target "You feel terrible!"))
    (t
     (set-affbit 1 +aff-protect-good+)
     (to-target "You feel invulnerable against the forces of good!"))))

(define-spell protect-from-devils ()
  (duration (+ 12 (floor level 8)))
  (set-affbit 2 +aff2-prot-devils+)
  (to-target "The devilish races will have difficulty harming you."))

(define-spell prot-from-lightning ()
  (duration (+ 12 (floor level 4)))
  (set-affbit 2 +aff2-prot-lightning+)
  (to-target "You feel like standing on a hill holding a flagpole!"))

(define-spell prot-from-fire ()
  (duration (+ 12 (floor level 4)))
  (set-affbit 2 +aff2-prot-fire+)
  (to-target "You feel like joining the local volunteer fire department!"))

(define-spell undead-prot ()
  (duration (+ 12 (floor level 8)))
  (set-affbit 2 +aff2-protect-undead+)
  (to-target "The undead are toast.  You're bad."))

(define-spell regenerate ()
  (duration (+ (floor (level-of caster)
                      10)
               (floor (remort-gen-of caster)
                      2)))
  (set-affbit 1 +aff-regen+)
  (to-target "Your body begins to regenerate at an accelerated rate."))

(define-spell rejuvenate ()
  (duration 3)
  (set-affbit 1 +aff-rejuv+)
  (to-target "You will heal faster while sleeping."))

(define-spell sanctuary ()
  (duration 4)
  (set-affbit 1 +aff-sanctuary+)
  (cond
    ((is-evil target)
     (to-target "A cloud of darkness surrounds you!")
     (to-room "$n is surrounded by a cloud of darkness."))
    (t
     (to-target "A white aura momentarily surrounds you!")
     (to-room "$n is surrounded by a white aura."))))

(define-spell sleep ()
  (unless (or (mob-flagged target +mob-nosleep+)
              (is-undead target))
    (duration (+ 4 (floor level 10)))
    (set-affbit 1 +aff-sleep+)
    (when (> (position-of target) +pos-sleeping+)
      (to-target "You feel very sleepy...ZZzzzz...")
      (to-room "$n goes to sleep.")
      (setf (position-of target) +pos-sleeping+))))

(define-spell strength ()
  (duration (+ 4 (floor level 2)))
  (affect +apply-str+ (+ 1 (random-range 0 (floor level 8))))
  (to-target "You feel stronger!"))

(define-spell word-of-intellect ()
  (duration (if (> level 18) 2 1))
  (affect +apply-int+ (+ 4 (floor level 2)))
  (setf (aff-flags-of target) (logandc2 (aff-flags-of target) +aff-confusion+))
  (to-target "Your mental faculties improve!"))

(define-spell sense-life ()
  (duration 1)
  (set-affbit 1 +aff-sense-life+)
  (to-target "You feel your awareness improve."))

(define-spell telekinesis ()
  (duration (floor level 2))
  (set-affbit 2 +aff2-telekinesis+)
  (to-target "You feel able to carry a greater load."))

(define-spell true-seeing ()
  (duration (+ 1 (floor level 4)))
  (set-affbit 2 +aff2-true-seeing+)
  (to-target "You can now see things as they really are.")
  (to-room "$n's eyes open wide."))

(define-spell waterwalk ()
  (duration 24)
  (set-affbit 1 +aff-waterwalk+)
  (to-target "You feel webbing between your toes."))

(define-spell mana-shield ()
  (duration -1)
  (affect)
  (to-target "Your mana will now absorb a percentage of damage."))

(define-spell power ()
  (duration (+ 4 (floor level 4)))
  (affect +apply-str+ (+ 1 (dice 1 (floor level 16))))
  (to-target "A psychic finger on your brain makes you feel stronger!"))

(define-spell weakness ()
  (duration (+ 4 (floor level 4)))
  (affect +apply-str+ (- (+ 1 (dice 1 (floor level 16)))))
  (to-target "A psychic finger on your brain makes you feel weaker!"))

(define-spell clumsiness ()
  (duration (+ 4 (floor level 4)))
  (affect +apply-str+ (- (+ 1 (dice 1 (floor level 8)))))
  (to-target "A psychic finger on your brain makes you feel less agile!"))

(define-spell intellect ()
  (duration (+ 4 (floor level 4)))
  (affect +apply-int+ (+ 1 (dice 1 (floor level 16))))
  (to-target "Your mental faculties improve!"))

(define-spell confusion ()
  (duration (+ 1 (floor level 4)))
  (affect +apply-hitroll+ (- (+ 1 (floor level 7))))
  (set-affbit 1 +aff-confusion+)
  (to-target "You suddenly feel very confused!")
  (wait-state target (* 2 +pulse-violence+))
  (when (> (position-of target)
           +pos-sleeping+)
    (to-room "$n stops in $s tracks and stares off into space.")))

(define-spell endurance ()
  (duration (+ 1 (floor level 4)))
  (affect +apply-move+ (+ 10 (* level 2)))
  (restore-move (* level 2))
  (to-target "You feel your energy capacity rise."))

(define-spell fear ()
  (cond
    ((or (is-undead target)
         (is-dragon target)
         (is-devil target))
     (when caster
       (act caster :target target
            :subject-emit "You fail to affect $N!"))
     (to-caster "You feel a wave of fear pass over you!"))
    (t
     (duration (+ 1 (floor level 16)))
     (affect)
     (to-target "You suddenly feel very afraid!")
     (to-room "$n looks very afraid!")
     (unless (and (> (position-of target) +pos-sitting+)
                  saved)
       (perform-flee target)))))

(define-spell telepathy ()
  (duration (+ 1 (floor level 16)))
  (affect)
  (to-target "Your telepathic senses are greatly heightened."))

(define-spell confidence ()
  (duration (+ 3 (floor level 4)))
  (affect +apply-hitroll+ (dice 2 (+ 1 (floor level 8))))
  (affect +apply-saving-spell+ (- (dice 1 (+ 1 (floor level 8)))))
  (set-affbit 1 +aff-confidence+)
  (to-target "You suddenly feel very confident!"))

(define-spell nopain ()
  (duration (+ 1 (dice 3 (+ 1 (floor level 16)))))
  (set-affbit 1 +aff-nopain+)
  (to-target "You feel like you can take anything!")
  (to-room "$n ripples $s muscles and grins insanely!"))

(define-spell retina ()
  (duration (+ 12 (floor level 2)))
  (set-affbit 1 +aff-retina+)
  (to-target "The rods of your retina are stimulated!")
  (to-room "$n's eyes shine brightly."))

(define-spell adrenaline ()
  (duration (+ 3 (floor level 8)))
  (affect +apply-hitroll+ (dice 1 (+ 1 (floor level 8))))
  (set-affbit 1 +aff-adrenaline+)
  (to-target "A rush of adrenaline hits your brain!"))

(define-spell dermal-hardening ()
  (duration (dice 4 (+ 1 (floor level 8))))
  (affect +apply-ac+ -10)
  (to-target "You feel your skin tighten up and thicken."))

(define-spell vertigo ()
  (duration 6)
  (affect +apply-hitroll+ (- (+ 2 (floor level 10))))
  (affect +apply-dex+ (- (+ 1 (floor level 16))))
  (set-affbit 2 +aff2-vertigo+)
  (to-target "You feel a wave of vertigo rush over you!")
  (to-room "$n staggers in a dazed way."))

(define-spell breathing-stasis ()
  (duration (* (dice 1 (+ 1 (floor level 8)))
               (floor level 16)))
  (affect +apply-move+ (- (floor level 2) 50))
  (set-affbit 3 +aff3-nobreathe+)
  (to-target "Your breathing rate drops into a static state"))

(define-spell metabolism ()
  (duration (dice 2 (+ 2 (floor level 8))))
  (affect +apply-saving-chem+ (floor level 4))
  (set-affbit 3 +aff3-nobreathe+)
  (to-target "Your metabolism speeds up."))

(define-spell relaxation ()
  (duration (dice 2 (+ 2 (floor level 8))))
  (affect +apply-move+ (- (floor level 2) 35))
  (affect +apply-str+ -1)
  (set-affbit 3 +aff3-mana-tap+)
  (to-target "Your body and mind relax."))

(define-spell cell-regen ()
  (duration (dice 1 (+ 1 (floor level 8))))
  (affect +apply-con+ 1)
  (restore-hitp (floor
                 (* (check-skill caster +spell-cure-light+)
                    (+ (dice 4 (+ 6 (floor (check-skill caster +spell-cell-regen+) 16)))
                       (random-range (floor level 2)
                                     (* level 2))))
                 100))
  (to-target "Your cell regeneration rate increases.")

  (when (affected-by-spell target +skill-hamstring+)
    (affect-from-char target +skill-hamstring+)
    (to-target "The wound on your leg closes!")
    (to-room "The gaping wound on $n's leg closes.")))

(define-spell psishield ()
  (duration (+ 3 (dice 1 (+ 1 (floor level 8)))))
  (set-affbit 3 +aff3-psishield+)
  (to-target "You feel a psionic shield form around your mind."))

(define-spell motor-spasm ()
  (duration (+ 1 (random-range 0 (floor level 16))))
  (affect +apply-dex+ (- (random-range 0 (floor level 8))))
  (damage (+ (dice 9 5) (floor level 2)))
  (to-target "Your muscles begin spasming uncontrollably."))

(define-spell psychic-resistance ()
  (duration (+ 3 (dice 1 (+ 1 (floor level 8)))))
  (affect +apply-saving-psi+ (- (+ 5 (floor level 8))))
  (to-target "The psychic conduits of your mind become resistant to external energies."))

(define-spell psychic-crush ()
  (duration (+ 2 (dice 1 (+ 1 (floor level 16)))))
  (affect +apply-mana+ (- (+ 5 (floor level 8))))
  (damage (dice level 10))
  (set-affbit 3 +aff3-psychic-crush+))

(define-spell psychic-feedback ()
  (duration (+ (floor level 8)
               (dice 1 (+ 1 (floor level 8)))))
  (affect)
  (to-target "You well now send psychic feedback to anyone who attacks you."))

(define-spell gamma-ray ()
  (duration (floor level 4))
  (affect +apply-hit+
          (* (- level)
                       (if (eql (char-class-of caster)
                                +class-physic+)
                           (floor (+ 2 (remort-gen-of caster) 2))
                           1)))
  (affect +apply-move+ (- (floor level 2)))
  (damage (dice 12 (floor level 2)))
  (to-target "You feel irradiated... how irritating.")
  (to-room "$n appears slightly irradiated."))

(define-spell gravity-well ()
  (duration (floor level 8))
  (let ((dam (+ 10 (dice (floor level 2) 5))))
    (if (aff3-flagged target +aff3-gravity-well+)
        (damage (floor dam 2) +type-pressure+)
        (damage dam +type-pressure+)))
  (when (and (not (aff3-flagged target +aff3-gravity-well+))
             (or (> (position-of target)
                    +pos-standing+)
                 (> (random-range 1 level)
                    (str-of target))))
    (setf (position-of target) +pos-resting+)
    (act target
         :subject-emit "The gravity around you suddenly increases, slamming you to the ground!"
         :place-emit "The gravity around $n suddenly increases, slamming $m to the ground!"))

  (affect +apply-str+
          (if (eql (char-class-of caster)
                   +class-physic+)
              (- (floor level 5))
              (- (floor level 8))))
  (to-target "The gravity well seems to take hold on your body."))

(define-spell capacitance-boost ()
  (duration (+ 1 (floor level 4)))
  (affect +apply-move+ (+ 10 (* level 2)))
  (restore-move (* level 2))
  (to-target "You feel your energy capacity rise."))

(define-spell vacuum-shroud ()
  (duration (max 15 (floor level 4)))
  (set-affbit 3 +aff3-nobreathe+)
  (set-affbit 2 +aff2-prot-fire+)
  (to-target "A total vacuum springs into existence around your body."))

(define-spell albedo-shield ()
  (duration level)
  (set-affbit 3 +aff3-emp-shield+)
  (to-target "You feel protected from electromagnetic attacks."))

(define-spell gauss-shield ()
  (duration (floor level 4))
  (affect)
  (to-target "You feel protected from metal."))

(define-spell chemical-stability ()
  (duration (floor level 4))
  (affect)
  (to-target "You feel more chemically inert.")
  (to-room "$n begins looking more chemically inert."))

(define-spell acidity ()
  (let ((af (affected-by-spell target +spell-chemical-stability+)))
    (cond
      (af
       (to-target "Your chemical stability prevents acidification from occurring!")
       (to-room "$n's chemical stability prevents acidification! from occurring")
       (decf (duration-of af) (floor level 8))
       (unless (plusp (duration-of af))
         (affect-remove target af)))

      (t
       (damage (+ (dice 3 4) (floor level 4)))
       (duration (floor level 8))
       (set-affbit 3 +aff3-acidity+)))))

(define-spell halflife ()
  (duration (floor level 4))
  (set-affbit 3 +aff3-radioactive+)
  (affect +apply-con+ (- (random-range 1 (+ (floor level 16)))))
  (to-target "You suddenly begin to feel radioactive.")
  (to-room "$n becomes radioactive."))

(define-spell electrostatic-field ()
  (duration (+ 2 (floor level 4)))
  (affect)
  (to-target "An electrostatic field cracles into being around you.")
  (to-room "An electrostatic field cracles into being around $n."))

(define-spell radioimmunity ()
  (duration (floor level 2))
  (set-affbit 2 +aff2-prot-rad+)
  (to-target "You feel more resistant to radiation."))

(define-spell attraction-field ()
  (duration (+ 1 (floor level 4)))
  (affect +apply-ac+ (+ 10 level))
  (to-target "You feel very attractive -- to weapons.")
  (to-room "$n suddenly becomes attractive like a magnet!"))

(define-spell repulsion-field ()
  (duration (max 12 (floor level 4)))
  (affect +apply-ac+ (- (+ 20 (floor level 4))))
  (to-target "The space around you begins repelling matter."))

(define-spell fluoresce ()
  (duration (+ 8 level))
  (set-affbit 2 +aff2-fluorescent+)
  (to-target "The area around you is illuminated with fluorescent atoms.")
  (to-room "The light of fluorescent atoms surrounds $n."))

(define-spell temporal-compression ()
  (duration (floor level 2))
  (set-affbit 2 +aff2-haste+)
  (to-target "Time seems to slow down around you."))

(define-spell temporal-dilation ()
  (duration (+ 1 (floor level 4)))
  (set-affbit 2 +aff2-slow+)
  (affect +apply-dex+
          (- (random-range 0 (floor level 25))))
  (to-target "Time seems to speed up around you as your movements slow to a crawl."))

(define-spell dimensional-shift ()
  (duration (random-range (+ 1 (floor level 15))
                          (+ 1 (floor level 9))))
  (affect)
  (to-target "You step into an infinitesimally different plane of the multiverse."))

(define-spell taint ()
  (cond
    ((has-symbol target)
     (to-caster "Your rune of taint fails to form."))
    (t
     (damage (dice (get-skill-bonus caster +spell-taint+) 15))
     (duration (dice level 6))
     (set-affbit 3 +aff3-tainted+)
     (to-target "The mark of the tainted begins to burn brightly on your forehead!")
     (to-room "The mark of theta inted begins to burn brightly on $n's forehead!"))))

(define-spell symbol-of-pain ()
  (cond
    ((has-symbol target)
     (to-caster "Your symbol of pain fails.")
     (to-target "$N's symbol of pain fails to mark you!"))
    (t
     (duration (random-range 1 3))
     (set-affbit 3 +aff3-symbol-of-pain+)
     (affect +apply-dex+ (- (floor level 7)))
     (affect +apply-hitroll+ (- (+ 1 (floor level 4))))
    (cond
      ((is-good target)
       (damage (+ (dice level 12)
                  (* level 5))))
      ((is-evil target)
       (damage (floor (+ (dice (- level (level-of target)) 7)
                         level)
                      4)))
      (t
       (damage (+ (dice (- level (level-of target)) 7)
                  level))))
     (to-target "You shudder and shake as your mind burns!")
     (to-room "$n shudders and shakes in pain!"))))

(define-spell time-warp ()
  (duration (+ 3 level))
  (set-affbit 1 +aff-time-warp+)
  (to-target "You are now able to move freely through time."))

(define-spell transmittance ()
  (duration (+ 8 (floor level 4)))
  (affect +apply-ac+ -16)
  (set-affbit 2 +aff2-transparent+)
  (to-target "You become transparent.")
  (to-room "$n's body slowly becomes completely transparent."))

(define-spell tidal-spacewarp ()
  (duration (+ 6 level))
  (set-affbit 1 +aff-inflight+)
  (setf (position-of target) +pos-flying+)
  (to-target "Your feet lift lightly from the ground.")
  (to-room "$n begins to hover above the ground."))

(define-spell quad-damage ()
  (duration 6)
  (affect)
  (to-target "There is a screaming roar and you begin to glow brightly!")
  (to-room "There is a screaming roar as $n begins to glow brightly!"))

(define-spell densify ()
  (duration (+ 1 (floor level 2)))
  (affect +apply-char-weight+ (+ level (int-of caster)))
  (to-target "You feel denser."))

(define-spell lattice-hardening ()
  (duration (+ 1 (floor level 2)))
  (affect)
  (to-target "Your molecular bonds seem strengthened."))

(define-spell refraction ()
  (duration (+ 1 (floor level 2)))
  (affect +apply-ac+ (- (int-of caster)))
  (set-affbit 2 +aff2-displacement+)
  (to-target "Your body becomes irregularly refractive.")
  (to-room "$n's body becomes irregularly refractive."))

(define-spell shield-of-righteousness ()
  (when (is-good target)
    (duration (floor level 4))
    (affect +apply-caster+ (idnum-of caster))
    (affect +apply-ac+ -10)
    (cond
      ((eql caster target)
       (to-target "A shield of righteousness appears around you.")
       (to-room "A shield of righteousness expands around $N."))
      (t
       (to-target "You feel enveloped in $N's shield of righteousness.")))))

(define-spell blackmantle ()
  (duration (floor level 4))
  (affect +apply-hit+ (max (- level) (- (- (max-hitp-of target)) 1)))
  (to-target "An evil black mantle of magic surrounds you.")
  (to-room "A mantle of darkness briefly surrounds $n."))

(define-spell sanctification ()
  (duration (floor level 8))
  (affect +apply-move+ (floor level 2))
  (to-target "You have been sanctified!")
  (to-room "An aura of sanctification glows about $n."))

(define-spell stigmata ()
  (cond
    ((is-good target)
     (to-caster "You cannot stigmatize the good."))
    ((has-symbol target)
     (to-caster "Your stigmata fails."))
    (t
     (duration (floor level 4))
     (affect)
     (to-target "A bloody stigmatic mark appears on your forehead.")
     (to-room "A bloody stigmatic mark appears on $n's forehead."))))

(define-spell entangle ()
  (cond
    ((notany (terrain-of (in-room-of target))
             (list +sect-field+ +sect-forest+ +sect-hills+ +sect-mountain+
                   +sect-farmland+ +sect-rock+ +sect-swamp+ +sect-city+
                   +sect-catacombs+ +sect-jungle+ +sect-road+))
     (to-caster "There is not enough vegetation here for that."))
    ((or (= (terrain-of (in-room-of target)) +sect-city+)
         (= (terrain-of (in-room-of target)) +sect-cracked-road+))
     (duration (floor level 16))
     (affect +apply-hitroll+ (- (floor level 4)))
     (affect +apply-dex+ (- (floor level 16)))
     (to-target "Nearby grass and weeds come alive, entangling you where you stand!")
     (to-room "Nearby grass and weeds come alive, entangling $n where $e stands!"))
    ((or (not (room-is-outside (in-room-of target)))
         (= (terrain-of (in-room-of target))
            +sect-road+))
     (duration (floor level 8))
     (affect +apply-hitroll+ (- (floor level 4)))
     (affect +apply-dex+ (- (floor level 16)))
     (to-target "Vines and vegetation come alive, entangling you where you stand!")
     (to-room "Vines and vegetation come alive, entangling $n where $e stands!"))
    (t
     (duration (floor level 4))
     (affect +apply-hitroll+ (- (floor level 2)))
     (affect +apply-dex+ (- (floor level 8)))
     (to-target "Vines and vegetation come alive, entangling you where you stand!")
     (to-room "Vines and vegetation come alive, entangling $n where $e stands!"))))

(define-spell amnesia ()
  (duration (max 10 (- level 20)))
  (affect +apply-int+ (- (floor level 8)))
  (to-target "A wave of amnesia washes over your mind.")
  (to-room "A cloud of forgetfulness passes over $n's face."))

(define-spell anti-magic-shell ()
  (duration level)
  (affect)
  (to-target "A dark and glittering translucent shell appears around you.")
  (to-room "A dark and glittering translucent shell appears around $n."))

(define-spell sphere-of-desecration ()
  (duration level)
  (affect)
  (to-target "A shimmering dark translucent sphere appears around you.")
  (to-room "A shimmering dark translucent sphere appears around $n."))

(define-spell divine-intervention ()
  (duration level)
  (affect)
  (to-target "A shimmering pearly translucent sphere appears around you.")
  (to-room "A shimmering pearly translucent sphere appears around $n."))

(define-spell malefic-violation ()
  (duration level)
  (affect)
  (to-target "You feel wickedly potent."))

(define-spell righteous-penetration ()
  (duration level)
  (affect)
  (to-target "You have been granted terrible potency against the forces of evil!"))

(define-spell entropy-field ()
  (duration level)
  (affect)
  (damage (+ 10 (dice (floor level 5) 3)))
  (to-target "You suddenly feel like you're falling apart!"))

(define-spell divine-power ()
  (duration (+ 8 (floor level 10)))
  (affect +apply-str+ (floor level 15))
  (affect +apply-hit+ (* level 3))
  (incf (hitp-of target) (* level 3))
  (set-affbit 3 +aff3-divine-power+)
  (to-target "Your veins course with the power of Guiharia!"))

(define-spell fire-breathing ()
  (duration (+ 10 (floor level 4)))
  (affect)
  (to-target "A warm tingling begins in the back of your throat.")
  (to-room "$n's eyes begin to glow a deep red."))

(define-spell frost-breathing ()
  (duration (+ 10 (floor level 4)))
  (affect)
  (to-target "A cool tingling begins in the back of your throat.")
  (to-room "$n's eyes begin to glow a deep blue."))

(define-song drifters-ditty ()
  (duration (+ 1 (floor level 2)))
  (affect +apply-move+ (+ 15 level))
  (restore-move (* level 2))
  (to-target "The song bolsters your spirits!"))

(define-song aria-of-armament ()
  (duration (+ 1 (floor level 4)))
  (affect +apply-ac+ (- (+ 20 (floor level 4))))
  (to-target "You feel the song form a protective shield around you."))

(define-song verse-of-vulnerability ()
  (duration (+ 1 (floor level 8)))
  (affect +apply-ac+ (+ (random-range 5 20)
                        (floor level 4)))
  (affect +apply-dex+ (- (+ 1 (floor level 25))))
  (to-target "Your armor softens at $N's words!")
  (to-room "$n's armor appears to grow softer with $N's song!"))

(define-song melody-of-mettle ()
  (duration (+ 10 (floor level 4)))
  (affect +apply-con+ (+ 1 (floor level 25)))
  (affect +apply-hit+ (+ 50 (min level 125)))
  (incf (hitp-of target) (+ 50 (min level 125)))
  (to-target "The power of the song infuses your spirit!"))

(define-song regalers-rhapsody ()
  (duration (+ 10 (floor level 4)))
  (affect +apply-nohunger+ 1)
  (affect +apply-nothirst+ 1)
  (to-target "The uplifting tune drains away your hunger and thirst."))

(define-song defense-ditty ()
  (duration (+ 20 (floor level 8)))
  (affect +apply-saving-psi+ (- (+ 1 (floor level 10))))
  (when (< (random-range 0 120) level)
    (affect +apply-saving-phy+ (- (+ 1 (floor level 10)))))
  (when (< (random-range 0 200) level)
    (affect +apply-saving-spell+ (- (+ 1 (floor level 10)))))
  (to-target "Your resistances increase as the music surrounds you."))

(define-song alrons-aria ()
  (duration (+ 3 (floor level 4)))
  (affect +apply-hitroll+ (dice 2 (+ 1 (floor level 8))))
  (affect +apply-saving-spell+ (- (dice 1 (+ 1 (floor level 8)))))
  (set-affbit 1 +aff-confidence+)
  (to-target "Your resistances increase as the music surrounds you."))

(define-song verse-of-valor ()
  (duration (+ 6 (floor level 8)))
  (affect +apply-hitroll+ (+ 5 (random-range 0 6) (floor level 25)))
  (to-target "The valor of heros gone comes crashing into your mind!"))

(define-song white-noise ()
  (duration (+ 1 (floor level 25)))
  (set-affbit 1 +aff-confusion+)
  (when (> (position-of target)
           +pos-sleeping+)
    (to-room "$n stops suddenly and stares around as if confused."))
  (to-target "You suddenly feel very confused!")
  (wait-state target (* 2 +pulse-violence+)))

(define-song chant-of-light ()
  (duration (+ 1 (floor level 4)))
  (set-affbit 1 +aff-glowlight+)
  (set-affbit 2 +aff2-endure-cold+)
  (to-target "The air around you begins to emit a warm glow.")
  (to-room "$n's music causes the air to glow warmly."))

(define-song irresistable-dance ()
  (duration (+ 1 (floor level 25)))
  (affect +apply-hitroll+ (- (+ 4 (floor level 20))))
  (when (> (position-of target)
           +pos-sitting+)
    (to-target "You begin to dance uncontrollably!")
    (to-room "$n begins to dance uncontrollably!")))

(define-song insidious-rhythm ()
  (duration (+ 1 (floor level 25)))
  (affect +apply-int+ (- (+ 2 (floor level 20))))

  (to-target "$N's music snakes its way into your brain, dulling your senses.")
  (to-room "The music causes $n's eyes to glaze over."))

(define-song eagles-overture ()
  (duration (+ 1 (floor level 4)))
  (affect +apply-cha+ (+ 5 (floor level 20)))
  (to-target "The song lifts your spirits and puts a smile on your face."))

(define-song weight-of-the-world ()
  (duration (+ 1 (floor level 25)))
  (set-affbit 2 +aff2-telekinesis+)
  (to-target "You feel the weight of the world lifted from your shoulders."))

(define-song guiharias-glory ()
  (duration (+ 3 (floor level 8)))
  (affect +apply-damroll+ (dice 2 (+ 1 (floor level 16))))
  (to-target "You feel the power of dieties flowing in your veins!"))

(define-song unladen-swallow-song ()
  (cond
    ((<= (position-of target)
         +pos-sleeping+)
     (act target :target caster :target-emit "$n is asleep and probably should not fly right now."))
    (t
     (duration (+ 5 (floor level 10)))
     (set-affbit 1 +aff-inflight+)
     (to-target "The music lifts you off your ffet and sustains you.")
     (to-room "$n is lifted by the music and held suspended.")
     (setf (position-of target) +pos-flying+))))

(define-song power-overture ()
  (duration (+ 1 (floor level 4)))
  (affect +apply-str+ (+ 1 (random-range 0 2)
                         (floor level 30)))
  (affect +apply-hitroll+ (+ 1 (random-range 0 7)
                             (floor level 30)))
  (to-target "Your strength seems to grow as the song swells."))

(define-song wounding-whispers ()
  (duration (+ 5 (floor level 4)))
  (affect)
  (to-target "Whispers of your song begin whirling around you!")
  (to-room "Whispers of $N's song begin whirling around $M!"))

(define-song rhythm-of-rage ()
  (duration (+ 1 (floor level 32)))
  (affect +apply-int+ (- (+ 4 (floor level 32))))
  (affect +apply-wis+ (- (+ 4 (floor level 32))))
  (affect +apply-damroll+ (+ 1 (floor level 12) (floor level 16)))
  (set-affbit 2 +aff2-berserk+)
  (to-target "The music drives you feril with rage!")
  (to-room "$n looks murderous.  You might want to get out of here!")
  (perform-barb-berserk target))

(define-song aria-of-asylum ()
  (duration (+ 5 (floor level 8)))
  (affect +apply-caster+ (idnum-of caster))
  (to-target "A gossimer shield of music forms around you.")
  (to-room "A gossimer shield of music forms around $n."))

(define-song fortissimo ()
  (duration (+ 1 (cha-of caster) (floor level 8)))
  (affect +apply-caster+ (idnum-of caster))
  (to-target "The air around you begins to vibrate with an increased intensity."))

(define-song lichs-lyrics ()
  (duration (+ 1 (floor (cha-of caster) 8)
               (floor level 16)))
  (affect +apply-caster+ (idnum-of caster))
  (damage (dice (floor level 2) 4))
  (to-target "Your flesh begins to rot and decay!")
  (to-room "$n's flesh begins to rot and decay!"))

(define-song misdirection-melisma ()
  (duration (+ 1 (floor (cha-of caster) 4) (floor level 16)))
  (affect +apply-caster+ (idnum-of caster))
  (to-target "You begin misdirecting attempts to track you."))

(define-song mirror-image-melody ()
  (duration (+ 1 (floor (cha-of caster) 4) (floor level 8)))
  (affect +apply-caster+ (idnum-of caster))
  (to-target "Mirror images of yourself begin moving around you.")
  (to-room "Mirror images of $n begin moving around $m."))

(define-spell cure-light ()
  (when (plusp (restore-hitp (floor
                              (* (check-skill caster +spell-cure-light+)
                                 (+ 1
                                    (dice 1 8)
                                    (floor level 4)))
                              100)))
    (to-target "You feel better.")))

(define-spell cure-critic ()
  (when (plusp (restore-hitp (floor
                              (* (check-skill caster +spell-cure-light+)
                                 (+ 3
                                    (dice 3 8)
                                    (floor level 4)))
                              100)))
    (to-target "You feel a lot better!")))

(define-spell heal ()
  (when (plusp (restore-hitp (floor
                              (* (check-skill caster +spell-cure-light+)
                                 (+ 50
                                    (dice 3 level)))
                              100)))
    (to-target "A warm feeling floods your body.")))

(define-spell greater-heal ()
  (when (plusp (restore-hitp (floor
                              (* (check-skill caster +spell-cure-light+)
                                 (+ 100
                                    (dice 5 level)))
                              100)))
    (to-target "A supreme warm feeling floods your body.")))

(define-spell restoration ()
  (unless (minusp (get-condition target +full+))
    (setf (aref (conditions-of target) +full+) 24))
  (unless (minusp (get-condition target +thirst+))
    (setf (aref (conditions-of target) +thirst+) 24))

  ;; Note hack here to handle blackmantle
  (when (plusp (restore-hitp (max-hitp-of target)))
    (setf (hitp-of target) (max-hitp-of target))
    (to-target "You feel totally healed!")))

(define-spell refresh ()
  (when (plusp (restore-move (floor
                              (* (check-skill caster +spell-greater-heal+)
                                 (+ 50 (random-range 0 level))
                                 (wis-of caster))
                              100)))
    (to-target "You feel refreshed!")))

(define-spell mana-restore ()
  (when (plusp (restore-mana (dice level 10)))
    (to-target "You feel your spiritual energies replenished.")
    (to-room "$n is surrounded by a brief aura of blue light.")))

(define-spell psychic-conduit ()
  (restore-mana
   (+ level
      (floor (check-skill caster +spell-psychic-conduit+)
             20)
      (random-range 0 (+ (wis-of caster)
                         (* 4 (remort-gen-of caster)))))))

(define-spell satiation ()
  (gain-condition target +full+ (dice 3 (min 3 (1+ (floor level 4)))))
  (to-target "You feel satiated."))

(define-spell quench ()
  (gain-condition target +thirst+ (dice 3 (min 3 (1+ (floor level 4)))))
  (to-target "Your thirst is quenched."))

(define-spell wound-closure ()
  (when (< (hitp-of target) (max-hitp-of target))
    (restore-hitp (+ (dice 3 (+ 6 (floor (check-skill caster +spell-wound-closure+)
                                         32)
                                (random-range (floor level 2)
                                              level)))
                     (* (remort-gen-of caster) 4)))
    (to-target "Some of your wounds seal as you watch.")))

(define-song rhapsody-of-remedy ()
  (when (< (hitp-of target) (max-hitp-of target))
    (restore-hitp (+
                   (dice 10 (1+ (floor (get-skill-bonus caster +song-rhapsody-of-remedy+) 4)))
                   (* (- (floor (get-skill-bonus caster +song-rhapsody-of-remedy+) 10)
                         (- 25 (cha-of caster)))
                      4)))
    (to-target "Your wounds fade along with the last notes of the music.")))

(define-spell essence-of-evil ()
  (cond
    ((is-evil target)
     (alter-alignment (* level -2))
     (to-target "You feel the essence of evil burning in your soul."))
    ((is-good target)
     (damage (+ 100 (* 4 level))))))

(define-spell essence-of-good ()
  (cond
    ((is-good target)
     (alter-alignment (* level 2))
     (to-target "You feel the essence of goodness bathe your soul."))
    ((is-evil target)
     (damage (+ 100 (* 4 level))))))

(define-spell psionic-shockwave ()
  (when (and (> (position-of target) +pos-sitting+)
             (> (random-range 5 25) (floor (dex-of target) 3)))
    (setf (position-of target) +pos-sitting+)
    (wait-state target (rl-sec 2))
    (to-caster "$N is knocked to the ground by your psionic shockwave!")
    (if (is-psionic target)
        (to-target "You are knocked to the ground by $N's psionic shockwave!")
        (to-target "Your head explodes with pain and you fall to the ground in agony!"))
    (to-room "$N suddenly falls to the ground, clutching $S head!")))

(defun activate-vstone (caster stone)
  (let ((dest-room (real-room (obj-val-of stone 0))))
    (cond
      ((or (not (is-obj-kind stone +item-vstone+))
           (/= (obj-val-of stone 2) 0 -1))
       (send-to-char caster "~a" +noeffect+))
      ((and (plusp (obj-val-of stone 1))
            (/= (obj-val-of stone 1)
                (idnum-of caster)))
       (act caster :item stone :all-emit "$p hums loudly and zaps $n!"))
      ((or (zerop (obj-val-of stone 0))
           (null dest-room))
       (act caster :item stone :subject-emit "$p is not linked with a real location."))
      ((and (not (eql (zone-of dest-room)
                      (zone-of (in-room-of caster))))
            (or (zone-flagged (zone-of dest-room) +zone-isolated+)
                (zone-flagged (zone-of (in-room-of caster)) +zone-isolated+)))
       (send-to-char caster "You cannot get to there from here.~%"))
      ((not (can-enter-room caster dest-room))
       (act caster :item stone
            :subject-emit "You slowly fade into nothingness."
            :place-emit "$p glows brightly as $n fades from view.")
       (act caster
            :subject-emit "Your gut wrenches as you are slung violently through spacetime."
            :place-emit "$n reappears, clutching $s gut in pain.")
       (when (plusp (obj-val-of stone 2))
         (decf (obj-val-of stone 2))
         (when (zerop (obj-val-of stone 2))
           (act caster :item stone
                :subject-emit "$p becomes dim and ceases to glow."))))

      (t
       (act caster :item stone
            :subject-emit "You slowly fade into nothingness."
            :place-emit "$p glows brightly as $n fades from view.")
       (char-from-room caster t)
       (char-to-room caster dest-room)
       (look-at-room caster (in-room-of caster) nil)
       (act caster :item stone
            :place-emit "$n slowly fades into view, $p brightly glowing.")
       (when (plusp (obj-val-of stone 2))
         (decf (obj-val-of stone 2))
         (when (zerop (obj-val-of stone 2))
           (act caster :item stone
                :subject-emit "$p becomes dim and ceases to glow.")))))))

(defun can-teleport? (caster target saved)
  (and (not (and (not (immortalp caster))
                 (immortalp target)))
       (not (and (is-pc target)
                 (link-of target)
                 (eql (state-of (link-of target)) 'network)))
       (or (creature-trusts target caster)
           (not saved))))

(defun teleportable? (ch room)
  (and (not (room-flagged room (logior +room-godroom+
                                       +room-notel+
                                       +room-norecall+
                                       +room-nomagic+
                                       +room-nophysic+
                                       +room-clan-house+
                                       +room-death+)))
       (zone-approvedp (zone-of room))
       (not (eql (plane-of (zone-of room)) +plane-olc+))
       (not (zone-flagged (zone-of room)
                          +zone-isolated+))
       (can-travel-terrain ch (terrain-of room))
       (can-enter-room ch room)))

(define-spell teleport ()
  (cond
    ((typep target 'obj-data)
     (activate-vstone caster target))
    ((or (room-flagged (in-room-of target)
                       +room-notel+)
         (room-flagged (in-room-of target)
                       +room-norecall+))
     (act target
          :subject-emit "You fade out for a moment, but the magic quickly dissipates!"
          :place-emit "$n fades out for a moment but quickly flickers back into view."))
    ((zone-flagged (zone-of (in-room-of target))
                   +zone-isolated+)
     (call-magic caster target nil nil +spell-local-teleport+ level +cast-spell+))
    ((and (not (eql caster target))
          (room-flagged (in-room-of target)
                        +room-peaceful+))
     (act caster :target target
          :subject-emit "You fail.  $N is in a non-violence zone!"
          :target-emit "You feel strange as $n attempts to teleport you."))
    ((and (immortalp target)
          (> (level-of target)
             (level-of caster)))
     (act caster :target target :all-emit "$n sneer$% at $N with disgust."))

    ((not (can-teleport? caster target saved))
     (act target :target caster
          :subject-emit "You resist $N's attempt to teleport you!"
          :target-emit "$n resists your attempt to teleport $m!"))
    ((or (mob-flagged target +mob-nosummon+)
         saved)
     (send-to-char caster "You fail.~%"))
    (t
     (act target
          :subject-emit "Your vision slowly fades to blackness..."
          :place-emit "$n slowly fades out of existence and is gone.")
     (char-from-room target t)

     (let ((teleportable-rooms (make-array 0 :adjustable t :fill-pointer 0)))
       (alexandria:maphash-values
        (lambda (room)
          (when (teleportable? target room)
            (vector-push-extend room teleportable-rooms)))
        *rooms*)
       (let ((dest-room (random-elt teleportable-rooms)))
         (char-to-room target dest-room t)))

     (act target
          :subject-emit "A new scene unfolds before you!"
          :place-emit "$n slowly fades into of existence.")
     (look-at-room target (in-room-of target) nil))))
