(in-package #:tempus)

(defparameter +saving-throws+
  #2A(
      ;; PARA
    (90 70 69 69 68 68 67 67 66 66 65   ; 0 - 10
            65 65 64 64 63 63 62 62 61 61   ; 11 - 20
            60 60 59 59 58 58 57 57 56 56   ; 21 - 30
            55 55 54 54 53 53 52 52 51 51   ; 31 - 40
            50 50 49 49 48 48 47 47 46 46   ; 41 - 50
            46 46 46 46 46 46 46 46 46 46
        45 44 43 42 41 40 39 38 37 36 35 35)
    ; ROD
    (90 75 74 73 72 71 70 69 69 68 68   ; 0 - 10
            67 66 65 64 63 62 61 60 59 58   ; 11 - 20
            57 56 55 54 53 52 51 50 49 48   ; 21 - 30
            47 46 45 44 43 42 41 40 39 38   ; 31 - 40
            37 36 35 34 33 32 31 30 29 20   ; 41 - 50
            20 20 20 20 20 20 20 20 20 20
        0 0 0 0 0 0 0 0 0 0 0 0)
    ; PETRI
    (90 70 69 69 68 68 67 67 66 66 65   ; 0 - 10
            65 65 64 64 63 63 62 62 61 61   ; 11 - 20
            60 60 59 59 58 58 57 57 56 56   ; 21 - 30
            55 55 54 54 53 53 52 52 51 50   ; 31 - 40
            47 46 45 44 43 42 41 40 39 30   ; 41 - 50
            30 30 30 30 30 30 30 30 30 30
        0 0 0 0 0 0 0 0 0 0 0 0)
     ;BREATH
    (90 90 89 88 87 86 75 73 71 70 69   ; 0 - 10
        77 76 75 74 73 72 71 70 69 68   ; 11 - 20
            67 66 65 64 63 62 61 60 59 58   ; 21 - 30
            57 56 55 54 53 52 51 50 49 48   ; 31 - 40
            47 46 45 44 43 42 41 40 39 30   ; 41 - 50
            30 30 30 30 30 30 30 30 30 30
        0 0 0 0 0 0 0 0 0 0 0 0)
    ; SPELL
    (95 90 90 89 89 89 88 88 88 87 87   ; 0 - 10
            87 86 86 86 85 85 85 84 84 84   ; 11 - 20
            83 83 83 82 82 82 81 81 81 80   ; 21 - 30
            80 79 79 78 78 77 77 76 76 76   ; 31 - 40
            75 75 74 74 73 73 72 72 71 71   ; 41 - 50
            70 69 68 67 66 65 63 61 59 55
        42 41 40 39 38 37 36 35 34 33 34 0)
    ; CHEM
    (90 88 87 86 85 84 83 82 81 80 79   ; 0 - 10
            78 78 77 77 77 76 76 75 74 73   ; 11 - 20
            72 71 70 69 68 67 66 65 64 63   ; 21 - 30
            62 61 60 59 58 57 56 55 54 53   ; 31 - 40
            52 51 50 49 48 47 46 45 44 43   ; 41 - 50
            43 43 43 43 43 43 43 43 43 43
        42 41 40 39 38 37 36 35 34 33 34 0)
    ; PSIONIC
    (90 88 87 86 85 84 83 82 81 80 79   ; 0 - 10
            78 78 77 77 77 76 76 75 74 73   ; 11 - 20
            72 71 70 69 68 67 66 65 64 63   ; 21 - 30
            62 61 60 59 58 57 56 55 54 53   ; 31 - 40
            52 51 50 49 48 47 46 45 44 43   ; 41 - 50
            43 43 43 43 43 43 43 43 43 43
        42 41 40 39 38 37 36 35 34 33 34 0)
    ; physic
    (90 88 87 86 85 84 83 82 81 80 79   ; 0 - 10
            78 78 77 77 77 76 76 75 74 73   ; 11 - 20
            72 71 70 69 68 67 66 65 64 63   ; 21 - 30
            62 61 60 59 58 57 56 55 54 53   ; 31 - 40
            52 51 50 49 48 47 46 45 44 43   ; 41 - 50
            43 43 43 43 43 43 43 43 43 43
        42 41 40 39 38 37 36 35 34 33 34 0)))


(defun calculate-saving-throw (ch level type)
  ;; Negative save modifiers make saving throws better!
  (let ((save (+ (aref +saving-throws+ type (level-of ch))
                 (aref (saves-of ch) type)
                 (floor level 2)
                 (if (aff2-flagged ch +aff2-evade+)
                     (- (floor (level-of ch) 5)) 0)
                 (if (< (position-of ch) +pos-fighting+)
                     (* 4 (- 10 (position-of ch))) 0)
                 (if (not (zerop (speed-of ch)))
                     (floor (speed-of ch) 8) 0)
                 (if (< (position-of ch) +pos-resting+)
                     10 0)
                 (- (* (remort-gen-of ch) 2)))))
    (cond
      ((= type +saving-para+)
       (decf save (floor (con-of ch) 8))
       (when (or (and (or (is-cleric ch) (is-knight ch))
                      (not (is-neutral ch)))
                 (is-ranger ch))
         (decf save (+ 5 (floor (level-of ch) 16)))))

      ((= type +saving-rod+)
       (when (or (aff-flagged ch +aff-adrenaline+)
                 (aff2-flagged ch +aff2-haste+))
         (decf save (floor (level-of ch) 5)))
       (when (is-mage ch)
         (decf save (+ 4 (floor (level-of ch) 16))))
       (when (is-dwarf ch)
         (decf save (+ (floor (con-of ch) 2) (remort-gen-of ch))))
       (when (is-barb ch)
         (decf save (+ (floor (level-of ch) 8) (remort-gen-of ch))))
       (when (= (char-class-of ch) +class-barb+) ; primary barb
         (decf save (+ (floor (level-of ch) 8) (* (remort-gen-of ch) 2))))
       (when (= (remort-char-class-of ch) +class-barb+) ; secondary barb
         (decf save (+ (floor (level-of ch) 16) (remort-gen-of ch)))))

      ((= type +saving-petri+)
       (when (or (is-monk ch) (is-thief ch) (> (move-of ch) (random-range 100 400)))
         (decf save (+ 5 (floor (level-of ch) 8))))
       (when (is-dwarf ch)
         (decf save (+ (floor (con-of ch) 4) (remort-gen-of ch))))
       (when (= (char-class-of ch) +class-barb+) ; primary barb
         (decf save (+ (floor (level-of ch) 8) (* (remort-gen-of ch) 2))))
       (when (= (remort-char-class-of ch) +class-barb+) ; secondary barb
         (decf save (+ (floor (level-of ch) 16) (remort-gen-of ch)))))

      ((= type +saving-breath+)
       (when (or (aff-flagged ch +aff-adrenaline+)
                 (aff2-flagged ch +aff2-haste+))
         (decf save (floor (level-of ch) 5)))
       (decf save (floor (int-of ch) 16))
       (decf save (floor (wis-of ch) 16))
       (when (is-dwarf ch)
         (decf save (+ (floor (con-of ch) 4) (remort-gen-of ch))))
       (when (= (char-class-of ch) +class-barb+) ; primary barb
         (decf save (+ (floor (level-of ch) 8) (* (remort-gen-of ch) 2))))
       (when (= (remort-char-class-of ch) +class-barb+) ; secondary barb
         (decf save (+ (floor (level-of ch) 16) (remort-gen-of ch)))))

      ((= type +saving-spell+)
       (when (or (aff-flagged ch +aff-adrenaline+)
                 (aff2-flagged ch +aff2-haste+))
         (decf save (floor (level-of ch) 5)))
       (decf save (floor (wis-of ch) 4))
       (when (is-dwarf ch)
         (decf save (+ (floor (con-of ch) 4) (remort-gen-of ch))))
       (when (= (char-class-of ch) +class-barb+) ; primary barb
         (decf save (+ (floor (level-of ch) 8) (* (remort-gen-of ch) 2))))
       (when (= (remort-char-class-of ch) +class-barb+) ; secondary barb
         (decf save (+ (floor (level-of ch) 16) (remort-gen-of ch))))
       (when (is-cyborg ch)
         (incf save (+ 5 (floor (level-of ch) 2))))
       (when (is-drow ch)
         (decf save (floor (level-of ch) 2)))
       (when (and (is-npc ch) (= (vnum-of ch) 7100))
         (decf save (level-of ch))))

      ((= type +saving-chem+)
       (when (is-cyborg ch)
         (decf save (floor (level-of ch) 1)))
       (when (is-dwarf ch)
         (decf save (+ (floor (con-of ch) 4) (remort-gen-of ch))))
       (when (= (char-class-of ch) +class-barb+) ; primary barb
         (decf save (+ (floor (level-of ch) 8) (* (remort-gen-of ch) 2))))
       (when (= (remort-char-class-of ch) +class-barb+) ; secondary barb
         (decf save (+ (floor (level-of ch) 16) (remort-gen-of ch)))))

      ((= type +saving-psi+)
       (incf save (- 15 (int-of ch))))

      ((= type +saving-phy+)
       (decf save (+ (floor (level-of ch) 8) (* (remort-gen-of ch) 2))))
      (t
       (errlog "unknown savetype in calculate-saving-throw")))
    save))

(defun mag-savingthrow (ch level type)
  (cond
    ((immortalp ch)
     t)
    ((> level 100)
     nil)
    ((eql type +saving-none+)
     nil)
    (t
     (< (max 1 (calculate-saving-throw ch level type))
        (random-range 0 99)))))

(defun update-iaffects (ch)
  "For every instant affect, decrement the duration.  Removes any
instant affect that has a zero or less duration."
  (setf (affected-of ch)
        (delete-if (lambda (af)
                     (when (is-instant-of af)
                       (decf (duration-of af))
                       (not (plusp (duration-of af)))))
                   (affected-of ch))))

(defmacro define-spell (name () &body body)
  (let ((func-name (intern (concatenate 'string "SPELL-" (string name))))
        (id-name (intern (format nil "+SPELL-~a+" name))))
    `(progn
       (defun ,func-name (caster level target)
         (let ((duration (floor level 4)))
           (flet ((duration (new-duration)
                    (setf duration new-duration))
                  (affect (&key (modifier 0) (location +apply-none+))
                    (affect-to-char target
                                    (make-instance 'affected-type
                                                   :kind ,id-name
                                                   :owner (idnum-of caster)
                                                   :duration duration
                                                   :level level
                                                   :modifier modifier
                                                   :location location)))
                  (set-affbit (idx bit)
                    (affect-to-char target
                                    (make-instance 'affected-type
                                                   :kind ,id-name
                                                   :owner (idnum-of caster)
                                                   :duration duration
                                                   :level level
                                                   :aff-index idx
                                                   :bitvector bit)))
                  (to-caster (str)
                    (send-to-char caster "~a~%" str))
                  (to-target (str)
                    (send-to-char target "~a~%" str))
                  (to-room (str)
                    (act target :place-emit str)))
             (declare (ignorable #'affect #'to-target #'to-room #'to-caster #'set-affbit)
                      (dynamic-extent #'affect #'to-target #'to-room #'to-caster
                                      #'set-affbit))
             ,@body)))
       (when (null (aref *spell-info* ,id-name))
         (setf (aref *spell-info* ,id-name) (make-instance 'spell-info)))
       (setf (func-of (aref *spell-info* ,id-name)) (function ,func-name)))))

(define-spell armor ()
  (duration 24)
  (affect :location +apply-ac+ :modifier (+ (floor level 4) 20))
  (to-target "You feel someone protecting you."))

(define-spell chill-touch ()
  (duration 4)
  (affect :location +apply-str+ :modifier (- (1+ (floor level 16))))
  (to-target "You feel your strength wither!"))

(define-spell barkskin ()
  (duration (dice 4 (1+ (floor level 8))))
  (when (affected-by-spell target +spell-stoneskin+)
    (affect-from-char target +spell-stoneskin+))
  (when (affected-by-spell target +spell-thorn-skin+)
    (affect-from-char target +spell-thorn-skin+))
  (affect :location +apply-ac+ :modifier -10)
  (to-target "Your skin tightens up and hardens."))

(define-spell stoneskin ()
  (duration(dice 4 (1+ (floor level 8))))
  (when (affected-by-spell target +spell-barkskin+)
    (affect-from-char target +spell-stoneskin+))
  (when (affected-by-spell target +spell-thorn-skin+)
    (affect-from-char target +spell-thorn-skin+))
  (affect :location +apply-ac+ :modifier -10)
  (to-target "Your skin hardens to a rock-like shell.")
  (to-room "$n's skin turns a pale, rough grey."))

(define-spell thorn-skin ()
  (when (affected-by-spell target +spell-barkskin+)
    (affect-from-char target +spell-barkskin+))
  (when (affected-by-spell target +spell-thorn-skin+)
    (affect-from-char target +spell-thorn-skin+))
  (duration (dice 3 (1+ (floor level 4))))
  (affect :location +apply-ac+
          :modifier (- (+ 5 (floor level 10))))
  (to-target "Large thorns erupt from your skin!")
  (to-room "Large thorns erupt from $n's skin!"))

(define-spell pray ()
  (duration (+ 4 (floor level 16)))
  (affect :location +apply-hitroll+ :modifier (+ 3 (floor level 8)))
  (affect :location +apply-saving-spell+ :modifier (- (+ 3 (floor level 16))))
  (if (is-good target)
    (to-target "You feel extremely righteous")
    (to-target "You feel a dark power enter your soul.")))

(define-spell blindness ()
  (cond
    ((mob-flagged target +mob-noblind+)
     (to-caster "You fail.~%"))
    (t
     (duration 2)
     (affect :location +apply-hitroll+ :modifier -4)
     (affect :location +apply-ac+ :modifier 40)
     (set-affbit 1 +aff-blind+)
     (if (is-good target)
         (to-target "You feel extremely righteous")
         (to-target "You feel a dark power enter your soul.")))))

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
     (affect :location +apply-int+ :modifier -1)
     (to-target "You have been stunned!")
     (to-room "$n suddenly looks stunned!"))))

(define-spell blur ()
  (duration (+ 2 (floor level 4)))
  (affect :location +apply-ac+ :modifier -10)
  (set-affbit 1 +aff-blur+)
  (to-target "Your image suddenly starts to blur and shift.")
  (to-room "The image of $n suddenly starts to blur and shift."))

(define-spell curse ()
  (duration (+ 1 (floor level 2)))
  (affect :location +apply-hitroll+ :modifier (- (+ 1 (floor level 8))))
  (affect :location +apply-damroll+ :modifier (- (+ 1 (floor level 8))))
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
  (affect :location +apply-ac+ :modifier -8)
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
  (affect :location +apply-ac+ :modifier -20)
  (set-affbit 1 +aff-invisible+)
  (to-target "You vanish.")
  (to-room "$n slowly fades out of existence."))

(define-spell greater-invis ()
  (duration (+ 3 (floor level 8)))
  (if (aff-flagged target +aff-invisible+)
      (affect :location +apply-ac+ :modifier -20)
      (affect :location +apply-ac+ :modifier -4))
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
  (affect :location +apply-saving-spell+ :modifier (+ (- (floor level 8)) 1))
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
    (affect :location +apply-str+ :modifier -2)
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
    (affect :location +apply-str+ :modifier -2)
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
    (affect :location +apply-hitroll+ :modifier (- (floor level 5)))
    (affect :location +apply-damroll+ :modifier (- (floor level 5)))
    (set-affbit 3 +aff3-sickness+)))

(define-spell shroud-obscurement ()
  (duration (+ 10 (floor level 2)))
  (set-affbit 3 +aff3-shroud-obscurement+)
  (to-target "An obscuring shroud forms in the space around you.")
  (to-room "An obscuring shroud forms around $n."))

(define-spell slow ()
  (duration (+ 1 (floor level 4)))
  (affect :location +apply-dex+ :modifier (- (random-range 0 (floor level 16))))
  (set-affbit 2 +aff2-slow+)
  (to-target "Your movements slow to a tortured crawl."))

(define-spell prot-from-evil ()
  (duration 12)
  (cond
    ((is-evil target)
     (affect :location (random-elt (list +apply-str+
                                         +apply-int+
                                         +apply-con+
                                         +apply-cha+
                                         +apply-hitroll+
                                         +apply-damroll+))
             :modifier (- (floor level 8)))
     (to-target "You feel terrible!"))
    (t
     (set-affbit 1 +aff-protect-evil+)
     (to-target "You feel invulnerable against the forces of evil!"))))

(define-spell prot-from-good ()
  (duration 12)
  (cond
    ((is-good target)
     (affect :location (random-elt (list +apply-str+
                                         +apply-int+
                                         +apply-con+
                                         +apply-cha+
                                         +apply-hitroll+
                                         +apply-damroll+))
             :modifier (- (floor level 8)))
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
  (affect :location +apply-str+ :modifier (+ 1 (random-range 0 (floor level 8))))
  (to-target "You feel stronger!"))

(define-spell word-of-intellect ()
  (duration (if (> level 18) 2 1))
  (affect :location +apply-int+ :modifier (+ 4 (floor level 2)))
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
  (affect :location +apply-str+ :modifier (+ 1 (dice 1 (floor level 16))))
  (to-target "A psychic finger on your brain makes you feel stronger!"))

(define-spell weakness ()
  (duration (+ 4 (floor level 4)))
  (affect :location +apply-str+ :modifier (- (+ 1 (dice 1 (floor level 16)))))
  (to-target "A psychic finger on your brain makes you feel weaker!"))

(define-spell clumsiness ()
  (duration (+ 4 (floor level 4)))
  (affect :location +apply-str+ :modifier (- (+ 1 (dice 1 (floor level 8)))))
  (to-target "A psychic finger on your brain makes you feel less agile!"))

(define-spell intellect ()
  (duration (+ 4 (floor level 4)))
  (affect :location +apply-int+ :modifier (+ 1 (dice 1 (floor level 16))))
  (to-target "Your mental faculties improve!"))

(define-spell confusion ()
  (duration (+ 1 (floor level 4)))
  (affect :location +apply-hitroll+ :modifier (- (+ 1 (floor level 7))))
  (set-affbit 1 +aff-confusion+)
  (to-target "You suddenly feel very confused!")
  (wait-state target (* 2 +pulse-violence+))
  (when (> (position-of target)
           +pos-sleeping+)
    (to-room "$n stops in $s tracks and stares off into space.")))

(define-spell endurance ()
  (duration (+ 1 (floor level 4)))
  (affect :location +apply-move+ :modifier (+ 10 (* level 2)))
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
     (to-room "$n looks very afraid!"))))

(define-spell telepathy ()
  (duration (+ 1 (floor level 16)))
  (affect)
  (to-target "Your telepathic senses are greatly heightened."))

(define-spell confidence ()
  (duration (+ 3 (floor level 4)))
  (affect :location +apply-hitroll+ :modifier (dice 2 (+ 1 (floor level 8))))
  (affect :location +apply-saving-spell+ :modifier (- (dice 1 (+ 1 (floor level 8)))))
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
  (affect :location +apply-hitroll+ :modifier (dice 1 (+ 1 (floor level 8))))
  (set-affbit 1 +aff-adrenaline+)
  (to-target "A rush of adrenaline hits your brain!"))

(define-spell dermal-hardening ()
  (duration (dice 4 (+ 1 (floor level 8))))
  (affect :location +apply-ac+ :modifier -10)
  (to-target "You feel your skin tighten up and thicken."))

(define-spell vertigo ()
  (duration 6)
  (affect :location +apply-hitroll+ :modifier (- (+ 2 (floor level 10))))
  (affect :location +apply-dex+ :modifier (- (+ 1 (floor level 16))))
  (set-affbit 2 +aff2-vertigo+)
  (to-target "You feel a wave of vertigo rush over you!")
  (to-room "$n staggers in a dazed way."))

(define-spell breathing-stasis ()
  (duration (* (dice 1 (+ 1 (floor level 8)))
               (floor level 16)))
  (affect :location +apply-move+ :modifier (- (floor level 2) 50))
  (set-affbit 3 +aff3-nobreathe+)
  (to-target "Your breathing rate drops into a static state"))

(define-spell metabolism ()
  (duration (dice 2 (+ 2 (floor level 8))))
  (affect :location +apply-saving-chem+ :modifier (floor level 4))
  (set-affbit 3 +aff3-nobreathe+)
  (to-target "Your metabolism speeds up."))

(define-spell relaxation ()
  (duration (dice 2 (+ 2 (floor level 8))))
  (affect :location +apply-move+ :modifier (- (floor level 2) 35))
  (affect :location +apply-str+ :modifier -1)
  (set-affbit 3 +aff3-mana-tap+)
  (to-target "Your body and mind relax."))

(define-spell cell-regen ()
  (duration (dice 1 (+ 1 (floor level 8))))
  (affect :location +apply-con+ :modifier 1)
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
  (affect :location +apply-dex+ :modifier (- (random-range 0 (floor level 8))))
  (to-target "Your muscles begin spasming uncontrollably."))

(define-spell psychic-resistance ()
  (duration (+ 3 (dice 1 (+ 1 (floor level 8)))))
  (affect :location +apply-saving-psi+ :modifier (- (+ 5 (floor level 8))))
  (to-target "The psychic conduits of your mind become resistant to external energies."))

(define-spell psychic-crush ()
  (duration (+ 2 (dice 1 (+ 1 (floor level 16)))))
  (affect :location +apply-mana+ :modifier (- (+ 5 (floor level 8))))
  (set-affbit 3 +aff3-psychic-crush+))

(define-spell psychic-feedback ()
  (duration (+ (floor level 8)
               (dice 1 (+ 1 (floor level 8)))))
  (affect)
  (to-target "You well now send psychic feedback to anyone who attacks you."))

(define-spell gamma-ray ()
  (duration (floor level 4))
  (affect :location +apply-hit+
          :modifier (* (- level)
                       (if (eql (char-class-of caster)
                                +class-physic+)
                           (floor (+ 2 (remort-gen-of caster) 2))
                           1)))
  (affect :location +apply-move+
          :modifier (- (floor level 2)))
  (to-target "You feel irradiated... how irritating.")
  (to-room "$n appears slightly irradiated."))

(define-spell gravity-well ()
  (duration (floor level 8))
  (affect :location +apply-str+
          :modifier (if (eql (char-class-of caster)
                             +class-physic+)
                        (- (floor level 5))
                        (- (floor level 8))))
  (to-target "The gravity well seems to take hold on your body."))

(define-spell capacitance-boost ()
  (duration (+ 1 (floor level 4)))
  (affect :location +apply-move+ :modifier (+ 10 (* level 2)))
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
       (duration (floor level 8))
       (set-affbit 3 +aff3-acidity+)))))

(define-spell halflife ()
  (duration (floor level 4))
  (set-affbit 3 +aff3-radioactive+)
  (affect :location +apply-con+ :modifier (- (random-range 1 (+ (floor level 16)))))
  (to-target "You suddenly begin to feel radioactive.")
  (to-room "$n becomes radioactive."))

(define-spell electrostatic-field ()
  (duration (+ 2 (floor level 4)))
  (to-target "An electrostatic field cracles into being around you.")
  (to-room "An electrostatic field cracles into being around $n."))

(define-spell radioimmunity ()
  (duration (floor level 2))
  (set-affbit 2 +aff2-prot-rad+)
  (to-target "You feel more resistant to radiation."))

(define-spell attraction-field ()
  (duration (+ 1 (floor level 4)))
  (affect :location +apply-ac+ :modifier (+ 10 level))
  (to-target "You feel very attractive -- to weapons.")
  (to-room "$n suddenly becomes attractive like a magnet!"))

(define-spell repulsion-field ()
  (duration (max 12 (floor level 4)))
  (affect :location +apply-ac+ :modifier (- (+ 20 (floor level 4))))
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
  (affect :location +apply-dex+
          :modifier (- (random-range 0 (floor level 25))))
  (to-target "Time seems to speed up around you as your movements slow to a crawl."))

(define-spell dimensional-shift ()
  (duration (random-range (+ 1 (floor level 15))
                          (+ 1 (floor level 9))))
  (affect)
  (to-target "You step into an infinitesimally different plane of the multiverse."))