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
         (flet ((affect (&key duration modifier location)
                  (affect-to-char target
                                  (make-instance 'affected-type
                                                 :kind ,id-name
                                                 :owner (idnum-of caster)
                                                 :duration duration
                                                 :level level
                                                 :modifier modifier
                                                 :location location)))
                (set-affbit (idx bit &key duration)
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
           ,@body))
       (when (null (aref *spell-info* ,id-name))
         (setf (aref *spell-info* ,id-name) (make-instance 'spell-info)))
       (setf (func-of (aref *spell-info* ,id-name)) (function ,func-name)))))

(define-spell armor ()
  (affect :duration 24 :modifier (+ (floor level 4) 20) :location +apply-ac+)
  (to-target "You feel someone protecting you."))

(define-spell chill-touch ()
  (affect :duration 4 :modifier (- (1+ (floor level 16))) :location +apply-str+)
  (to-target "You feel your strength wither!"))

(define-spell barkskin ()
  (when (affected-by-spell target +spell-stoneskin+)
    (affect-from-char target +spell-stoneskin+))
  (when (affected-by-spell target +spell-thorn-skin+)
    (affect-from-char target +spell-thorn-skin+))
  (affect :duration (dice 4 (1+ (floor level 8))) :modifier -10 :location +apply-ac+)
  (to-target "Your skin tightens up and hardens."))

(define-spell stoneskin ()
  (when (affected-by-spell target +spell-barkskin+)
    (affect-from-char target +spell-stoneskin+))
  (when (affected-by-spell target +spell-thorn-skin+)
    (affect-from-char target +spell-thorn-skin+))
  (affect :duration (dice 4 (1+ (floor level 8))) :modifier -10 :location +apply-ac+)
  (to-target "Your skin hardens to a rock-like shell.")
  (to-room "$n's skin turns a pale, rough grey."))

(define-spell thorn-skin ()
  (when (affected-by-spell target +spell-barkskin+)
    (affect-from-char target +spell-barkskin+))
  (when (affected-by-spell target +spell-thorn-skin+)
    (affect-from-char target +spell-thorn-skin+))
  (affect :duration (dice 3 (1+ (floor level 4)))
          :modifier (- (+ 5 (floor (get-skill-bonus caster +spell-thorn-skin+) 10)))
          :location +apply-ac+)
  (to-target "Large thorns erupt from your skin!")
  (to-room "Large thorns erupt from $n's skin!"))

(define-spell pray ()
  (affect :location +apply-hitroll+
          :modifier (+ 3 (floor level 8))
          :duration (+ 4 (floor level 16)))
  (affect :location +apply-saving-spell+
          :modifier (- (+ 3 (floor level 16)))
          :duration (+ 4 (floor level 16)))
  (if (is-good target)
    (to-target "You feel extremely righteous")
    (to-target "You feel a dark power enter your soul.")))

(define-spell blindness ()
  (cond
    ((mob-flagged target +mob-noblind+)
     (to-caster "You fail.~%"))
    (t
     (affect :location +apply-hitroll+ :modifier -4 :duration 2)
     (affect :location +apply-ac+ :modifier 40 :duration 2)
     (set-affbit 0 +aff-blind+ :duration 2)
     (if (is-good target)
         (to-target "You feel extremely righteous")
         (to-target "You feel a dark power enter your soul.")))))

(define-spell breathe-water ()
  (set-affbit 0 +aff-waterbreath+ :duration (+ 10 level))
  (to-target "You are now able to breathe underwater."))

(define-spell spirit-track ()
  (affect :duration level)
  (to-target "You can now sense trails to other creatures."))

(define-spell word-stun ()
  (cond
    ((mob2-flagged target +mob2-nostun+)
     (to-caster "You fail the stun.~%"))
    (t
     (remove-all-combat target)
     (setf (position-of target) +pos-stunned+)
     (wait-state target (* 2 +pulse-violence+))
     (affect :location +apply-int+ :modifier -1 :duration 1)
     (to-target "You have been stunned!")
     (to-room "$n suddenly looks stunned!"))))
