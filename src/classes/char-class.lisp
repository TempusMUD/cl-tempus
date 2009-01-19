(in-package #:tempus)

;; 0 - class/race combination not allowed
;; 1 - class/race combination allowed only for secondary class
;; 2 - class/race combination allowed for primary class
(defparameter +race-restrictions+
;;                      MG CL TH WR BR PS PH CY KN RN BD MN VP MR S1 S2 S3
  #2A((#.+race-human+    2  2  2  0  2  2  2  2  2  2  2  2  0  2  0  0  0 )
      (#.+race-elf+      2  2  2  0  0  2  2  2  2  2  2  2  0  2  0  0  0 )
      (#.+race-dwarf+    0  2  2  0  2  1  1  1  2  0  0  0  0  1  0  0  0 )
      (#.+race-half-orc+ 0  0  2  0  2  0  2  2  0  0  0  0  0  2  0  0  0 )
      (#.+race-halfling+ 2  2  2  0  2  1  1  1  2  2  2  2  0  1  0  0  0 )
      (#.+race-tabaxi+   2  2  2  0  2  2  2  2  0  2  0  2  0  2  0  0  0 )
      (#.+race-drow+     2  2  2  0  0  1  1  1  2  2  2  0  0  1  0  0  0 )
      (#.+race-minotaur+ 2  2  0  0  2  0  1  1  0  2  0  0  0  1  0  0  0 )
      (#.+race-orc+      0  0  1  0  2  0  1  2  0  0  0  2  0  2  0  0  0 )))

(defun past-class-p (i)
  (member i (list +class-magic-user+
                  +class-cleric+
                  +class-ranger+
                  +class-knight+
                  +class-barb+
                  +class-thief+
                  +class-monk+
                  +class-bard+)))

(defun future-class-p (i)
  (member i (list +class-mercenary+
                  +class-psionic+
                  +class-physic+
                  +class-cyborg+)))


(defun invalid-char-class (ch obj)
  "Used to determine if a piece of equipment is usable by a particular character class, based on the +ITEM-ANTI-<class>+ bitvectors"
  (cond
    ((and (not (is-npc ch))
          (plusp (owner-id-of (shared-of obj)))
          (/= (owner-id-of (shared-of obj)) (idnum-of ch)))
     nil)
    ((or (and (is-obj-stat obj +item-anti-magic-user+) (is-magic-user ch))
         (and (is-obj-stat obj +item-anti-cleric+) (is-cleric ch))
         (and (is-obj-stat obj +item-anti-warrior+) (is-warrior ch))
         (and (is-obj-stat obj +item-anti-thief+) (is-thief ch))
         (and (is-obj-stat obj +item-anti-barb+) (is-barb ch))
         (and (is-obj-stat obj +item-anti-psychic+) (is-psionic ch))
         (and (is-obj-stat obj +item-anti-physic+) (is-physic ch))
         (and (is-obj-stat obj +item-anti-cyborg+) (is-cyborg ch))
         (and (is-obj-stat obj +item-anti-knight+) (is-knight ch))
         (and (is-obj-stat obj +item-anti-ranger+) (is-ranger ch))
         (and (is-obj-stat obj +item-anti-bard+) (is-bard ch))
         (and (is-obj-stat obj +item-anti-monk+) (is-monk ch))
         (and (is-obj-stat2 obj +item2-anti-merc+) (is-merc ch))
         (and (not (approvedp obj))
              (not (testerp ch))
              (not (immortalp ch))))
     nil)
    ((and (or (not (is-obj-stat3 obj +item3-req-mage+)) (is-mage ch))
          (or (not (is-obj-stat3 obj +item3-req-cleric+)) (is-cleric ch))
          (or (not (is-obj-stat3 obj +item3-req-thief+)) (is-thief ch))
          (or (not (is-obj-stat3 obj +item3-req-warrior+)) (is-warrior ch))
          (or (not (is-obj-stat3 obj +item3-req-barb+)) (is-barb ch))
          (or (not (is-obj-stat3 obj +item3-req-psionic+)) (is-psionic ch))
          (or (not (is-obj-stat3 obj +item3-req-physic+)) (is-physic ch))
          (or (not (is-obj-stat3 obj +item3-req-cyborg+)) (is-cyborg ch))
          (or (not (is-obj-stat3 obj +item3-req-knight+)) (is-knight ch))
          (or (not (is-obj-stat3 obj +item3-req-ranger+)) (is-ranger ch))
          (or (not (is-obj-stat3 obj +item3-req-bard+)) (is-bard ch))
          (or (not (is-obj-stat3 obj +item3-req-monk+)) (is-monk ch))
          (or (not (is-obj-stat3 obj +item3-req-vampire+)) (is-vampire ch))
          (or (not (is-obj-stat3 obj +item3-req-mercenary+)) (is-merc ch))
          (or (not (is-obj-stat3 obj +item3-req-spare1+)) (is-spare1 ch))
          (or (not (is-obj-stat3 obj +item3-req-spare2+)) (is-spare2 ch))
          (or (not (is-obj-stat3 obj +item3-req-spare3+)) (is-spare3 ch)))
     nil)
    (t
     t)))

(defparameter +prac-params+
  #2A(
  ;; MG  CL  TH  WR  BR  PS  PH  CY  KN  RN  BD  MN  VP  MR  S1  S2  S3
	(75  75  70  70  65  75  75  80  75  75  80  75  75  70  70  70  70)
	(25  20  20  20  20  25  20  30  20  25  30  20  15  25  25  25  25)
	(15  15  10  15  10  15  15  15  15  15  15  10  10  10  10  10  10)
	(spl spl skl skl skl trg alt prg spl spl sng zen spl skl skl skl skl)))

(defparameter +hitp-gain-table+
  #2A(
  ;; MG  CL  TH  WR  BR  PS  PH  CY  KN  RN  BD  MN  VP  MR  S1  S2  S3
	( 5   2   3   1   1   3   4   2   1   1   3   3   2   1   2   2   2)
	( 3   5   4  10  13   3   4   6   7   4   5   6   5   6   5   5   5)
	(10  11  10  15  18   8   9  14  13  11  10  12  16  14  16  16  16)))

(defparameter +mana-gain-table+
  #2A(
  ;; MG  CL  TH  WR  BR  PS  PH  CY  KN  RN  BD  MN  VP  MR  S1  S2  S3
	( 3   5   1   1   1   5   3  15  15   8   3  22   4   1   4   4   4)
	( 1   1   1   1   1   1   1   1   1   1   5   1   1   1   1   1   1)
	(11  10   1   5   1   7   6   2   4   6  10   2  13   1  13  13  13)))

(defparameter +move-gain-table+
  #2A(
  ;; MG  CL  TH  WR  BR  PS  PH  CY  KN  RN  BD  MN  VP  MR  S1  S2  S3
	( 1   1   2   5   5   2   2   5   3   6   3   6   7   3   7   7   7)
	( 3   4   6  10  10   6  10   8   8  14   5   9  15   9  15  15  15)))

(defun learned (ch)
  (if (is-remort ch)
      (+ (max (aref +prac-params+ 0 (min (1- +num-classes+) (char-class-of ch)))
              (aref +prac-params+ 0 (min (1- +num-classes+) (remort-char-class-of ch))))
         (* (remort-gen-of ch) 2))
      (aref +prac-params+ 0 (min (1- +num-classes+) (char-class-of ch)))))

(defun gain-skill-proficiency (ch skill)
  (let ((learned (if (or (= skill +skill-read-scrolls+)
                         (= skill +skill-use-wands+))
                     10
                     (learned ch))))
    (when (and
           ;; Mobiles don't learn skills
           (not (is-npc ch))
           ;; You can't gain in a skill that you don't really know
           (or (< (level-of ch) (spell-level skill (char-class-of ch)))
               (and (is-remort ch)
                    (< (level-of ch) (spell-level skill (remort-char-class-of ch)))))
           ;; Check for remort skills too
           (or (zerop (spell-gen skill (char-class-of ch)))
               (< (remort-gen-of ch) (spell-gen skill (char-class-of ch))))
           (>= (skill-of ch skill) (- learned 10))
           (<= (- (skill-of ch skill) (level-of ch)) 66)
           (zerop (random-range 0 (level-of ch))))
      (incf (skill-of ch skill)))))

(defun roll-real-abils (ch)
  "Roll the six stats for a character.  Each stat is balanced so that their total adds up to 72 before modifiers.  The stats are then sorted according to character class."
  (let ((stats (make-array 6 :initial-element 12)))

    (dotimes (i 24)
      ;; increment a random stat
      (loop for stat-idx = (random-range 0 5)
           while (= (aref stats stat-idx) 18)
           finally (incf (aref stats stat-idx)))
      ;; then decrement a random stat
      (loop for stat-idx = (random-range 0 5)
           while (= (aref stats stat-idx) 3)
           finally (decf (aref stats stat-idx))))

    (setf stats (sort stats #'<))

    (let ((priorities (cond
                        ((= (char-class-of ch) +class-magic-user+)
                         '(:int :wis :dex :cha :con :str))
                        ((= (char-class-of ch) +class-cleric+)
                         '(:wis :int :str :dex :con :cha))
                        ((= (char-class-of ch) +class-thief+)
                         '(:dex :str :con :int :wis :cha))
                        ((= (char-class-of ch) +class-warrior+)
                         '(:con :str :dex :wis :int :cha))
                        ((= (char-class-of ch) +class-barb+)
                         '(:str :con :dex :wis :int :cha))
                        ((= (char-class-of ch) +class-psionic+)
                         '(:int :wis :dex :str :con :cha))
                        ((= (char-class-of ch) +class-physic+)
                         '(:int :wis :dex :str :con :cha))
                        ((and (= (char-class-of ch) +class-cyborg+)
                              (= (old-char-class-of ch) +borg-mentant+))
                         '(:int :str :dex :con :wis :cha))
                        ((and (= (char-class-of ch) +class-cyborg+)
                              (= (old-char-class-of ch) +borg-speed+))
                         '(:dex :con :str :int :wis :cha))
                        ((= (char-class-of ch) +class-cyborg+)
                         '(:str :con :dex :int :wis :cha))
                        ((= (char-class-of ch) +class-knight+)
                         '(:str :con :wis :dex :int :cha))
                        ((= (char-class-of ch) +class-ranger+)
                         '(:dex :wis :con :str :int :cha))
                        ((= (char-class-of ch) +class-monk+)
                         '(:dex :con :str :wis :int :cha))
                        ((= (char-class-of ch) +class-mercenary+)
                         '(:str :con :dex :int :wis :cha))
                        ((= (char-class-of ch) +class-bard+)
                         '(:cha :dex :str :con :wis :int))
                        (t
                         '(:dex :con :str :wis :int :cha)))))
      (loop
         for stat in priorities
         as idx from 0
         do (case stat
              (:str
               (setf (str-of ch) (aref stats idx)))
              (:int
               (setf (int-of ch) (aref stats idx)))
              (:wis
               (setf (wis-of ch) (aref stats idx)))
              (:dex
               (setf (dex-of ch) (aref stats idx)))
              (:con
               (setf (con-of ch) (aref stats idx)))
              (:cha
               (setf (cha-of ch) (aref stats idx)))))))
  (cond
    ((or (eql (race-of ch) +race-elf+)
         (eql (race-of ch) +race-drow+))
     (incf (int-of ch))
     (incf (dex-of ch))
     (decf (con-of ch)))
    ((eql (race-of ch) +race-halfling+)
     (decf (str-of ch) 2)
     (incf (dex-of ch) 2))
    ((eql (race-of ch) +race-dwarf+)
     (incf (con-of ch))
     (incf (str-of ch))
     (decf (cha-of ch)))
    ((eql (race-of ch) +race-half-orc+)
     (incf (str-of ch) 2)
     (incf (con-of ch))
     (decf (cha-of ch) 3))
    ((eql (race-of ch) +race-orc+)
     (incf (str-of ch))
     (incf (con-of ch))
     (decf (int-of ch))
     (decf (wis-of ch) 2)
     (decf (cha-of ch) 3))
    ((eql (race-of ch) +race-tabaxi+)
     (incf (dex-of ch) 3)
     (incf (con-of ch))
     (decf (int-of ch))
     (decf (wis-of ch) 3)
     (decf (cha-of ch) 2))
    ((eql (race-of ch) +race-minotaur+)
     (incf (str-of ch) 3)
     (incf (con-of ch) 2)
     (decf (wis-of ch) 3)
     (decf (int-of ch) 2)
     (decf (cha-of ch) 2))
    ((eql (race-of ch) +race-human+)    ; human
     (cond
       ((eql (char-class-of ch) +class-magic-user+)
        (incf (int-of ch))
        (incf (dex-of ch)))
       ((eql (char-class-of ch) +class-cleric+)
        (incf (int-of ch))
        (incf (wis-of ch)))
       ((eql (char-class-of ch) +class-barb+)
        (incf (str-of ch))
        (incf (con-of ch)))
       ((eql (char-class-of ch) +class-ranger+)
        (incf (int-of ch))
        (incf (dex-of ch)))
       ((eql (char-class-of ch) +class-thief+)
        (incf (int-of ch))
        (incf (dex-of ch)))
       ((eql (char-class-of ch) +class-knight+)
        (incf (str-of ch))
        (incf (wis-of ch)))
       ((eql (char-class-of ch) +class-psionic+)
        (incf (int-of ch))
        (incf (wis-of ch)))
       ((eql (char-class-of ch) +class-physic+)
        (incf (int-of ch))
        (incf (wis-of ch)))
       ((and (eql (char-class-of ch) +class-cyborg+)
             (eql (old-char-class-of ch) +borg-mentant+))
        (incf (int-of ch))
        (incf (wis-of ch)))
       ((and (eql (char-class-of ch) +class-cyborg+)
             (eql (old-char-class-of ch) +borg-speed+))
        (incf (int-of ch))
        (incf (dex-of ch)))
       ((and (eql (char-class-of ch) +class-cyborg+)
             (eql (old-char-class-of ch) +borg-mentant+))
        (incf (str-of ch)))
       ((eql (char-class-of ch) +class-monk+)
        (incf (wis-of ch))
        (incf (dex-of ch)))
       ((eql (char-class-of ch) +class-mercenary+)
        (incf (str-of ch))
        (incf (dex-of ch))))))

  (setf (aff-abils-of ch) (copy-abilities (real-abils-of ch))))

(defun do-start (ch mode)
  (let ((implant-save (make-array +num-wears+))
        (tattoo-save (make-array +num-wears+))
        (new-player (and (zerop (exp-of ch)) (not (is-remort ch)))))
    ;; remove implant affects
    (dotimes (pos +num-wears+)
      (when (get-implant ch pos)
        (setf (aref implant-save pos) (unequip-char ch pos :implant t)))
      (when (get-tattoo ch pos)
        (setf (aref tattoo-save pos) (unequip-char ch pos :tattoo t))))

    (setf (level-of ch) 1)
    (setf (exp-of ch) 1)

    (when mode
      (roll-real-abils ch))

    (loop for skill from 1 upto +max-skills+ do
         (setf (aref (skills-of ch) skill) 0))
    (setf (life-points-of ch) (floor (* 3 (+ (wis-of ch) (con-of ch))) 40))

    (setf (max-hitp-of ch) 20)
    (setf (max-mana-of ch) 100)
    (setf (max-move-of ch) 82)

    (when (is-tabaxi ch)
      (setf (aref (skills-of ch)  +skill-claw+) (learned ch))
      (setf (aref (skills-of ch)  +skill-bite+) (learned ch)))

    (when (is-elf ch)
      (setf (aref (skills-of ch)  +skill-archery+) (learned ch)))

    (cond
      ((= (char-class-of ch) +class-magic-user+)
       (setf (aref (skills-of ch) +skill-punch+) (learned ch)))
      ((= (char-class-of ch) +class-cleric+)
       (setf (aref (skills-of ch) +skill-punch+) (learned ch)))
      ((= (char-class-of ch) +class-thief+)
       (setf (aref (skills-of ch) +skill-punch+) 15)
       (setf (aref (skills-of ch) +skill-sneak+) 10)
       (setf (aref (skills-of ch) +skill-hide+) 5)
       (setf (aref (skills-of ch) +skill-steal+) 15))
      ((= (char-class-of ch) +class-warrior+)
       (setf (aref (skills-of ch) +skill-punch+) 20))
      ((= (char-class-of ch) +class-barb+)
       (setf (aref (skills-of ch) +skill-punch+) 15))
      ((= (char-class-of ch) +class-psionic+)
       (setf (aref (skills-of ch) +skill-punch+) 10))
      ((= (char-class-of ch) +class-physic+)
       (setf (aref (skills-of ch) +skill-punch+) 10))
      ((= (char-class-of ch) +class-knight+)
       (setf (aref (skills-of ch) +skill-punch+) 20))
      ((= (char-class-of ch) +class-ranger+)
       (setf (aref (skills-of ch) +skill-punch+) 15)
       (incf (max-move-of ch) (dice 4 9)))
      ((= (char-class-of ch) +class-monk+)
       (setf (aref (skills-of ch) +skill-punch+) 20))
      ((= (char-class-of ch) +class-mercenary+)
       (setf (aref (skills-of ch) +skill-punch+) 20))
      ((= (char-class-of ch) +class-bard+)
       (setf (aref (skills-of ch) +skill-punch+) 25)
       (setf (aref (skills-of ch) +skill-archery+) 25)))

    (when new-player
      (cond
        ((past-class-p (char-class-of ch))
         (deposit-past-bank (account-of ch) (+ 8192
                                               (random-range 256 2048)
                                               (int-of ch)
                                               (wis-of ch)))
         (setf (gold-of ch) (+ 8192
                               (random-range 256 2048)
                               (int-of ch)
                               (wis-of ch))))
        ((future-class-p (char-class-of ch))
         (deposit-future-bank (account-of ch) (+ 8192
                                                 (random-range 256 2048)
                                                 (int-of ch)
                                                 (wis-of ch)))
         (setf (cash-of ch) (+ 8192
                               (random-range 256 2048)
                               (int-of ch)
                               (wis-of ch)))))

      (setf (title-of ch) "the complete newbie")
      (setf (played-time-of ch) 0)
      (setf (login-time-of ch) (now)))

    (advance-level ch 0)

    (incf (max-move-of ch) (con-of ch))

    (setf (hitp-of ch) (max-hitp-of ch))
    (setf (mana-of ch) (max-mana-of ch))
    (setf (move-of ch) (max-move-of ch))

    (setf (aref (conditions-of ch) +thirst+) 24)
    (setf (aref (conditions-of ch) +full+) 24)
    (setf (aref (conditions-of ch) +drunk+) 0)

    (dotimes (pos +num-wears+)
      (when (aref implant-save pos)
        (equip-char ch (aref implant-save pos) pos :implant))
      (when (aref tattoo-save pos)
        (equip-char ch (aref tattoo-save pos) pos :tattoo)))))

(defun calculate-point-gains (class con level)
  (values
   ;; hitpoint gains
   (+ (floor (getf (aref +con-app+ con) :hitp)
             (aref +hitp-gain-table+ 0 class))
      (random-range (aref +hitp-gain-table+ 1 class)
                    (aref +hitp-gain-table+ 2 class)))
   ;; mana gains
   (+ (floor level (aref +mana-gain-table+ 0 class))
      (random-range (aref +mana-gain-table+ 1 class)
                    (aref +mana-gain-table+ 2 class)))
   ;; move gains
   (random-range (aref +move-gain-table+ 0 class)
                 (aref +move-gain-table+ 1 class))))

(defun advance-level (ch keep-internal-p)
  (multiple-value-bind (add-hitp add-mana add-move)
      (calculate-point-gains (char-class-of ch) (con-of ch) (level-of ch))
    (incf (max-hitp-of ch) (max 1 add-hitp))
    (incf (max-mana-of ch) (max 1 add-mana))
    (incf (max-move-of ch) (max 1 add-move)))

  (when (is-remort ch)
    (multiple-value-bind (add-hitp add-mana add-move)
        (calculate-point-gains (remort-char-class-of ch) (con-of ch) (level-of ch))
      (incf (max-hitp-of ch) (max 1 (floor add-hitp 4)))
      (incf (max-mana-of ch) (max 1 (floor add-mana 2)))
      (incf (max-move-of ch) (max 1 (floor add-move 4)))))

  (incf (life-points-of ch) (floor (* (level-of ch) (+ (wis-of ch) (con-of ch))) 300))

  (when (>= (level-of ch) +lvl-ambassador+)
    (dotimes (i 3)
      (setf (aref (conditions-of ch) i) -1))
    (setf (bitp (prefs-of ch) +pref-holylight+) t)
    (setf (bitp (prefs-of ch) +pref-nohassle+) t))

  (when (= (level-of ch) 10)
    (setf (bitp (prefs-of ch) +pref-newbie-helper+) t))

  (save-player-to-xml ch)

  (let ((msg (format nil "~a advanced to level ~d~:[~*~; in room ~d~]~:[~; <TESTER>~]"
                     (name-of ch)
                     (level-of ch)
                     (in-room-of ch)
                     (number-of (in-room-of ch))
                     (testerp ch))))
    (when keep-internal-p
      (slog "~a" msg)
      (mudlog 'info t "~a" msg))))
