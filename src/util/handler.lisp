(in-package #:tempus)

(defun is-name (str namelist)
  (find str
        (split-sequence #\space namelist :remove-empty-subseqs t)
        :test #'string-abbrev))

(defun apply-object-affects (ch obj addp)
  (when (and obj
             (or (eql (worn-by-of obj) ch)
                 (and (in-obj-of obj)
                      (is-interface (in-obj-of obj))
                      (= (interface-type (in-obj-of obj)) +interface-chips+)
                      (is-chip obj)))
             (not (invalid-char-class ch obj))
             (not (or (and (is-obj-stat obj +item-anti-good+) (is-good ch))
                      (and (is-obj-stat obj +item-anti-evil+) (is-evil ch))
                      (and (is-obj-stat obj +item-anti-neutral+) (is-neutral ch))))
             (or (not (eql obj (aref (equipment-of ch) (worn-on-of obj)))))
                 (and (not (is-implant obj))
                      (not (and (= (worn-on-of obj) +wear-belt+)
                                (or (= (kind-of obj) +item-weapon+)
                                    (= (kind-of obj) +item-pipe+))))))
             (not (and (is-device obj)
                       (zerop (engine-state obj)))))

    ;; Apply object affects
    (dotimes (j +max-obj-affect+)
      (let ((af (aref (affected-of obj) j)))
        (affect-modify ch (location-of af) (modifier-of af) 0 0 addp)))
    (dotimes (j 3)
      (affect-modify ch 0 0 (aref (bitvector-of obj) j) (1+ j) addp))

    ;; Special skill chip affects
    (when (and (is-chip obj)
               (skillchip obj)
               (< 0 (chip-data obj) +max-skills+))
        (affect-modify ch (- (chip-data obj)) (chip-max obj) 0 0 addp))

    ;; Apply affects of contained chips, if this is a chip interface
    (when (and (is-interface obj)
               (eql (interface-type obj) +interface-chips+))
      (dolist (chip (contains-of obj))
        (apply-object-affects ch chip addp))))

(defun apply-skill (ch skill mod)
  (when (not (is-npc ch))
    (setf (aref (skills-of ch) skill)
          (min (+ (aref (skills-of ch) skill) mod) 125))))

(defun affect-modify (ch loc mod bitv index addp)
  (unless (zerop bitv)
    ;; Handle bitvectors
    (if addp
        ;; Set bitvectors on character
        (case index
          (1
           (when (and (= bitv +aff-glowlight+)
                      (in-room-of ch)
                      (not (aff-flagged ch +aff-glowlight+))
                      (not (aff2-flagged ch (logior +aff2-fluorescent+
                                                    +aff2-divine-illumination+)))
                      (not (affected-by-spell ch +spell-quad-damage+)))
             (incf (light-of (in-room-of ch))))
           (setf (aff-flags-of ch) (logior (aff-flags-of ch) bitv)))
          (2
           (when (and (in-room-of ch)
                      (or (= bitv +aff2-fluorescent+)
                          (= bitv +aff2-divine-illumination+))
                      (not (aff-flagged ch +aff-glowlight+))
                      (not (aff2-flagged ch (logior +aff2-fluorescent+
                                                    +aff2-divine-illumination+)))
                      (not (affected-by-spell ch +spell-quad-damage+)))
             (incf (light-of (in-room-of ch))))
           (setf (aff2-flags-of ch) (logior (aff2-flags-of ch) bitv)))
          (3
           (setf (aff3-flags-of ch) (logior (aff3-flags-of ch) bitv))))
        ;; Clear bitvectors on character
        (case index
          (1
           (setf (aff-flags-of ch) (logandc2 (aff-flags-of ch) bitv))
           (when (and (in-room-of ch)
                      (= bitv +aff-glowlight+)
                      (not (aff-flagged ch +aff-glowlight+))
                      (not (aff2-flagged ch (logior +aff2-fluorescent+
                                                    +aff2-divine-illumination+)))
                      (not (affected-by-spell ch +spell-quad-damage+)))
             (decf (light-of (in-room-of ch)))))
          (2
           (setf (aff2-flags-of ch) (logandc2 (aff2-flags-of ch) bitv))
           (when (and (in-room-of ch)
                      (or (= bitv +aff2-fluorescent+)
                          (= bitv +aff2-divine-illumination+))
                      (not (aff-flagged ch +aff-glowlight+))
                      (not (aff2-flagged ch (logior +aff2-fluorescent+
                                                    +aff2-divine-illumination+)))
                      (not (affected-by-spell ch +spell-quad-damage+)))
             (decf (light-of (in-room-of ch)))))
          (3
           (setf (aff3-flags-of ch) (logandc2 (aff3-flags-of ch) bitv))))))

  (when (not addp)
    (setf mod (- mod)))

  (cond
    ((zerop loc)
     ;; do nothing
     nil)
    ((minusp loc)
     (setf loc (- loc))
     (when (and (< loc +max-skills+) (not (is-npc ch)))
       (apply-skill ch loc mod)))
    ((= loc +apply-str+)
     (incf (str-of (aff-abils-of ch)) mod)
     (incf (str-of (aff-abils-of ch)) (floor (str-add-of (aff-abils-of ch)) 10))
     (setf (str-add-of (aff-abils-of ch)) 0))
    ((= loc +apply-dex+)
     (incf (dex-of (aff-abils-of ch)) mod))
    ((= loc +apply-int+)
     (incf (int-of (aff-abils-of ch)) mod))
    ((= loc +apply-wis+)
     (incf (wis-of (aff-abils-of ch)) mod))
    ((= loc +apply-con+)
     (incf (con-of (aff-abils-of ch)) mod))
    ((= loc +apply-cha+)
     (incf (cha-of (aff-abils-of ch)) mod))
    ((= loc +apply-age+)
     (incf (age-adjust-of ch) mod))
    ((= loc +apply-char-weight+)
     (incf (weight-of ch) mod))
    ((= loc +apply-char-height+)
     (incf (height-of ch) mod))
    ((= loc +apply-mana+)
     (incf (max-mana-of ch) mod))
    ((= loc +apply-hit+)
     (incf (max-hitp-of ch) mod))
    ((= loc +apply-move+)
     (incf (max-move-of ch) mod))
    ((= loc +apply-ac+)
     (incf (armor-of ch) mod))
    ((= loc +apply-hitroll+)
     (setf (hitroll-of ch) (pin (+ (hitroll-of ch) mod) -125 125)))
    ((= loc +apply-damroll+)
     (setf (damroll-of ch) (pin (+ (damroll-of ch) mod) -125 125)))
    ((= loc +apply-saving-para+)
     (incf (aref (saves-of ch) +saving-para+) mod))
    ((= loc +apply-saving-rod+)
     (incf (aref (saves-of ch) +saving-rod+) mod))
    ((= loc +apply-saving-petri+)
     (incf (aref (saves-of ch) +saving-petri+) mod))
    ((= loc +apply-saving-breath+)
     (incf (aref (saves-of ch) +saving-breath+) mod))
    ((= loc +apply-saving-spell+)
     (incf (aref (saves-of ch) +saving-spell+) mod))
    ((= loc +apply-saving-chem+)
     (incf (aref (saves-of ch) +saving-chem+) mod))
    ((= loc +apply-saving-psi+)
     (incf (aref (saves-of ch) +saving-psi+) mod))
    ((= loc +apply-saving-phy+)
     (incf (aref (saves-of ch) +saving-phy+) mod))
    ((= loc +apply-sneak+)
     (apply-skill ch +skill-sneak+ mod))
    ((= loc +apply-hide+)
     (apply-skill ch +skill-hide+ mod))
    ((= loc +apply-race+)
     (when (<= 0 mod (1- +num-races+))
       (setf (race-of ch) mod)))
    ((= loc +apply-sex+)
     (when (<= 0 mod 2)
       (setf (sex-of ch) mod)))
    ((= loc +apply-backstab+)
     (apply-skill ch +skill-backstab+ mod))
    ((= loc +apply-pick-lock+)
     (apply-skill ch +skill-pick-lock+ mod))
    ((= loc +apply-punch+)
     (apply-skill ch +skill-punch+ mod))
    ((= loc +apply-shoot+)
     (apply-skill ch +skill-shoot+ mod))
    ((= loc +apply-kick+)
     (apply-skill ch +skill-kick+ mod))
    ((= loc +apply-track+)
     (apply-skill ch +skill-track+ mod))
    ((= loc +apply-impale+)
     (apply-skill ch +skill-impale+ mod))
    ((= loc +apply-behead+)
     (apply-skill ch +skill-behead+ mod))
    ((= loc +apply-throwing+)
     (apply-skill ch +skill-throwing+ mod))
    ((= loc +apply-riding+)
     (apply-skill ch +skill-riding+ mod))
    ((= loc +apply-turn+)
     (apply-skill ch +skill-turn+ mod))
    ((= loc +apply-align+)
     (setf (alignment-of ch) (pin (+ (alignment-of ch) mod) -1000 1000)))
    ((= loc +apply-nothirst+)
     (when (and (not (is-npc ch))
                (/= (aref (condition-of ch) +thirst+) -1))
       (setf (aref (condition-of ch) +thirst+)
             (if (plusp mod) -2 0))))
    ((= loc +apply-nohunger+)
     (when (and (not (is-npc ch))
                (/= (aref (condition-of ch) +full+) -1))
       (setf (aref (condition-of ch) +full+)
             (if (plusp mod) -2 0))))
    ((= loc +apply-nodrunk+)
     (when (and (not (is-npc ch))
                (/= (aref (condition-of ch) +drunk+) -1))
       (setf (aref (condition-of ch) +drunk+)
             (if (plusp mod) -1 0))))
    ((= loc +apply-speed+)
     (incf (speed-of ch) mod))
    ((member loc (list +apply-caster+
                       +apply-weaponspeed+
                       +apply-disguise+))
     ;; These do nothing
     nil)
    (t
     (error "Unknown apply adjust attempt on ~a ~d + ~d in affect_modify add=~a"
            (name-of ch) loc mod addp))))

(defun affect-total (ch)
  "Updates a character by subtracting everything it is affected by, restoring original abilities, and then affecting it all again."
  (when (> (str-of (aff-abils-of ch)) 18)
    (incf (str-of (aff-abils-of ch)) 10))

  ;; Remove all item-based affects
  (dotimes (i +num-wears+)
    ;; Remove equipment affects
    (when (get-eq ch i)
      (apply-object-affects ch (get-eq ch i) nil))
    (when (aref (implants-of ch) i)
      (apply-object-affects ch (aref (implants-of ch) i) nil))
    (when (aref (tattoos-of ch) i)
      (apply-object-affects ch (aref (tattoos-of ch) i) nil)))

  ;; remove all spell affects
  (dolist (af (affected-of ch))
    (affect-modify ch (location-of af) (modifier-of af) (bitvector-of af)
                   (aff-index-of af) nil))

  ;; Set stats to real stats
  (setf (aff-abils-of ch) (copy-abilities (real-abils-of ch)))

  (when (> (str-of (aff-abils-of ch)) 18)
    (incf (str-of (aff-abils-of ch)) 10))

  (dotimes (i 10)
    (setf (aref (saves-of ch) i) 0))

  (cond
    ((and (typep ch 'mobile) (proto-of (shared-of ch)))
     (setf (hitroll-of ch) (hitroll-of (proto-of (shared-of ch))))
     (setf (damroll-of ch) (damroll-of (proto-of (shared-of ch)))))
    (t
     (setf (hitroll-of ch) 0)
     (setf (damroll-of ch) 0)))

  (setf (speed-of ch) 0)

  ;; Reset affected stats

  ;; Re-apply all item-based affects
  (dotimes (i +num-wears+)
    (when (get-eq ch i)
      (apply-object-affects ch (get-eq ch i) t))
    (when (aref (implants-of ch) i)
      (apply-object-affects ch (aref (implants-of ch) i) t))
    (when (aref (tattoos-of ch) i)
      (apply-object-affects ch (aref (tattoos-of ch) i) t)))

  (dolist (af (affected-of ch))
    (affect-modify ch (location-of af) (modifier-of af) (bitvector-of af)
                   (aff-index-of af) t))

  ;; Make certain values are between 0..25, not < 0 and not > 25!
  (let ((max-dex (if (is-npc ch) 25
                     (min 25 (+ 18
                                (if (is-remort ch) (remort-gen-of ch) 0)
                                (if (is-tabaxi ch) 2 0)
                                (if (or (is-elf ch) (is-drow ch)) 1 0)))))
        (max-int (if (is-npc ch) 25
                     (min 25 (+ 18
                                (if (is-remort ch) (remort-gen-of ch) 0)
                                (if (or (is-elf ch) (is-drow ch)) 1 0)
                                (if (is-minotaur ch) -2 0)
                                (if (is-tabaxi ch) -1 0)
                                (if (is-orc ch) -1 0)
                                (if (is-half-orc ch) -1 0)))))
        (max-wis (if (is-npc ch) 25
                     (min 25 (+ 18
                                (if (is-remort ch) (remort-gen-of ch) 0)
                                (if (is-minotaur ch) -2 0)
                                (if (is-tabaxi ch) -1 0)
                                (if (is-orc ch) -1 0)
                                (if (is-half-orc ch) -1 0)))))
        (max-con (if (is-npc ch) 25
                     (min 25 (+ 18
                                (if (is-remort ch) (remort-gen-of ch) 0)
                                (if (or (is-minotaur ch) (is-dwarf ch)) 1 0)
                                (if (is-tabaxi ch) 1 0)
                                (if (is-half-orc ch) 1 0)
                                (if (is-orc ch) 2 0)
                                (if (or (is-elf ch) (is-drow ch)) -1 0)))))
        (max-cha (if (is-npc ch) 25
                     (min 25 (+ 18
                                (if (is-remort ch) (remort-gen-of ch) 0)
                                (if (is-half-orc ch) -3 0)
                                (if (is-orc ch) -3 0)
                                (if (is-dwarf ch) -1 0)
                                (if (is-tabaxi ch) -2 0))))))
    (setf (dex-of (aff-abils-of ch)) (pin (dex-of (aff-abils-of ch)) 1 max-dex))
    (setf (int-of (aff-abils-of ch)) (pin (int-of (aff-abils-of ch)) 1 max-int))
    (setf (wis-of (aff-abils-of ch)) (pin (wis-of (aff-abils-of ch)) 1 max-wis))
    (setf (con-of (aff-abils-of ch)) (pin (con-of (aff-abils-of ch)) 1 max-con))
    (setf (cha-of (aff-abils-of ch)) (pin (cha-of (aff-abils-of ch)) 1 max-cha))
    (setf (str-of (aff-abils-of ch)) (max (str-of (aff-abils-of ch)) 1)))

  ;; Make sure that hit !> max-hit, etc
  (setf (hitp-of ch) (min (max-hitp-of ch) (hitp-of ch)))
  (setf (mana-of ch) (min (max-mana-of ch) (mana-of ch)))
  (setf (move-of ch) (min (max-move-of ch) (move-of ch)))

  (let ((i (+ (str-of (aff-abils-of ch)) (floor (str-add-of (aff-abils-of ch)) 10))))
    (cond
      ((<= i 18)
       (setf (str-add-of (aff-abils-of ch)) 0))
      ((<= i 28)
       (setf (str-of (aff-abils-of ch)) 18)
       (setf (str-add-of (aff-abils-of ch)) (* (- i 18) 10)))
      ((or (is-remort ch)
           (is-minotaur ch)
           (is-npc ch)
           (is-half-orc ch)
           (is-dwarf ch)
           (is-orc ch)
           (immortal-level-p ch))
       (setf (str-of (aff-abils-of ch)) (min 25
                              (- (+ (str-of (aff-abils-of ch)) (floor (str-add-of (aff-abils-of ch)) 10)) 10)
                              (+ 18
                                 (remort-gen-of ch)
                                 (if (or (is-npc ch) (immortal-level-p ch)) 8 0)
                                 (if (is-minotaur ch) 2 0)
                                 (if (is-dwarf ch) 1 0)
                                 (if (is-half-orc ch) 2 0)
                                 (if (is-orc ch) 1 0))))
       (setf (str-add-of (aff-abils-of ch)) 0))
      (t
       (setf (str-of (aff-abils-of ch)) 18)
       (setf (str-add-of (aff-abils-of ch)) 100)))))

(defun char-to-room (ch room &optional check-specials)
  (assert (null(in-room-of ch)) nil "creature already in a room in char-to-room!")
  (assert (and ch room) nil "Illegal values passed to char-to-room")

  (push ch (people-of room))
  (setf (in-room-of ch) room)
  (when (and (eql (race-of ch) +race-elemental+)
                    (eql (class-of ch) +class-fire+))
    (incf (light-of room)))

  (let ((light-eq (get-eq ch +wear-light+)))
    (when (and light-eq
               (eql (kind-of light-eq) +item-light+)
               (plusp (aref (val-of light-eq) 2)))
      (incf (light-of room))))

  (when (or (aff-flagged ch +aff-glowlight+)
            (aff2-flagged ch +aff2-fluorescent+)
            (aff2-flagged ch +aff2-divine-illumination+)
            (affected-by-spell ch +spell-quad-damage+))
    (incf (light-of room)))

  (unless (is-npc ch)
    (incf (num-players-of (zone-of room)))
    (setf (idle-time-of (zone-of room)) 0)))

(defun zone-number (vnum)
  (floor vnum 100))

(defun obj-to-char (obj ch &optional sorted)
  "give an object to a char"
  (assert obj nil "NIL obj passed to obj-to-char")
  (assert ch nil "NIL ch passed to obj-to-char")

  (push obj (carrying-of ch))
  (when sorted
    (setf (carrying-of ch) (sort (carrying-of ch) #'vnum-of)))

  (setf (carried-by-of obj) ch)
  (incf (carry-weight-of ch) (weight-of obj))
  (incf (carry-items-of ch))

  (when (and (= (kind-of obj) +item-key+)
             (zerop (aref (value-of obj) 1))
             (not (is-npc ch))
             (not (immortal-level-p ch))
             (zerop (timer-of obj)))
    (let ((zone (real-zone (zone-number (vnum-of obj)))))
      (setf (timer-of obj)
            (if zone (max 2 (floor (lifespan-of zone) 2)) 15)))))

(defun char-from-room (ch)
  (setf (people-of (in-room-of ch)) (delete ch (people-of (in-room-of ch))))
  (setf (in-room-of ch) nil))

(defun obj-to-room (obj room)
  (assert (null (in-room-of obj)) nil 'invalid-obj-to-room obj)
  (push obj (contents-of room))
  (setf (in-room-of obj) room))

(defun obj-from-room (obj)
  (assert (in-room-of obj) nil 'invalid-obj-from-room obj)
  (setf (contents-of (in-room-of obj))
        (delete obj (contents-of (in-room-of obj))))
  (setf (in-room-of obj) nil))

(defun obj-to-obj (obj obj-to &optional sorted)
  "put an object in an object (quaint)"

  (assert obj nil "Illegal NIL object")
  (assert obj-to nil "Illegal NIL target object")
  (assert (not (eql obj obj-to)) nil "object is eql to target object")

  (push obj (contains-of obj-to))
  (setf (in-obj-of obj) obj-to)

  (when sorted
    (setf (contains-of obj-to) (sort (contains-of obj-to) 'vnum-of)))

  ;; top level object. Subtract weight from inventory if necessary.
  (incf (weight-of obj-to) (weight-of obj))

  (when (and (in-room-of obj-to)
             (room-flagged (in-room-of obj-to) +room-house-crash+))
    (setf (flags-of (in-room-of obj-to)) (logior (flags-of (in-room-of obj-to)) +room-house-crash+)))

  (when (is-interface obj-to)
    (let ((vict (worn-by-of obj-to)))
      (when (and vict
                 (or (not (eql obj-to (get-eq vict (worn-on-of obj-to))))
                     (/= (worn-on-of obj-to) +wear-belt+)
                     (and (not (eql (kind-of obj) +item-weapon+))
                          (not (eql (kind-of obj) +item-pipe+))))
                 (not (invalid-char-class vict obj))
                 (or (not (eql obj-to (get-eq vict (worn-on-of obj-to))))
                     (not (is-implant obj))))

        (when (and (skillchip obj)
                   (plusp (chip-data obj))
                   (< (chip-data obj) +max-skills+))
          (affect-modify vict (- (chip-data obj)) (chip-max obj) 0 0 t))
        (apply-object-affects vict obj t)))))

(defun obj-from-obj (obj)
  "Remove an object from an object"
  (assert (in-obj-of obj) nil "Trying to extract object from object")
  (let ((obj-from (in-obj-of obj)))
    (decf (weight-of obj-from) (weight-of obj))
    (setf (contains-of obj-from) (delete obj (contains-of obj-from)))

    (when (is-interface obj-from)
      (let ((vict (worn-by-of obj-from)))
        (when (and vict
                   (or (not (eql obj-from (get-eq vict (worn-on-of obj-from))))
                       (/= (worn-on-of obj-from) +wear-belt+)
                       (and (not (eql (kind-of obj) +item-weapon+))
                            (not (eql (kind-of obj) +item-pipe+))))
                   (not (invalid-char-class vict obj))
                   (or (not (eql obj-from (get-eq vict (worn-on-of obj-from))))
                       (not (is-implant obj))))

      (when (and (skillchip obj)
                 (plusp (chip-data obj))
                 (< (chip-data obj) +max-skills+))
        (affect-modify vict (- (chip-data obj)) (chip-max obj) 0 0 nil))
      (apply-object-affects vict obj nil))))

    (when (and (in-room-of obj-from)
               (room-flagged (in-room-of obj-from) +room-house+))
      (setf (flags-of (in-room-of obj-from))
            (logior (in-room-of obj-from) +room-house-crash+)))

    (setf (in-obj-of obj) nil)

    (setf (extra2-flags-of obj) (logandc2 (extra2-flags-of obj) +item2-hidden+))
    obj))

(defun printbits (bits descriptions)
  (format nil "~{~a~^ ~}"
          (loop for idx from 0
             for descrip across descriptions
             when (logtest bits (ash 1 idx))
             collect descrip)))

(defun is-alias-of (str alias)
  (let ((aliases (cl-ppcre:split #/\s+/ alias)))
    (member str aliases :test #'string-abbrev)))

(defun resolve-alias (ch str)
  (find str (remove-if-not (lambda (i)
                             (can-see-creature ch i))
                           (people-of (in-room-of ch)))
        :key 'aliases-of
        :test 'is-alias-of))

(defun get-obj-in-list-num (num list)
  "Search a given list for an object number, and return a ptr to that obj."
  (find num list :key (lambda (obj)
                        (vnum-of (shared-of obj)))))

(defun obj-from-char (obj)
  (assert obj nil "NIL object passed to obj-from-char")
  (assert (carried-by-of obj) nil "NIL carried by")

  (let ((ch (carried-by-of obj)))
  (setf (carrying-of ch) (delete obj (carrying-of ch)))

  ;; set flag for crash-save system
  (unless (is-npc ch)
    (setf (plr-bits-of ch) (logior (plr-bits-of ch) +plr-crash+)))

  (decf (carry-weight-of ch) (weight-of obj))
  (decf (carry-items-of ch))
  (when (and (aux-obj-of obj)
             (or (eql (kind-of obj) +item-scuba-mask+)
                 (eql (kind-of obj) +item-scuba-tank+)))
    (setf (aux-obj-of (aux-obj-of obj)) nil)
    (setf (aux-obj-of obj) nil))

  (setf (carried-by-of obj) nil)
  obj))


(defun apply-ac (ch eq-pos)
  (let ((obj (get-eq ch eq-pos)))
    (assert obj nil "NIL eq at eq_pos")
    (if (eql (kind-of obj) +item-armor+)
        (cond
          ((= eq-pos +wear-body+)
           (* 3 (aref (value-of obj) 0)))
          ((or (= eq-pos +wear-head+)
               (= eq-pos +wear-legs+))
           (* 2 (aref (value-of obj) 0)))
          (t
           (aref (value-of obj) 0)))
        0)))


(defun equip-char (ch obj pos mode)
  (assert (<= 0 pos +num-wears+) nil "Illegal pos in equip-char")
  (assert (not (carried-by-of obj)) nil "Object is carried-by when equipped")
  (assert (not (in-room-of obj)) nil "Object is in-room when equipped")
  (ecase mode
    (:worn
     (assert (not (get-eq ch pos)) nil "~a is already equipped: ~a"
             (name-of ch)
             (name-of obj))
     (setf (aref (equipment-of ch) pos) obj)
     (when (eql (kind-of obj) +item-armor+)
       (decf (armor-of ch) (apply-ac ch pos)))
     (incf (worn-weight-of ch) (weight-of obj))
     (when (and (in-room-of ch)
                (= pos +wear-light+)
                (eql (kind-of obj) +item-light+)
                (plusp (aref (value-of obj) 2))) ; if light is ON
       (incf (light-of (in-room-of ch)))))
    (:implant
     (assert (not (aref (implants-of ch) pos)) nil "~a is already implanted: ~a"
             (name-of ch)
             (name-of obj))
     (setf (aref (implants-of ch) pos) obj)
     (incf (weight-of ch) (weight-of obj)))
    (:tattoo
     (assert (not (aref (tattoos-of ch) pos)) nil "~a is already tattooed: ~a"
             (name-of ch)
             (name-of obj))
     (setf (aref (tattoos-of ch) pos) obj)))

  (setf (worn-by-of obj) ch)
  (setf (worn-on-of obj) pos)

  (apply-object-affects ch obj t)

  (affect-total ch))

(defun unequip-char (ch pos mode disable-checks)
  (assert (< 0 pos +num-wears+) nil "Illegal pos in unequip-char")
  (let ((obj nil))
    (ecase mode
      (:worn
       (assert (get-eq ch pos) nil
               "eq NIL at pos ~d" pos)
       (setf obj (get-eq ch pos))
       (when (= (kind-of obj) +item-armor+)
         (incf (armor-of ch) (apply-ac ch pos)))

       (decf (worn-weight-of ch) (weight-of obj))

       (when (and (in-room-of ch)
                  (= pos +wear-light+)
                  (= (kind-of obj) +item-light+)
                  (plusp (aref (value-of obj) 2)))
         (decf (light-of (in-room-of ch))))
       (setf (aref (equipment-of ch) pos) nil))
      (:implant
       (assert (aref (implants-of ch) pos) nil "implant NIL at pos ~d" pos)
       (setf obj (aref (implants-of ch) pos))
       (setf (aref (implants-of ch) pos) nil)
       (decf (weight-of ch) (weight-of obj)))
      (:tattoo
       (assert (aref (tattoos-of ch) pos) nil "tattoo NIL at pos ~d" pos)
       (setf obj (aref (tattoos-of ch) pos))
       (setf (aref (tattoos-of ch) pos) nil)))

    (dotimes (j +max-obj-affect+)
      (let ((af (aref (affected-of obj) j)))
        (affect-modify ch (location-of af) (modifier-of af) 0 0 nil)))
    (dotimes (j 3)
      (affect-modify ch 0 0 (aref (bitvector-of obj) j) (1+ j) nil))

    (setf (worn-by-of obj) nil)
    (setf (worn-on-of obj) -1)

    (affect-total ch)

    (when (and (not disable-checks)
               (eql mode :worn))
      (when (and (= pos +wear-waist+)
                 (get-eq ch +wear-belt+))
        (obj-to-char (unequip-char ch +wear-belt+ :worn nil) ch))
      (when (and (= pos +wear-wield+)
                 (get-eq ch +wear-wield-2+))
        (equip-char ch (unequip-char ch +wear-wield-2+ :worn nil)
                    +wear-wield+ :worn)))

    obj))

(defun extract-obj (obj)
  "Extract an object from the world"
  (cond
    ((worn-by-of obj)
     (unequip-char (worn-by-of obj)
                   (worn-on-of obj)
                   (cond
                     ((eql obj (aref (equipment-of (worn-by-of obj))
                                      (worn-on-of obj)))
                      :worn)
                     ((eql obj (aref (implants-of (worn-by-of obj))
                                      (worn-on-of obj)))
                      :implant)
                     (t
                      :tattoo))
                   nil))
    ((in-room-of obj)
     (obj-from-room obj))
    ((carried-by-of obj)
     (obj-from-char obj))
    ((in-obj-of obj)
     (obj-from-obj obj)))

  (when (aux-obj-of obj)
    (when (and (aux-obj-of (aux-obj-of obj))
               (eql (aux-obj-of (aux-obj-of obj)) obj))
      (setf (aux-obj-of (aux-obj-of obj)) nil))
    (setf (aux-obj-of obj) nil))

  (prog-unreference-object obj)

  (setf (contains-of obj) nil)

  (when (and (shared-of obj)
             (plusp (vnum-of (shared-of obj))))
    (decf (number-of (shared-of obj))))

  (setf *object-list* (delete obj *object-list*))

  (when (and (is-corpse obj)
             (plusp (corpse-idnum obj)))
    (delete-file (corpse-pathname (corpse-idnum obj)))))

(defun get-number (name)
  (cl-ppcre:register-groups-bind (number-str name-str)
      (#/(?:([+-]?\d+)\.)?(.*)/ name :sharedp t)
    (if number-str
        (let ((num (parse-integer number-str)))
          (if (plusp num)
              (values num name-str)
              nil))
        (values 1 name-str))))

(defun get-matching-objects (ch arg list)
  "Returns a list of all objects in LIST visible to CH that match ARG"
  (case (find-all-dots arg)
    (:find-all
     (loop for obj in list when (can-see-object ch obj) collect obj))
    (:find-alldot
     (loop
        with name = (subseq arg 4)
        for obj in list
        when (and (is-name name (aliases-of obj))
                  (can-see-object ch obj))
        collect obj))
    (:find-indiv
     (multiple-value-bind (number name)
         (get-number arg)
       (dolist (obj list)
         (when (and (is-name name (aliases-of obj))
                    (can-see-object ch obj)
                    (zerop (decf number)))
           (return-from get-matching-objects (list obj))))))))

(defun get-obj-in-list-vis (ch arg list)
  (multiple-value-bind (number name)
      (get-number arg)
    (dolist (obj list)
      (when (and (is-name name (aliases-of obj))
                 (can-see-object ch obj)
                 (zerop (decf number)))
        (return-from get-obj-in-list-vis obj)))))

(defun find-all-dots (arg)
  (cond
    ((string-equal arg "all")
     :find-all)
    ((< (length arg) 4)
     :find-indiv)
    ((string-equal arg "all." :end1 4)
     :find-alldot)
    (t
     :find-indiv)))