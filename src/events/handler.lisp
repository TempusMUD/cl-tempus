(in-package #:tempus)

(defmethod affected-by-spell ((ch creature) kind)
  (find (the fixnum kind) (the list (affected-of ch)) :key 'kind-of))

(defmethod affected-by-spell ((obj obj-data) kind)
  (find kind (the list (tmp-affects-of obj)) :key 'kind-of))

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
     (incf (str-of (aff-abils-of ch)) mod))
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
       (setf (sex-of ch)
             (case mod
               (0 'neuter)
               (1 'male)
               (2 'female)))))
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
    ((= loc +apply-nothirst+)
     (when (and (not (is-npc ch))
                (/= (aref (conditions-of ch) +thirst+) -1))
       (setf (aref (conditions-of ch) +thirst+)
             (if (plusp mod) -2 0))))
    ((= loc +apply-nohunger+)
     (when (and (not (is-npc ch))
                (/= (aref (conditions-of ch) +full+) -1))
       (setf (aref (conditions-of ch) +full+)
             (if (plusp mod) -2 0))))
    ((= loc +apply-nodrunk+)
     (when (and (not (is-npc ch))
                (/= (aref (conditions-of ch) +drunk+) -1))
       (setf (aref (conditions-of ch) +drunk+)
             (if (plusp mod) -1 0))))
    ((= loc +apply-speed+)
     (incf (speed-of ch) mod))
    ((member loc (list +apply-caster+
                       +apply-weaponspeed+
                       +apply-disguise+
                       +apply-class+
                       +apply-align+
                       +apply-exp+
                       +apply-gold+
                       +apply-level+
                       +apply-none+))
     ;; These do nothing
     nil)
    (t
     (error "Unknown apply adjust attempt on ~a ~d + ~d in affect_modify add=~a"
            (name-of ch) loc mod addp))))

(defun affect-total (ch)
  "Updates a character by subtracting everything it is affected by, restoring original abilities, and then affecting it all again."

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
  (setf (move-of ch) (min (max-move-of ch) (move-of ch))))

(defun affect-to-char (ch af)
  "Insert an affect type in a creature, setting appropriate bits and applies"
  (push af (affected-of ch))
  (affect-modify ch
                 (location-of af)
                 (modifier-of af)
                 (bitvector-of af)
                 (aff-index-of af)
                 t)
  (affect-total ch))

(defun affect-join (ch af
                    add-duration-p
                    average-duration-p
                    add-modifier-p
                    average-modifier-p)
  (let ((old-af (find-if (lambda (a)
                           (and (= (kind-of af) (kind-of a))
                                (= (location-of af) (location-of a))
                                (= (aff-index-of af) (aff-index-of a))))
                         (affected-of ch))))
    (cond
      (old-af
       ;; join with old affect
       (when add-duration-p
         (setf (duration-of af) (pin (+ (duration-of af)
                                        (duration-of old-af))
                                     0 666)))
       (when average-duration-p
         (setf (duration-of af) (floor (duration-of old-af))))
       (when add-modifier-p
         (setf (modifier-of af) (pin (+ (modifier-of af)
                                        (modifier-of old-af))
                                     -666 666)))
       (when average-modifier-p
         (setf (modifier-of af) (floor (modifier-of old-af))))
       (affect-remove ch old-af)
       (affect-to-char ch af))
      (t
       ;; Add fresh affect
       (affect-to-char ch af)))))


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
               (plusp (aref (value-of light-eq) 2)))
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

(defun insert-into-list (obj list &key (test 'eql) (unsorted nil))
  (cond
    ((null list)
     ;; Only element in list
     (list obj))
    (unsorted
     ;; No sorting required - just stick on the end
     (append list (list obj)))
    (t
     ;; Needs to be placed next to the same objs in the list
     (let ((found nil))
       (mapcon (lambda (o)
                 (cond
                   (found
                    (list (car o)))
                   ((null (cdr o))
                    (list (car o) obj))
                   ((funcall test (car o) obj)
                    (setf found t)
                    (list obj (car o)))
                   (t
                    (list (car o)))))
               list)))))

(defun obj-to-char (obj ch &optional unsorted)
  "give an object to a char"
  (assert obj nil "NIL obj passed to obj-to-char")
  (assert ch nil "NIL ch passed to obj-to-char")

  (setf (carrying-of ch) (insert-into-list obj (carrying-of ch) :test 'same-obj :unsorted unsorted))

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

(defun remove-combat (ch victim)
  (when (member victim (fighting-of ch))
    (setf (fighting-of ch) (delete victim (fighting-of ch)))
    (when (and (null (fighting-of ch))
               (eql (position-of ch) +pos-fighting+))
      (setf (position-of ch) +pos-standing+))))

(defun remove-all-combat (ch)
  (setf (fighting-of ch) nil)
  (when (eql (position-of ch) +pos-fighting+)
    (setf (position-of ch) +pos-standing+))
  (dolist (tch (people-of (in-room-of ch)))
    (when (eql (fighting-of tch) ch)
      (setf (fighting-of tch) nil)
      (when (eql (position-of tch) +pos-fighting+)
        (setf (position-of tch) +pos-standing+)))))

(defun char-from-room (ch check-specials-p)
  (assert (in-room-of ch) nil "NIL room in char-from-room")
  (remove-all-combat ch)

  (when (and (is-race ch +race-elemental+)
             (is-class ch +class-fire+))
    (decf (light-of (in-room-of ch))))
  (let ((light (get-eq ch +wear-light+)))
    (when (and light
               (is-obj-kind light +item-light+)
               (not (zerop (aref (value-of light) 2))))
      (decf (light-of (in-room-of ch)))))
  (when (or (aff-flagged ch +aff-glowlight+)
            (aff-flagged ch +aff2-fluorescent+)
            (aff-flagged ch +aff2-divine-illumination+)
            (affected-by-spell ch +spell-quad-damage+))
    (decf (light-of (in-room-of ch))))

  (unless (is-npc ch)
    (decf (num-players-of (zone-of (in-room-of ch)))))

  (affect-from-char ch +spell-entangle+)

  (when (typep ch 'player)
    (setf (olc-srch-of ch) nil))

  (when check-specials-p
    (check-specials 'leave ch nil nil))

  (setf (people-of (in-room-of ch)) (delete ch (people-of (in-room-of ch))))
  (setf (in-room-of ch) nil))

(defun obj-to-room (obj room &optional unsorted)
  (assert (null (in-room-of obj)) nil 'invalid-obj-to-room obj)
  (setf (contents-of room) (insert-into-list obj (contents-of room) :test 'same-obj :unsorted unsorted))
  (setf (in-room-of obj) room))

(defun obj-from-room (obj)
  (assert (in-room-of obj) nil 'invalid-obj-from-room obj)
  (setf (contents-of (in-room-of obj))
        (delete obj (contents-of (in-room-of obj))))
  (setf (in-room-of obj) nil))

(defun obj-to-obj (obj obj-to &optional unsorted)
  "put an object in an object (quaint)"

  (assert obj nil "Illegal NIL object")
  (assert obj-to nil "Illegal NIL target object")
  (assert (not (eql obj obj-to)) nil "object is eql to target object")

  (setf (contains-of obj-to) (insert-into-list obj (contains-of obj-to) :test 'same-obj :unsorted unsorted))
  (setf (in-obj-of obj) obj-to)

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

(defun printbits (bits descriptions zero-desc)
  (if (zerop bits)
      zero-desc
      (format nil "~{~a~^ ~}"
              (loop for idx from 0
                 for descrip across descriptions
                 when (logtest bits (ash 1 idx))
                 collect descrip))))

(defun printbitarray (bits descriptions)
  (format nil "~{~a~^ ~}"
          (loop for idx from 0
             for descrip across descriptions
             when (bitp bits idx)
             collect descrip)))

(defun is-alias-of (str alias &key test)
  (let ((aliases (cl-ppcre:split #/\s+/ alias)))
    (member str aliases :test (or test #'string-abbrev))))

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

(defun weapon-proficiency (ch weapon)
  (cond
    ((not (is-obj-kind weapon +item-weapon+))
     0)
    ((not (>= 0 (aref (value-of weapon) 3)
              (- +top-attacktype+ +type-hit+)))
     0)
    ((zerop (aref +weapon-proficiencies+ (aref (value-of weapon)
                                               3)))
     0)
    (t
     (skill-of ch (aref +weapon-proficiencies+ (aref (value-of weapon)
                                                     3))))))


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
  (assert (<= 0 pos (1- +num-wears+)) nil "Illegal pos in unequip-char")
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

(defun check-eq-align (ch)
  (unless (and (in-room-of ch) (not (immortalp ch)))
    (dotimes (pos +num-wears+)
      (let ((obj (aref (equipment-of ch) pos))
            (implant (aref (implants-of ch) pos)))
        (when (and implant
                   (or (and (is-good ch) (is-obj-stat implant +item-damned+))
                       (and (is-evil ch) (is-obj-stat implant +item-bless+))))
          (obj-to-char (unequip-char ch pos :implant nil) ch)
          (act ch :item implant
               :subject-emit "$p burns its way out through your flesh!"
               :place-emit "$n screams in horror as $p burns its way out through $s flesh!")
          (damage-eq nil implant (floor (damage-of obj) 2) +top-spell-define+)
          (damage-creature ch ch (dice (floor
                                        (cond
                                          ((= pos +wear-body+)
                                           (* 2 (abs (alignment-of ch))))
                                          ((= pos +wear-head+)
                                           (* 2 (abs (alignment-of ch))))
                                          (t
                                           (abs (alignment-of ch))))
                                        8)
                                       3)
                           nil +top-spell-define+
                           pos))
        (when (and obj
                   (or (and (is-good ch) (is-obj-stat obj +item-damned+))
                       (and (is-evil ch) (is-obj-stat obj +item-bless+))))
          (act ch :item obj
               :subject-emit "You are burned by $p and frantically take it off!"
               :place-emit "$n franctically takes off $p as $e screams in agony!")
          (obj-to-char (unequip-char ch pos :worn nil) ch)
          (damage-creature ch ch (dice (max (floor (abs (alignment-of ch)) 32) 1) 2)
                           nil +top-spell-define+ pos))
        (when (and obj
                   (or (and (is-evil ch) (is-obj-stat obj +item-anti-evil+))
                       (and (is-good ch) (is-obj-stat obj +item-anti-good+))
                       (and (is-neutral ch) (is-obj-stat obj +item-anti-neutral+))))
          (act ch :item obj
               :subject-emit "You are zapped by $p and instantly let go of it."
               :place-emit "$n is zapped by $p and instantly lets go of it.")
          (obj-to-char (unequip-char ch pos :worn nil) ch)
          (when (is-npc ch)
            (obj-from-char obj)
            (obj-to-room obj (in-room-of ch))))))))

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
    (when (probe-file (corpse-pathname (corpse-idnum obj)))
      (delete-file (corpse-pathname (corpse-idnum obj))))))

(defun get-number (name)
  (cl-ppcre:register-groups-bind (number-str name-str)
      (#/(?:([+-]?\d+)\.)?(.*)/ name :sharedp t)
    (if number-str
        (let ((num (parse-integer number-str)))
          (if (plusp num)
              (values num name-str)
              nil))
        (values 1 name-str))))

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

(defun resolve-alias (ch arg list)
  "Returns a list of all objects and creatures in LIST visible to CH that match ARG"
  (cond
    ((and (member arg '("me" "self") :test #'string-equal)
          (find ch list))
     (list ch))
    (t
     (case (find-all-dots arg)
       (:find-all
        (loop for obj in list when (is-visible-to obj ch) collect obj))
       (:find-alldot
        (loop
           with name = (subseq arg 4)
           for obj in list
           when (and (is-alias-of name (aliases-of obj))
                     (is-visible-to obj ch))
           collect obj))
       (:find-indiv
        (multiple-value-bind (number name)
            (get-number arg)
          (dolist (obj list)
            (when (and (is-alias-of name (aliases-of obj))
                       (is-visible-to obj ch)
                       (zerop (decf number)))
              (return-from resolve-alias (list obj))))))))))

(defun resolve-alias-in-room (ch str)
  (first (resolve-alias ch str (append
                                (people-of (in-room-of ch))
                                (contents-of (in-room-of ch))))))

(defun get-char-in-world-by-idnum (idnum)
  (gethash idnum *character-map*))

(defun get-char-room-vis (ch name)
  (first (resolve-alias ch name (people-of (in-room-of ch)))))

(defun get-char-random-vis (ch room)
  (random-elt (remove-if-not (lambda (tch)
                               (and (not (eql tch ch))
                                    (can-see-creature ch tch)))
                             (people-of room))))

(defun get-char-vis (ch name)
  (or
   ;; check the room first
   (get-char-room-vis ch name)
   ;; check the whole world
   (first (resolve-alias ch name *characters*))))

(defun get-player-vis (ch name)
  (or
   ;; check the room first
   (first (resolve-alias ch name
                         (remove-if 'is-npc (people-of (in-room-of ch)))))
   ;; check the whole world
   (first (resolve-alias ch name
                         (remove-if 'is-npc *characters*)))))

(defun get-obj-vis (ch name)
  "Search the entire world for an object."
  (or
   ;; scan items carried
   (first (resolve-alias ch name (carrying-of ch)))
   ;; scan room
   (first (resolve-alias ch name (contents-of (in-room-of ch))))
   ;; scan the entire object list
   (let ((objs (resolve-alias ch name *object-list*)))
     (first objs))))

(defun money-name (amount currency)
  (if (eql currency :gold)
      (cond
        ((= amount 1) "a gold coin")
        ((<= amount 10) "a tiny pile of gold coins")
        ((<= amount 20) "a handful of gold coins")
        ((<= amount 75) "a little pile of gold coins")
        ((<= amount 200) "a small pile of gold coins")
        ((<= amount 1000) "a pile of gold coins")
        ((<= amount 5000) "a big pile of gold coins")
        ((<= amount 10000) "a large heap of gold coins")
        ((<= amount 20000) "a huge mound of gold coins")
        ((<= amount 75000) "an enormous mound of gold coins")
        ((<= amount 150000) "a small mountain of gold coins")
        ((<= amount 250000) "a mountain of gold coins")
        ((<= amount 500000) "a huge mountain of gold coins")
        ((<= amount 1000000) "an enormous mountain of gold coins")
        (t "an absolutely colossal mountain of gold coins"))
      (cond
        ((= amount 1) "a one-credit note")
        ((<= amount 10) "a small wad of cash")
		((<= amount 20) "a handful of cash")
		((<= amount 75) "a large wad of cash")
		((<= amount 200) "a huge wad of cash")
		((<= amount 1000) "a small pile of cash")
		((<= amount 5000) "a big pile of cash")
		((<= amount 10000) "a large heap of cash")
		((<= amount 20000) "a huge mound of cash")
		((<= amount 75000) "an enormous mound of cash")
		((<= amount 150000) "a small mountain of cash money")
		((<= amount 250000) "a mountain of cash money")
        ((<= amount 500000) "a huge mountain of cash")
		((<= amount 1000000) "an enormous mountain of cash")
		(t "an absolutely colossal mountain of cash"))))

(defun money-description (amount currency)
  (cond
    ((= amount 1)
     (if (eql currency :gold)
         "It's just one miserable little gold coin."
         "It's one almighty credit!"))
    ((< amount 10)
     (format nil "There are ~d ~a." amount
             (if (eql currency :gold) "coins" "credits")))
    ((< amount 100)
     (format nil "There are about ~d ~a."
             (- amount (mod amount 10))
             (if (eql currency :gold) "coins" "credits")))
    ((< amount 1000)
     (format nil "There are about ~d ~a."
             (- amount (mod amount 100))
             (if (eql currency :gold) "coins" "credits")))
    ((< amount 100000)
     (format nil "There are about ~d ~a."
             (- amount (mod amount 1000))
             (if (eql currency :gold) "coins" "credits")))
    (t
     (format nil "There are a LOT of ~a."
             (if (eql currency :gold) "coins" "credits")))))

(defun make-money-object (amount currency)
  (assert (plusp amount) nil
          "Attempt to create a negative amount of money")
  (let* ((desc (money-name amount currency))
         (obj (make-object :unknown 0
                           :name desc
                           :aliases (if (eql currency :gold)
                                        (if (= amount 1)
                                            "coin miserable gold"
                                            "coins gold")
                                        (if (= amount 1)
                                            "credit money note one-credit"
                                            "credits money cash"))
                           :line-desc (format nil "~a is lying here."
                                              desc)
                           :kind +item-money+
                           :value0 amount
                           :value1 (if (eql currency :gold) 0 1)
                           :material (if (eql currency :gold)
                                         +mat-gold+
                                         +mat-paper+)
                           :wear-flags +item-wear-take+)))
    (setf (ex-description-of obj)
          (list
           (make-instance 'extra-descr-data
                          :keyword (if (eql currency :gold)
                                       "coins gold"
                                       "credits money cash")
                          :description (money-description amount currency))))
    obj))

(define-condition matcher-parse-error ()
  ((msg :accessor msg-of :initarg :msg)
   (lineno :accessor lineno-of :initarg :lineno)))

(defun match-spec-to-predicate (tokens)
  (let ((spec (first tokens))
        (param (rest tokens)))
    (let ((char-class (parse-player-class spec)))
      (when char-class
        (return-from match-spec-to-predicate
          (lambda (ch) (is-class ch char-class)))))
    (let ((race (parse-pc-race spec)))
      (when race
        (return-from match-spec-to-predicate
          (lambda (ch) (is-race ch race)))))
    (let ((clan (resolve-clan-alias spec)))
      (when clan
        (return-from match-spec-to-predicate
          (lambda (ch) (clan-member-p ch clan)))))

    (string-abbrev-case spec
      ("all" (constantly t))
      ("good" (lambda (ch) (is-good ch)))
      ("evil" (lambda (ch) (is-evil ch)))
      ("neutral" (lambda (ch) (is-neutral ch)))
      ("criminal" (lambda (ch) (criminalp ch)))
      ("player" (lambda (ch) (is-pc ch)))
      ("lvl<" (let ((lvl (parse-integer (first param) :junk-allowed t)))
                (if lvl
                    (lambda (ch)
                      (< (level-of ch) (parse-integer (first param))))
                    (signal 'matcher-parse-error :msg (format nil "Invalid level '~a'"
                                                              (first param))))))
      ("lvl>" (let ((lvl (parse-integer (first param) :junk-allowed t)))
                (if lvl
                    (lambda (ch)
                      (> (level-of ch) (parse-integer (first param))))
                    (signal 'matcher-parse-error :msg (format nil "Invalid level '~a'"
                                                              (first param))))))
      ("clanleader" (lambda (ch) (plr-flagged ch +plr-clan-leader+)))
      (t
       (signal 'matcher-parse-error :msg (format nil "Invalid condition '~a'" spec))))))

(defun match-clause-to-lambda (tokens prev)
  (let* ((allow-or-deny (string-equal (first tokens) "allow"))
         (complement (string-equal (second tokens) "not"))
         (raw-predicate (match-spec-to-predicate (nthcdr (if complement 2 1) tokens)))
         (predicate (if complement
                        (complement raw-predicate)
                        raw-predicate)))
    (if prev
        (lambda (ch)
          (multiple-value-bind (result decided)
              (funcall prev ch)
            (cond
              (decided
               (values result t))
              ((funcall predicate ch)
               (values allow-or-deny t))
              (t
               nil))))
        (lambda (ch)
          (when (funcall predicate ch)
            (values allow-or-deny t))))))

(defun make-creature-matcher (text)
  "Converts the text to a function which matches a creature with attributes described by the text.  Ignores lines which do not begin with 'allow' or 'deny'.  May signal a MATCHER-PARSE-ERROR if the text is invalid."
  (with-input-from-string (str text)
    (let ((result nil)
          (lineno 1))
      (handler-bind ((matcher-parse-error
                      (lambda (err)
                        (setf (lineno-of err) lineno)
                        (signal err))))
        (loop
           for line = (read-line str nil)
           while line do
             (let ((tokens (cl-ppcre:split "\\s+" line)))
               (incf lineno)
               (when (find (first tokens) '("allow" "deny") :test #'string-equal)
                 (setf result (match-clause-to-lambda tokens result)))))
        result))))

(defun circle-follow (ch victim)
  "Returns T if CH following VICTIM would make a follow loop."
  (loop
     for k = victim then (master-of k)
     until (or (null k)
               (eql k ch))
     finally (return (eql k ch))))

(defun add-follower (ch leader)
  (assert (null (master-of ch)) nil "Master of creature is non-NULL in add-follower")
  (setf (master-of ch) leader)
  (push ch (followers-of leader))

  (act ch :target leader
       :subject-emit "You now follow $N."
       :target-emit "$n starts following you."
       :not-target-emit "$n starts to follow $N."))

(defun stop-following (ch)
  (assert (master-of ch) nil "stop-follower called with NIL master")

  (cond
    ((and (aff-flagged ch +aff-charm+)
          (not (mob2-flagged ch +mob2-mount+)))
     (act ch :target (master-of ch)
          :subject-emit "You realize that $N is a jerk!"
          :target-emit "$n hates your guts!"
          :not-target-emit "$n realizes that $N is a jerk!")
     (affect-from-char ch +spell-charm+))
    (t
     (act ch :target (master-of ch)
          :subject-emit "You stop following $N."
          :not-target-emit "$n stops following $N.")
     (when (and (or (is-npc ch)
                    (< (invis-level-of ch) (level-of (master-of ch))))
                (not (aff-flagged ch +aff-sneak+)))
       (act ch :target (master-of ch)
            :target-emit "$n stops following you."))))

  (setf (followers-of (master-of ch)) (delete ch (followers-of (master-of ch))))
  (setf (master-of ch) nil)

  (setf (aff-flags-of ch) (logandc2 (aff-flags-of ch) (logior +aff-charm+ +aff-group+))))