(in-package #:tempus)

(defparameter +interface-none+ 0)
(defparameter +interface-power+ 1)
(defparameter +interface-chips+ 2)
(defparameter +num-interfaces+ 3)

(defparameter +chip-none+ 0)
(defparameter +chip-skill+ 1)
(defparameter +chip-affects+ 2)
(defparameter +num-chips+ 3)

(defun interface-type (obj)
  (aref (value-of obj) 0))
(defun interface-max (obj)
  (aref (value-of obj) 2))
(defun chip-type (obj)
  (aref (value-of obj) 0))
(defun chip-data (obj)
  (aref (value-of obj) 1))
(defun chip-max (obj)
  (aref (value-of obj) 2))
(defun skillchip (obj)
  (= (chip-type obj) +chip-skill+))

(defun check-interface (ch obj mode)
  (dolist (chip (contains-of obj))
    (when (and (skillchip chip)
               (plusp (chip-data chip))
               (< (chip-data chip) +max-skills+))
      (affect-modify ch (- (chip-data chip)) (chip-max chip) 0 0 mode))
    (dotimes (j +max-obj-affect+)
      (let ((af (aref (affected-of obj) j)))
        (affect-modify ch (location-of af) (modifier-of af) 0 0 mode)))
    (dotimes (j 3)
      (affect-modify ch 0 0 (aref (bitvector-of obj) j) (1+ j) nil))))
      
(defun obj-gives-affects (obj ch mode)
  (and (eql (worn-by-of obj) ch)
       (not (invalid-char-class ch obj))
       (or (not (eql mode :worn))
           (and (not (is-implant obj))
                (not (and (= (worn-on-of obj) +wear-belt+)
                          (or (= (kind-of obj) +item-weapon+)
                              (= (kind-of obj) +item-pipe+))))))
       (not (and (is-device obj)
                 (zerop (engine-state obj))))))

(defun apply-skill (ch skill mod)
  (setf (aref (skills-of ch) skill)
        (min (+ (aref (skills-of ch) skill) mod) 125)))

(defun affect-modify (ch loc mod bitv index add)
  (when bitv
    ;; Handle bitvectors
    (if add
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
           (setf (aff3-flags-of ch) (logandc2 (aff3-flags-of ch) bitv)))))

  (when (not add)
    (setf mod (- mod)))

  (when (minusp loc)
    (setf loc (- loc))
    (when (and (< loc +max-skills+) (not (is-npc ch)))
      (apply-skill ch loc mod))
    (return-from affect-modify))

  (cond
    ((= loc +apply-str+)
     (incf (str-of ch) mod)
     (incf (str-of ch) (floor (str-add-of ch) 10))
     (setf (str-add-of ch) 0))
    ((= loc +apply-dex+)
     (incf (dex-of ch) mod))
    ((= loc +apply-int+)
     (incf (int-of ch) mod))
    ((= loc +apply-wis+)
     (incf (wis-of ch) mod))
    ((= loc +apply-con+)
     (incf (con-of ch) mod))
    ((= loc +apply-cha+)
     (incf (cha-of ch) mod))
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
    (t
     (error "Unknown apply adjust attempt on ~a ~d + ~d in affect_modify add=~a"
            (name-of ch) loc mod add)))))

(defun affect-total (ch)
  "Updates a character by subtracting everything it is affected by, restoring original abilities, and then affecting it all again."
  (when (> (str-of ch) 18)
    (incf (str-of ch) 10))

  ;; Remove all item-based affects
  (dotimes (i +num-wears+)
    ;; Remove equipment affects
    (when (aref (equipment-of ch) i)
      (apply-object-affects ch (aref (equipment-of ch) i) nil))
    (when (aref (implants-of ch) i)
      (apply-object-affects ch (aref (implants-of ch) i) nil)
      (when (= (kind-of obj) +item-interface+)
        (check-interface obj ch nil)))
    (when (aref (tattoos-of ch) i)
      (apply-object-affects ch (aref (tattoos-of ch) i) nil)))

  ;; remove all spell affects
  (dolist (af (affected-of ch))
    (affect-modify ch (location-of af) (modifier-of af) (bitvector-of af)
                   (aff-index-of af) nil))



(defun char-to-room (ch room &optional check-specials)
  (assert (null(in-room-of ch)) nil "creature already in a room in char-to-room!")
  (assert (and ch room) nil "Illegal values passed to char-to-room")

  (push ch (people-of room))
  (setf (in-room-of ch) room)
  #+nil  (when (and (eql (race-of ch) +race-elemental+)
                    (eql (class-of ch) +class-fire+))
           (incf (light-of room)))

  #+nil  (let ((light-eq (get-eq ch +wear-light+)))
           (when (and light-eq
                      (eql (kind-of light-eq) +item-light+)
                      (plusp (aref (val-of light-eq) 2)))
             (incf (light-of room))))

  #+nil  (when (or (is-affected ch +aff-glowlight+)
                   (is-affected-2 ch +aff2-fluorescent+)
                   (is-affected-2 ch +aff2-divine-illumination+)
                   (affected-by-spell ch +spell-quad-damage+))
           (incf (light-of room)))

  #+nil  (when (is-pc ch)
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

(defun apply-ac (ch eq-pos)
  (let ((obj (aref (equipment-of ch) eq-pos)))
    (assert obj nil "NIL eq at eq_pos")
    (if (= (kind-of obj) +item-armor+)
        (cond
          ((= eq-pos +wear-body+)
           (* 3 (aref (value-of obj) 0)))
          ((or (= eq-pos +wear-head+)
               (= eq-pos +wear-legs+))
           (* 2 (aref (value-of obj) 0)))
          (t
           (aref (value-of obj) 0)))
        0)))


(defun unequip-char (ch pos mode disable-checks)
  (assert (< 0 pos +num-wears+) nil "Illegal pos in unequip-char")
  (let ((obj nil))
    (case mode
      (:worn
       (assert (aref (equipment-of ch) pos) nil
               "eq NIL at pos ~d" pos)
       (setf obj (aref (equipment-of ch) pos))
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

    (when (obj-gives-affects obj ch mode)
      (dotimes (j +max-obj-affect+)
        (let ((af (aref (affected-of obj) j)))
          (affect-modify ch (location-of af) (modifier-of af) 0 0 nil)))
      (dotimes (j 3)
        (affect-modify ch 0 0 (aref (bitvector-of obj) j) (1+ j) nil))
      (when (and (is-interface obj)
                 (= (interface-type obj) +interface-chips+)
                 (contains-of obj))
        (check-interface ch obj nil)))

    (setf (worn-by-of obj) nil)
    (setf (worn-on-of obj) -1)

    (affect-total ch)

    (when (and (not disable-checks)
               (eql mode :worn))
      (when (and (= pos +wear-waist+)
                 (aref (equipment-of ch) +wear-belt+))
        (obj-to-char (unequip-char ch +wear-belt+ :worn nil) ch))
      (when (and (= pos +wear-wield+)
                 (aref (equipment-of ch) +wear-wield-2+))
        (equip-char ch (unequip-char ch +wear-wield-2+ :worn nil)
                    +wear-wield+ :worn)))

    obj))
      
(defun extract-obj (obj)
  "Extract an object from the world"
  (cond
    ((worn-by-of obj)
     (when (unequip-char (worn-by-of obj) (worn-on-of obj)
                         (if (eql obj (get-eq (worn-by-of obj) (worn-on-of obj)))
                             +equip-worn+
                             +equip-implant+)
                         nil)
       (error "Inconsistent worn-by and worn-on references!")))
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

  (when (is-vehicle obj)
    (path-remove-object obj))

  (when (and (is-corpse obj)
             (plusp (corpse-idnum obj)))
    (delete-file (corpse-file-path (corpse-idnum obj)))))