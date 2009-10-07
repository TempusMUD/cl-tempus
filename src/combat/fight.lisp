(in-package :tempus)

(defun remove-fighting-affects (ch)
  (cond
    ((and (in-room-of ch) (room-is-open-air (in-room-of ch)))
     (setf (position-of ch) +pos-flying+))
    ((is-npc ch)
     (when (or (and (aff-flagged ch +aff-charm+)
                    (is-undead ch))
               (> (position-of ch) +pos-sitting+))
       (setf (position-of ch) +pos-standing+)))
    ((>= (position-of ch) +pos-fighting+)
     (setf (position-of ch) +pos-standing+))
    ((> (position-of ch) +pos-resting+)
     (setf (position-of ch) +pos-sitting+)))
  (update-pos ch))

(defun change-alignment (ch victim)
  (setf (alignment-of ch) (pin (- (alignment-of ch)
                                  (floor (alignment-of victim) 100))
                               -1000
                               1000))
  (check-eq-align ch))

(defun transfer-items-to-corpse (ch killer corpse)
  (let ((lose-eq (or (is-npc ch)
                     (and (not (immortal-level-p ch))
                          (not (arena-combat-p killer ch)))))
        (lose-implants (or (is-npc ch)
                           (and (not (immortal-level-p ch))
                                (not (arena-combat-p killer ch))
                                (not (npk-combat-p killer ch))))))
    ;; transfer equipment and inventory to corpse
    (loop
       for obj across (equipment-of ch)
       when (and obj (or lose-eq (unrentablep obj))) do
         (unequip-char ch (worn-on-of obj) :worn nil)
         (obj-to-obj obj corpse))
    (loop
       for obj across (tattoos-of ch)
       when obj do
         (unequip-char ch (worn-on-of obj) :worn nil)
         (extract-obj obj))

    (loop
       for obj across (implants-of ch)
       when (and obj (or lose-implants (unrentablep obj))) do
         (unequip-char ch (worn-on-of obj) :worn nil)
         (obj-to-obj obj corpse))

    (dolist (obj (copy-list (carrying-of ch)))
      (when (or lose-eq (unrentablep obj))
        (obj-from-char obj)
        (obj-to-obj obj corpse)))

    (unless (zerop (gold-of ch))
      (obj-to-obj (make-money-object (gold-of ch) :gold) corpse))
    (unless (zerop (cash-of ch))
      (obj-to-obj (make-money-object (gold-of ch) :cash) corpse))))

(defun raw-kill (ch killer attacktype)
  (when (and (is-npc ch)
             (func-of (shared-of ch)))
    (funcall (func-of (shared-of ch)) killer ch 0 nil :death))

  (when (is-cyborg ch)
    (setf (total-dam-of ch) 0)
    (setf (broken-component-of ch) 0))

  (when (and (not (is-npc ch))
             killer
             (not (arena-combat-p killer ch)))
    (gain-exp ch (- (min (floor (exp-of ch) 8)
                         (- (exp-of ch)
                            (aref +exp-scale+ (level-of ch)))))))

  (unless (= attacktype +skill-garotte+)
    (death-cry ch))

  (trigger-prog-dying ch killer)

  ;; Handle dying progs before creating the corpse
  (when (prog-obj-of (in-room-of ch))
    (trigger-prog-death (in-room-of ch) :room ch))

  (dolist (tch (people-of (in-room-of ch)))
    (trigger-prog-death tch :mobile ch))

  (let ((corpse (make-corpse ch killer attacktype)))
    (transfer-items-to-corpse ch killer corpse)
    (when (noncorporealp ch)
      (dolist (obj (copy-list (contains-of corpse)))
        (obj-from-obj obj)
        (obj-to-room obj (in-room-of ch)))
      (extract-obj corpse))

    ;; Remove affects from character
    (dolist (af (copy-list (affected-of ch)))
      (affect-remove ch af))

    (setf (aff2-flags-of ch) (logandc2 (aff2-flags-of ch)
                                       (logior +aff2-petrified+
                                               +aff2-ablaze+)))
    (setf (aff3-flags-of ch) (logandc2 (aff3-flags-of ch)
                                       +aff3-self-destruct+))
    (setf (total-dam-of ch) 0)
    (setf (broken-component-of ch) 0)

    (cond
      ((is-npc ch)
       (incf (kills-of (shared-of ch))))
      (t
       ;; Cap exp loss at the beginning of the level
       (gain-exp ch (- (min (floor (exp-of ch) 8)
                            (aref +exp-scale+ (level-of ch)))))

       (when (quest-id-of ch)
         (tally-quest-death ch))

       (incf (deaths-of ch))))

    (cond
      ((arena-combat-p killer ch)
       (arena-die ch))
      ((and (npk-combat-p killer ch)
            (not (room-flagged (in-room-of ch) +room-death+)))
       (npk-die ch))
      (t
       (die ch)))

    (when (and killer
               (not (eql killer ch))
               (pref-flagged killer +pref-autoloot+))
      (perform-autoloot killer corpse))))

(defun calc-exp-penalty (ch victim)
  (+ (if (and (is-good ch) (is-good victim)
              (or (is-cleric ch) (is-knight ch)))
         0.5 1)
     (float (/ (remort-gen-of ch)
               (+ (remort-gen-of ch) 2)))
     (cond
       ((not (is-remort ch))
        0)
       ((<= (level-of ch) 15)
        -0.1)
       ((>= (level-of ch) 40)
        0.1)
       (t
        0))))

(defun calc-explore-bonus (ch victim)
  (if (and (not (is-npc ch)) (is-npc victim))
      (let ((kill (assoc (vnum-of victim) (recently-killed-of ch))))
        (cond
          (kill
           (incf (cdr kill)))
          (t
           (setf kill (list (vnum-of victim) 1))
           (setf (recently-killed-of ch)
                 (last (recently-killed-of ch) 100))
           (setf (recently-killed-of ch)
                 (nconc (recently-killed-of ch) (list kill)))))
        (if (<= (cdr kill) 10)
            0.25 0))
      0))

(defun group-gain-experience (ch victim)
  (dolist (tch (followers-of (or (master-of ch) ch)))
    (gain-kill-experience tch victim 1.0)))

(defun gain-kill-experience (ch victim group-multiplier)
  (let* ((vict-exp (floor (* (exp-of victim) group-multiplier)
                          (if (is-npc victim) 3 8)))
         (level-exp (floor (* vict-exp (min (if (is-npc victim) 4 8)
                                            (- (level-of victim)
                                               (level-of ch))))))
         (max-exp (min +max-exp-gain+
                       (floor (- (aref +exp-scale+ (1+ (level-of ch)))
                                 (aref +exp-scale+ (level-of ch)))
                              8)))
         (raw-exp (+ vict-exp level-exp))
         (bonus (calc-explore-bonus ch victim))
         (exp (pin (+ raw-exp
                      (- (* raw-exp (calc-exp-penalty ch victim)))
                      (+ (* raw-exp bonus)))
                   1 max-exp)))
    (when (and (not (is-npc ch))
               (is-npc victim)
               (minusp (exp-of victim))
               (> exp 5000000))
      (slog "~a killed ~a(~d) for exp: ~d"
            (name-of ch)
            (name-of victim)
            (exp-of victim)
            exp))

    (cond
      ((plusp exp)
       (when (plusp bonus)
         (send-to-char ch "&YYou've received an exploration bonus!&n~%"))
       (send-to-char ch "&YYou have gained ~d experience.~%" exp))
      ((minusp exp)
       (send-to-char ch "&YYou have lost experience.~%"))
      (t
       (send-to-char ch "You have gained trivial experience.~%")))

    (gain-exp ch exp)
    (change-alignment ch victim)))

(defun maybe-gain-exp (ch victim)
  (unless (or (eql ch victim)
              (and (is-npc victim)
                   (mob2-flagged victim +mob2-unapproved+)
                   (not (testerp ch)))
              (and (is-npc ch) (is-pet ch))
              (and (is-npc victim) (is-pet victim))
              (let ((distance (find-distance (in-room-of ch)
                                             (in-room-of victim))))
                (and distance
                     (<= distance 2))))
    (if (aff-flagged ch +aff-group+)
        (group-gain-experience ch victim)
        (gain-kill-experience ch victim 1))))

(defun destroyed-object-properties (obj type)
  "Returns properties of the object created when OBJ is destroyed by the TYPE spell.  The properties returned are the message emitted, the object name, the object aliases, and the new object linedesc."
  (let ((obj-material-name (aref +material-names+ (material-of obj))))
    (cond
      ((and (eql type +spell-oxidize+) (is-ferrous obj))
       (values "$p dissolves into a pile of rust!!"
               "a pile of rust"
               "pile rust"
               "A pile of rust is lying here."
               +mat-rust+))
      ((and (eql type +spell-oxidize+) (is-combustable obj))
       (values "$p is incinerated!!"
               "a pile of ash"
               "pile ash"
               "A pile of ash is lying here."
               +mat-ash+))
      ((eql type +spell-bless+)
       (values "$p glows bright blue and shatters to pieces!!"
               (format nil "shattered fragments of ~a" obj-material-name)
               (format nil "~a shattered fragments" obj-material-name)
               (format nil "Shattered fragments of ~a are lying here." obj-material-name)
               (material-of obj)))
      ((eql type +spell-damn+)
       (values "$p glows bright red and shatters to pieces!!"
               (format nil "shattered fragments of ~a" obj-material-name)
               (format nil "~a shattered fragments" obj-material-name)
               (format nil "Shattered fragments of ~a are lying here." obj-material-name)
               (material-of obj)))
      ((is-metal obj)
       (values "$p is reduced to a mangled pile of scrap!!"
               (format nil "mangled heap of ~a" obj-material-name)
               (format nil "~a mangled heap" obj-material-name)
               (format nil "A mangled heap of ~a are lying here."
                       obj-material-name)
               (material-of obj)))
      ((or (is-glass obj) (is-stone obj))
       (values "$p shatters into a thousand fragments!!"
               (format nil "shattered fragments of ~a" obj-material-name)
               (format nil "~a shattered fragments" obj-material-name)
               (format nil "Shattered fragments of ~a are lying here."
                       obj-material-name)
               (material-of obj)))
      (t
       (values "$p has been destroyed!!"
               (format nil "a multilated heap of ~a" obj-material-name)
               (format nil "~a multilated heap" obj-material-name)
               (format nil "A multilated heap of ~a is lying here." obj-material-name))))))

(defun replace-destroyed-object (destroyed-obj name aliases ldesc material)
  (let ((new-obj (make-object :unknown 0
                              :kind +item-trash+
                              :name name
                              :aliases aliases
                              :line-desc ldesc
                              :material material
                              :wear-flags +item-wear-take+
                              :extra-flags (logior +item-nodonate+
                                                   +item-nosell+)
                              :extra2-flags (logand (extra2-flags-of destroyed-obj) +item2-implant+)
                              :weight (weight-of destroyed-obj)
                              :max-dam 100
                              :damage 100
                              :value0 0
                              :value1 2
                              :value2 0
                              :value3 0)))
    ;; replace the old object with the new object
    (cond
      ((worn-by-of destroyed-obj)
       (let* ((wearer (worn-by-of destroyed-obj))
              (pos (worn-on-of destroyed-obj))
              (wear-mode (if (aref (equipment-of wearer) pos) :worn :implant)))
         (unequip-char wearer pos wear-mode t)
         (equip-char wearer new-obj pos wear-mode)))
      ((carried-by-of destroyed-obj)
       (obj-to-char new-obj (carried-by-of destroyed-obj)))
      ((in-room-of destroyed-obj)
       (obj-to-room new-obj (in-room-of destroyed-obj)))
      ((in-obj-of destroyed-obj)
       (obj-to-obj new-obj (in-obj-of destroyed-obj))))))

(defun dump-object-contents (obj)
  "Dumps contents of OBJ into the location of OBJ."
  (loop
     for inner-obj = (first (contains-of obj))
     while inner-obj
     do
       (obj-from-obj inner-obj)
       (cond
         ((in-room-of obj)
          (obj-to-room inner-obj (in-room-of obj)))
         ((worn-by-of obj)
          (obj-to-char inner-obj (worn-by-of obj)))
         ((carried-by-of obj)
          (obj-to-char inner-obj (carried-by-of obj)))
         ((in-obj-of obj)
          (obj-to-obj inner-obj (in-obj-of obj))))))

(defun act-obj (ch obj msg)
  "Sends an act message regarding OBJ to the necessary observers."
  (cond
    ((in-room-of obj)
     (act (first (people-of (in-room-of obj))) :item obj :all-emit msg))
    ((worn-by-of obj)
     (act (worn-by-of obj) :item obj :subject-emit msg))
    (t
     (act ch :item obj :subject-emit msg))))

(defun destroy-object (ch obj type)
  (multiple-value-bind (msg name aliases ldesc material)
      (destroyed-object-properties obj type)
    (act-obj ch obj msg)
    (when (proto-of (shared-of obj))
        (replace-destroyed-object obj name aliases ldesc material)
        (dump-object-contents obj)))
    (extract-obj obj))

(defun can-damage-obj (ch obj)
  "Returns T if OBJ can be damaged by CH.  Otherwise returns NIL."
  (not (or (eql (kind-of obj) +item-money+)
           (eql (kind-of obj) +item-key+)
           (eql (kind-of obj) +item-script+)
           (minusp (damage-of obj))
           (minusp (max-dam-of obj))
           (and (not (immortalp ch)) (not (can-wear obj +item-wear-take+)))
           (and ch (room-flagged (in-room-of ch) +room-arena+))
           (and ch (plusp (quest-id-of ch)) (quest-flagged (quest-by-vnum (quest-id-of ch)) +quest-arena+))
           (let ((room (where-obj obj)))
             (or (room-flagged room +room-arena+)
                 (zerop (number-of room)))))))

(defun damage-eq (ch obj amount type)
  (when (can-damage-obj ch obj)
    (let ((old-damage (damage-of obj)))
      (decf (damage-of obj) (min (damage-of obj) amount))

      (cond
        ((< (damage-of obj) (floor (max-dam-of obj) 32))
         ;; damage interior items
         (send-to-char ch "Destroying...~%")
         (dolist (inner-obj (copy-list (contains-of obj)))
           (damage-eq ch inner-obj (floor amount 2) type))
         (destroy-object ch obj type))
        ((< (damage-of obj) (floor (max-dam-of obj) 8) old-damage)
         (setf (extra2-flags-of obj) (logior (extra2-flags-of obj) +item2-broken+))
         (act-obj ch obj "$p has been severely damaged!")
         ;; unequip object if equipped
         (let ((vict (worn-by-of obj)))
           (when (and vict (aref (equipment-of vict) (worn-on-of obj)))
             (obj-to-char (unequip-char vict (worn-on-of obj) :worn nil) vict)))
         ;; turn off object if device
         (when (is-device obj)
           (setf (aref (value-of obj) 2) 0)))
        ((< (damage-of obj) (floor (max-dam-of obj) 4) old-damage)
         (act-obj ch obj
                  (format nil "$p is starting to look pretty ~a."
                          (cond
                            ((is-metal-type obj) "mangled")
                            ((or (is-leather-type obj) (is-cloth-type obj)) "ripped up")
                            (t "bad")))))
        ((< (damage-of obj) (floor (max-dam-of obj) 2) old-damage)
         (act-obj ch obj "$p is starting to show signs of wear."))))))

;; damage kinds are as follows:
;;   crushing slashing impaling fire ice heat cold electric disease
;;   poison radiance necros

;; damage modifiers are as follows:
;;   magic divine psionic
(defun damage-creature (victim amount kind pos)
  (declare (ignore victim amount kind pos))
  nil)
