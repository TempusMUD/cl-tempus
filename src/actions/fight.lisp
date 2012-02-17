(in-package :tempus)

(define-condition creature-died ()
  ((creature :reader creature-of :initform :creature)))

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
         (unequip-char ch (worn-on-of obj) :worn t)
         (obj-to-obj obj corpse))
    (loop
       for obj across (tattoos-of ch)
       when obj do
         (unequip-char ch (worn-on-of obj) :tattoo t)
         (extract-obj obj))

    (loop
       for obj across (implants-of ch)
       when (and obj (or lose-implants (unrentablep obj))) do
         (unequip-char ch (worn-on-of obj) :implant t)
         (obj-to-obj obj corpse))

    (dolist (obj (copy-list (carrying-of ch)))
      (when (or lose-eq (unrentablep obj))
        (obj-from-char obj)
        (obj-to-obj obj corpse)))

    (unless (zerop (gold-of ch))
      (obj-to-obj (make-money-object (gold-of ch) :gold) corpse))
    (unless (zerop (cash-of ch))
      (obj-to-obj (make-money-object (cash-of ch) :cash) corpse))))

(defun raw-kill (ch killer attacktype)
  (when (and (is-npc ch)
             (func-of (shared-of ch)))
    (funcall (func-of (shared-of ch)) killer ch 0 nil :death))

  (when (and (is-pc ch) (is-cyborg ch))
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
    (when (is-pc ch)
      (setf (total-dam-of ch) 0)
      (setf (broken-component-of ch) 0))

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

    (when (and killer
               (not (eql killer ch))
               (pref-flagged killer +pref-autoloot+))
      (perform-autoloot killer corpse))

    (cond
      ((arena-combat-p killer ch)
       (arena-die ch))
      ((and (npk-combat-p killer ch)
            (not (room-flagged (in-room-of ch) +room-death+)))
       (npk-die ch))
      (t
       (die ch)))

    (signal 'creature-died :creature ch)))

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
  "Returns properties of the object created when OBJ is destroyed by
the TYPE spell.  The properties returned are the message emitted, the
object name, the object aliases, and the new object linedesc."
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
                            ((is-metal obj) "mangled")
                            ((or (is-leather obj) (is-cloth obj)) "ripped up")
                            (t "bad")))))
        ((< (damage-of obj) (floor (max-dam-of obj) 2) old-damage)
         (act-obj ch obj "$p is starting to show signs of wear."))))))

;; damage kinds are as follows:
;;   crushing slashing impaling heat cold electric disease
;;   poison radiance necros

;; damage modifiers are as follows:
;;   magic divine psionic
(defun is-greater-devil (ch)
  (and (is-devil ch)
       (or (is-class ch +class-greater+)
           (is-class ch +class-arch+)
           (is-class ch +class-duke+))))

(defun is-weapon (skill)
  (or (< (1- +type-hit+) skill +top-attacktype+)
      (= skill +skill-second-weapon+)
      (= skill +skill-energy-weapons+)
      (= skill +skill-archery+)
      (= skill +skill-proj-weapons+)
      (< +type-egun-laser+ skill +type-egun-top+)))

(defun can-attack (ch victim)
  "Returns T if the creature CH can even attempt to damage VICTIM.
Returns NIL if that would violate the First Amendment, the Mann Act,
and/or the structure of spacetime as we know it."
  ;; Can't damage someone who is writing
  (when (and ch
             (plr-flagged victim (logior +plr-mailing+ +plr-writing+ +plr-olc+))
             (not (eql ch victim)))
    (mudlog 'info t "~a has attacked ~a while writing at ~d"
            (name-of ch)
            (name-of victim)
            (number-of (in-room-of ch)))
    (send-to-char ch "NO!  Do you want to be ANNIHILATED by the gods?!")
    (return-from can-attack nil))

  ;; High enough level immortals can do (almost) anything
  (when (and ch (> (level-of ch) +lvl-creator+))
    (return-from can-attack t))

  ;; Can't damage utility mobs
  (when (mob-flagged victim +mob-utility+)
    (return-from can-attack nil))
  (when (and ch
             (mob2-flagged ch +mob2-unapproved+)
             (not (testerp victim)))
    (send-to-char ch "You're an unapproved mob.~ Stop attacking people.%")
    (return-from can-attack nil))
  (when (and ch
             (testerp ch)
             (is-pc victim)
             (not (testerp victim)))
    (send-to-char ch "Testers can only attack other tester players.~%")
    (return-from can-attack nil))
  (when (and ch
             (is-npc victim)
             (immortal-level-p ch)
             (< (level-of ch) +lvl-timegod+))
    (send-to-char ch "Immortals cannot attack mobiles.  Sorry.~%")
    (return-from can-attack nil))

  ;; Newbie protection and NOPK check
  (when (and ch
             (not (eql ch victim))
             (is-npc victim)
             (or (is-pc ch))
                 (and (master-of ch) (is-pc (master-of ch))))
    (unless (arena-combat-p ch victim)
      (when (plr-flagged ch +plr-nopk+)
        (send-to-char ch "A small dark shape flies in from the future and sticks to your eyebrow.~%")
        (return-from can-attack nil))
      (when (plr-flagged victim +plr-nopk+)
        (send-to-char ch "A small dark shape flies in from the future and sticks to your nose.~%")
        (return-from can-attack nil))))


  (when (and (< +lvl-ambassador+ (level-of ch) +lvl-god+)
             (is-npc victim)
             (not *mini-mud*))
    (send-to-char ch "You are not allowed to attack mobiles!~%")
    (return-from can-attack nil))

  ;; Passed all the tests - attacking possible
  t)

(defun can-damage-creature (ch victim weapon type)
  "Returns T if the creature CH can damage VICTIM.  If not, the result
is simply no damage.  CH may be NIL for damage caused by the
environment.  The second value returned is T if a message has already
been displayed."

  (when (and (is-undead victim) (eql type +spell-poison+))
    (return-from can-damage-creature nil))

  (when (and (not (char-has-blood victim)) (eql type +type-bleed+))
    (return-from can-damage-creature nil))

  ;; Can't damage an immortal who isn't mortalized
  (when (immortalp victim)
    (return-from can-damage-creature nil))

  ;; Damage from the environment is fully checked out
  (when (null ch)
    (return-from can-damage-creature t))

  (unless (can-attack ch victim)
    (return-from can-damage-creature nil))

  ;; Can only damage some creatures with magical attacks
  (when (or (noncorporealp victim)
            (is-rakshasa victim)
            (is-greater-devil victim))
    (unless (or (or (null ch)
                    ;; They can hit each other
                    (noncorporealp ch)
                    (is-rakshasa ch)
                    (is-greater-devil ch)
                    ;; bare-handed attacks with kata work
                    (and (is-weapon type)
                         (null weapon)
                         (affected-by-spell ch +skill-kata+)))
                ;; spells can hit them
                (not (is-weapon type))
                ;; magical items can hit them
                (and weapon (is-obj-stat weapon +item-magic+))
                ;; energy weapons can hit them
                (and weapon (is-obj-kind weapon +item-energy-gun+)))
      (return-from can-damage-creature (values nil t))))

  ;; Shield mastery
  (when (and victim
             (is-weapon type)
             (aref (equipment-of victim) +wear-shield+)
             (> (skill-of victim +skill-shield-mastery+) 20)
             (> (skill-of victim +skill-shield-mastery+)
                (random-range 0 600))
             (>= (position-of victim) +pos-fighting+))
    (act victim
         :target ch
         :item (aref (equipment-of victim) +wear-shield+)
         :subject-emit "You deflect $N's attack with $p!"
         :target-emit "$n deflects your attack with $s shield!"
         :not-target-emit "$n deflects $N's attack with $S shield!")
    (return-from can-damage-creature (values nil t)))

  ;; Uncanny dodge
  (when (and victim
             (is-weapon type)
             (not (spell-is-psionic type))
             (not (spell-is-bardic type))
             (> (skill-of victim +skill-uncanny-dodge+) 20)
             (> (skill-of victim +skill-uncanny-dodge+)
                (random-range 0 350))
             (>= (position-of victim) +pos-fighting+))
    (act victim
         :target ch
         :item (aref (equipment-of victim) +wear-shield+)
         :subject-emit "You smirk as you easily sidestep $N's attack!"
         :target-emit "$n smirks as $e easily sidesteps your attack!"
         :not-target-emit "$n smirks as $e easily sidesteps $N's attack!")
    (return-from can-damage-creature (values nil t)))

  ;; Tumbling
  (when (and victim
             (is-weapon type)
             (not (spell-is-psionic type))
             (not (spell-is-bardic type))
             (> (skill-of victim +skill-tumbling+) 20)
             (> (skill-of victim +skill-tumbling+)
                (random-range 0 425))
             (>= (position-of victim) +pos-fighting+))
    (act victim
         :target ch
         :item (aref (equipment-of victim) +wear-shield+)
         :subject-emit "You dexterously roll away from $N's attack!"
         :target-emit "$n dexterously rolls away from your attack!"
         :not-target-emit "$n dexterously rolls away from $N's attack!")
    (return-from can-damage-creature (values nil t)))

  ;; TODO: Mirror image melody
  ;; TODO: Dimensional shift
  ;; TODO: Prismatic sphere
  ;; TODO: Electrostatic field
  ;; TODO: Thorn skin

  ;; Passed all the tests - damaging is actually possible
  t)

(defun is-affected-by-sanc (ch attacker)
  (and (aff-flagged ch +aff-sanctuary+)
       (or (null attacker)
           (not (or (and (is-evil ch)
                         (affected-by-spell attacker
                                            +spell-righteous-penetration+))
                    (and (is-good ch)
                         (affected-by-spell attacker
                                            +spell-malefic-violation+)))))))

(defun group-affect-caster (ch spell)
  "When CH is affected by SPELL, returns the caster of SPELL, if the caster is CH or is in the same room as CH."
  (let* ((af (find-if (lambda (af)
                        (and (eql (kind-of af) spell)
                             (eql (location-of af) +apply-caster+)))
                      (affected-of ch)))
         (caster-id (and af (modifier-of af))))
    (when af
      (if (eql caster-id (idnum-of ch))
          ch
          (find-if (lambda (tch)
                     (or (and (is-pc ch)
                              (eql caster-id (idnum-of tch)))
                         (and (is-npc ch)
                              (eql caster-id (- (mob-idnum-of tch))))))
                   (people-of (in-room-of ch)))))))

(defun dam-reduction-of (ch attacker)
  "Returns a floating point value of CH's damage reduction when attacked by ATTACKER."
  (let ((reduction 0))
    ;; good clerics get and alignment-based protection, up to 30% on
    ;; the full moon, up to 10% otherwise
    (when (and ch (is-class ch +class-cleric+) (is-good ch))
      (incf reduction (/ (alignment-of ch)
                         (if (eql (lunar-phase *lunar-day*) +moon-full+)
                             30
                             100))))
    ;; Sanctuary may be bypassed by an attacker with malefic violation
    ;; or righteous penetration
    (when (aff-flagged ch +aff-sanctuary+)
      (incf reduction
            (cond
              ((and ch
                    (or (not (is-affected-by-sanc ch attacker))
                        (is-vampire ch)))
               0)
              ((and (not (is-neutral ch))
                    (or (is-cleric ch) (is-knight ch)))
               (incf reduction 25))
              ((or (is-cyborg ch) (is-physic ch))
               (incf reduction 8))
              (t
               (incf reduction 15)))))
    ;; Zen of oblivity ranges up to about 35%
    (when (and (aff2-flagged ch +aff2-oblivity+) (is-neutral ch))
      (incf reduction (/ (+ (* (+ (level-of ch)
                                  (get-skill-bonus ch +zen-oblivity+))
                               10)
                            (- 1000 (abs (alignment-of ch)))
                            (* (check-skill ch +zen-oblivity+) 10))
                         100)))
    ;; No pain
    (when (aff-flagged ch +aff-nopain+)
      (incf reduction 25))
    ;; Berserk
    (when (aff2-flagged ch +aff2-berserk+)
      (incf reduction
            (if (is-barb ch)
                (/ (get-skill-bonus ch +skill-berserk+) 6)
                7)))
    ;; Damage control
    (when (aff3-flagged ch +aff3-damage-control+)
      (incf reduction (/ (get-skill-bonus ch +skill-damage-control+) 5)))
    ;; Alcoholics
    (when (> (aref (conditions-of ch) +drunk+) 5)
      (incf reduction (aref (conditions-of ch) +drunk+)))
    ;; Shield of righteousness
    (let ((caster (group-affect-caster ch +spell-shield-of-righteousness+)))
      (when caster
        (incf reduction
              (+ (/ (get-skill-bonus caster
                                     +spell-shield-of-righteousness+) 20)
                 (/ (alignment-of ch) 100)))))
    ;; Aria of Asylum
    (let ((caster (group-affect-caster ch +song-aria-of-asylum+)))
      (when caster
        (incf reduction
              (+ 5
                 (/ (get-skill-bonus caster
                                     +song-aria-of-asylum+) 10)
                 (/ (- 1000 (abs (alignment-of ch))) 100)))))
    ;; Lattice hardening
    (when (affected-by-spell ch +spell-lattice-hardening+)
      (incf reduction (/ (get-skill-bonus ch +spell-lattice-hardening+) 6)))
    ;; Stoneskin/barkskin/dermal hardening
    (let ((af (or (affected-by-spell ch +spell-stoneskin+)
                  (affected-by-spell ch +spell-barkskin+)
                  (affected-by-spell ch +spell-dermal-hardening+))))
      (when af
        (incf reduction
              (cond
                ((eql (kind-of af) +spell-stoneskin+)
                 (/ (level-of af) 4))
                ((or (eql (kind-of af) +spell-barkskin+)
                     (eql (kind-of af) +spell-dermal-hardening+))
                 (/ (level-of af) 6))
                (t
                 0)))))
    ;; Petrification
    (when (aff2-flagged ch +aff2-petrified+)
      (incf reduction 75))

    ;; Various forms of protection
    (when attacker
      (when (and (is-evil attacker) (aff-flagged ch +aff-protect-evil+))
        (incf reduction 8))
      (when (and (is-good attacker) (aff-flagged ch +aff-protect-good+))
        (incf reduction 8))
      (when (and (is-undead attacker) (aff2-flagged ch +aff2-protect-undead+))
        (incf reduction 8))
      (when (and (is-demon attacker) (aff2-flagged ch +aff2-prot-demons+))
        (incf reduction 8))
      (when (and (is-devil attacker) (aff2-flagged ch +aff2-prot-devils+))
        (incf reduction 8)))

    ;; Excessive armor class reduction
    (when (<= (armor-of ch) -300)
      (incf reduction (/ (+ (armor-of ch) 300) 5)))

    ;; Maximum damage reduction of 75%
    (/ (min reduction 75) 100)))


(defun calculate-damage-cap (attacker)
  (if attacker
      (max (+ 20 (level-of attacker) (* 2 (remort-gen-of attacker)))
           (+ (* 20 (level-of attacker)) (* 2 (level-of attacker) (remort-gen-of attacker))))
      7000))

(defun bad-attack-type (attack)
  (find attack (list +type-bleed+
                     +spell-poison+
                     +type-ablaze+
                     +type-acid-burn+
                     +type-pressure+
                     +type-suffocating+
                     +type-anguish+
                     +type-overload+
                     +type-suffering+
                     +spell-stigmata+
                     +type-drowning+
                     +spell-sickness+
                     +type-rad-sickness+
                     +skill-holy-touch+)))

(defun calculate-damage (attacker victim amount kind)
  ;; These only apply when there's an attacker
  (when attacker
    ;; Quad damage affects PvE
    (when (and (or (is-npc attacker) (is-npc victim))
               (affected-by-spell attacker +spell-quad-damage+))
      (setf amount (* amount 4)))
    ;; Double damage
    (when (aff3-flagged attacker +aff3-double-damage+)
      (setf amount (* amount 2)))
    ;; Barbarian charge
    (when (aff3-flagged attacker +aff3-inst-aff+)
      (let ((af (affected-by-spell attacker +skill-charge+)))
        (incf amount (* amount (floor (modifier-of af) 10)))))
    ;; Sanctification
    (let ((af (affected-by-spell attacker +spell-sanctification+)))
      (when af
        (cond
          ((and (is-evil victim) (not (is-soulless victim)))
           (incf amount (floor (* amount (remort-gen-of attacker)) 20)))
          ((and (not (eql attacker victim)) (is-good victim))
           (affect-remove attacker af))))))

  ;; Damage modifier for not standing
  (when (< (position-of victim) +pos-fighting+)
    (incf amount
          (floor (* amount (- +pos-fighting+ (position-of victim))) 3)))
  ;; Psychic resistance
  (when (and (spell-is-psionic kind)
             (affected-by-spell victim +spell-psychic-resistance+))
    (setf amount (floor amount 2)))
  ;; Magical protection
  (when (and (or (spell-is-magic kind) (spell-is-divine kind))
             (affected-by-spell victim +spell-magical-prot+))
    (setf amount (floor amount 4)))
  ;; Mana shield
  (when (and (affected-by-spell victim +spell-mana-shield+)
             (not (bad-attack-type kind)))
    (let ((absorbed (pin (floor (* amount (mana-shield-pct-of victim)) 100)
                         0
                         (- (mana-of victim)
                            (mana-shield-low-of victim)))))
      (decf amount absorbed)
      (decf (mana-of victim)
            (floor (* absorbed (- 1 (min (dam-reduction-of victim attacker)) 0.5))))

      (when (<= (mana-of victim) (mana-shield-low-of victim))
        (send-to-char victim "Your mana shield has expired.~%")
        (affect-from-char victim +spell-mana-shield+))))
  ;; Knights, clerics, and monks out of alignment
  (when (and attacker
             (or (and (is-neutral attacker)
                      (or (is-knight attacker) (is-cleric attacker)))
                 (and (not (is-neutral attacker))
                      (is-monk attacker))))
    (decf amount (floor amount 4)))

  ;; Lunar alignment damage bonuses and reduction
  (when (and attacker (is-class attacker +class-cleric+) (is-evil attacker))
    ;; evil clerics get an alignment-based damage bonus, up to 30% on
    ;; new moons, %10 otherwise.
    (incf amount (floor (* amount (- (alignment-of attacker)))
                        (if (eql (lunar-phase *lunar-day*) +moon-new+)
                            3000
                            10000))))
  ;; Damage reduction
  (decf amount (* amount (dam-reduction-of victim attacker)))

  ;; Very hard to damage mobs
  (when (or (is-pudding victim) (is-slime victim) (is-troll victim))
    (setf amount (floor amount 4)))

  ;; Damage by goodness
  (when (or (eql kind +spell-stigmata+)
            (eql kind +type-holyocean+))
    (when (or (is-undead victim) (is-evil victim))
      (setf amount (* amount 2)))
    (when (aff-flagged victim +aff-protect-good+)
      (setf amount (floor amount 2))))

  ;; Oxidation damage
  (when (eql kind +spell-oxidize+)
    (when (is-cyborg victim)
      (setf amount (* amount 2)))
    (when (affected-by-spell victim +spell-chemical-stability+)
      (setf amount (floor amount 4))))

  ;; Lightning damage of all kinds
  (when (member kind (list +spell-call-lightning+
                           +spell-lightning-bolt+
                           +spell-lightning-breath+
                           +javelin-of-lightning+
                           +skill-energy-field+
                           +skill-discharge+
                           +spell-electric-arc+
                           +type-egun-lightning+))
    (when (is-vampire victim)
      (setf amount (floor amount 2)))
    (when (and (room-is-outside (in-room-of victim))
               (eql (sky-of (weather-of (zone-of (in-room-of victim))))
                    :lightning))
      (setf amount (* amount 2)))
    (when (aff2-flagged victim +aff2-prot-lightning+)
      (setf amount (floor amount 2)))
    (when (is-cyborg victim)
      (setf amount (* amount 2))))

  ;; Hailstorms outside while raining
  (when (and (eql kind +spell-hailstorm+)
             (room-is-outside (in-room-of victim))
             (eql (sky-of (weather-of (zone-of (in-room-of victim))))
                  :raining))
    (setf amount (* amount 2)))

  ;; Fire damage of all kinds
  (when (member kind (list +spell-hell-fire+
                           +spell-taint+
                           +spell-burning-hands+
                           +spell-fireball+
                           +spell-flame-strike+
                           +spell-fire-elemental+
                           +spell-fire-breath+
                           +type-ablaze+
                           +spell-fire-shield+
                           +type-flamethrower+
                           +type-egun-plasma+))
    (when (char-withstands-fire victim)
      (setf amount (floor amount 2)))
    (when (or (is-troll victim) (is-pudding victim) (is-slime victim))
      (setf amount (* amount 4))))

  ;; Cold damage of all kinds
  (when (and (member kind (list +spell-cone-cold+
                                +spell-chill-touch+
                                +spell-icy-blast+
                                +spell-frost-breath+
                                +type-freezing+))
             (char-withstands-cold victim))
    (setf amount (floor amount 2)))

  ;; Heat damage
  (when (and (member kind (list +spell-steam-breath+
                                +type-boiling-pitch+))
             (aff3-flagged victim +aff3-prot-heat+))
    (setf amount (floor amount 2)))

  ;; Acid damage
  (when (and (member kind (list +type-acid-burn+
                                +spell-acidity+
                                +spell-acid-breath+))
             (affected-by-spell victim +spell-chemical-stability+))
    (setf amount (floor amount 2)))

  (floor (min (calculate-damage-cap attacker) amount)))

(defun remove-damage-sensitive-affects (ch)
  (when (aff-flagged ch +aff-sleep+)
    (affect-from-char ch +spell-sleep+)
    (affect-from-char ch +spell-melatonic-flood+)
    (affect-from-char ch +skill-sleeper+))
  (when (aff3-flagged ch +aff3-stasis+)
    (send-to-char ch "Emergency restart of system processes...~%")
    (setf (aff3-flags-of ch) (logandc2 (aff3-flags-of ch) +aff3-stasis+))))

(defun damage-counterattack (ch victim kind pos)
  nil)

(defun gain-damage-exp (ch victim amount)
  (when (eql (in-room-of ch) (in-room-of victim))
    (gain-exp ch (min (expt (level-of ch) 3)
                      (* (level-of ch) amount)))))

(defun pick-damage-location (victim kind hit-location)
  (let  ((location +wear-body+))
    (when (eql kind +spell-psychic-crush+)
      (setf location +wear-head+))
    (when (eql hit-location +wear-random+)
      (setf location (choose-random-limb victim)))
    (when (and (or (eql location +wear-finger-r+)
                   (eql location +wear-finger-l+))
               (get-eq victim +wear-hands+))
      (setf location +wear-hands+))
    (when (and (or (eql location +wear-ear-r+)
                   (eql location +wear-ear-l+))
               (get-eq victim +wear-head+))
      (setf location +wear-head+))

    location))

(defun damage-creature (ch victim base-amount weapon kind hit-location)
  "Causes the creature VICTIM to be damaged by an amount based on
BASE-AMOUNT.  CH is the dealer of the damage or NIL if a creature was
not responsible, KIND is the skill, spell or type of the damage, and
POS is the hit location.  Returns T if damage was dealt, and NIL if
the attack failed."
  (multiple-value-bind (can-damage sent-message)
      (can-damage-creature ch victim weapon kind)
    (when (or can-damage (not sent-message))
      (let ((amount (if can-damage (calculate-damage ch victim base-amount kind) 0))
            (damage-cap (calculate-damage-cap ch))
            (location (pick-damage-location victim kind hit-location)))
        (when (and ch (pref-flagged ch +pref-debug+))
          (send-to-char ch "&c[DAMAGE] ~a   dam: ~d  cap: ~d   type: ~d   wait: ~d   pos: ~d&n~%"
                        (name-of victim)
                        amount
                        damage-cap
                        kind
                        (if (link-of victim)
                            (wait-of (link-of victim))
                            (wait-state-of victim))
                        location))
        (when (pref-flagged victim +pref-debug+)
          (send-to-char victim "&c[DAMAGE] ~a   dam: ~d  cap: ~d   type: ~d   wait: ~d   pos: ~d&n~%"
                        (name-of victim)
                        amount
                        damage-cap
                        kind
                        (if (link-of victim)
                            (wait-of (link-of victim))
                            (wait-state-of victim))
                        location))
        (when ch
          (damage-counterattack ch victim kind location))

        (decf (hitp-of victim) amount)
        (when (is-pc victim)
          (incf (total-dam-of victim) amount))
        (when ch
          (gain-damage-exp ch victim amount))
        (remove-damage-sensitive-affects victim)

        (update-pos victim)

        (if (or (not (is-weapon kind))
                (zerop amount)
                (eql (position-of victim) +pos-dead+))
            (skill-message ch victim amount weapon kind)
            (damage-message ch victim amount weapon kind hit-location))

        (when (and ch (eql (master-of victim) ch))
          (stop-following victim))

        (cond
          ((eql (position-of victim) +pos-mortallyw+)
           (act victim :subject-emit "You are badly wounded, but will recover very slowly."
                :place-emit "$n is badly wounded, but will recover very slowly."))
          ((eql (position-of victim) +pos-incap+)
           (act victim :subject-emit "You are incapacitated but will probably recover."
                :place-emit "$n is incapacitated but will probably recover."))
          ((eql (position-of victim) +pos-stunned+)
           (act victim :subject-emit "You're stunned, but will probably regain consciousness again."
                :place-emit "$n is stunned, but will probably regain consciousness again."))
          ((eql (position-of victim) +pos-dead+)
           (act victim :subject-emit "You are dead!  Sorry..."
                :place-emit "$n is dead!  R.I.P.")
           ;; log death of victim
           (raw-kill victim ch kind))
          (t
           (when (> amount (floor (max-hitp-of victim) 4))
             (send-to-char victim "That really did HURT!~%"))
           (when (and (< (hitp-of victim)
                         (floor (max-hitp-of victim) 4))
                      (< (hitp-of victim) 200))
             (send-to-char victim "&rYou wish that you wounds would stop &RBLEEDING&r so much!&n~%"))))

        (plusp amount)))))

(defun obj-is-weapon (obj)
  "Returns non-NIL if OBJ is a weapon or energy gun.  Returns NIL if
it isn't or if OBJ is NIL."
  (and obj
       (or (is-obj-kind obj +item-weapon+)
           (is-obj-kind obj +item-energy-gun+))))


(defparameter +limb-probs+
  #(0							;#define WEAR_LIGHT      0
    3							;#define WEAR_FINGER_R   1
    3							;#define WEAR_FINGER_L   2
    4							;#define WEAR_NECK_1     3
    4							;#define WEAR_NECK_2     4
    35							;#define WEAR_BODY       5
    12							;#define WEAR_HEAD       6
    20							;#define WEAR_LEGS       7
    5							;#define WEAR_FEET       8
    10							;#define WEAR_HANDS      9
    25							;#define WEAR_ARMS      10
    50							;#define WEAR_SHIELD    11
    0							;#define WEAR_ABOUT     12
    10							;#define WEAR_WAIST     13
    5							;#define WEAR_WRIST_R   14
    5							;#define WEAR_WRIST_L   15
    0							;#define WEAR_WIELD     16
    0							;#define WEAR_HOLD      17
    15							;#define WEAR_CROTCH    18
    5							;#define WEAR_EYES      19
    5							;#define WEAR_BACK      20
    0							;#define WEAR_BELT      21
    10							;#define WEAR_FACE      22
    4							;#define WEAR_EAR_L     23
    4							;#define WEAR_EAR_R     24
    0							;#define WEAR_WIELD_2   25
    0							;#define WEAR_ASS       26
    ))
(defparameter *cumulative-limb-probs* (loop for num across +limb-probs+
                                         summing num into sum
                                         collect (+ num sum)))
(defparameter *max-limb-prob* (car (last *cumulative-limb-probs*)))

(defun choose-random-limb (ch)
  (let ((limb (position (random-range 0 *max-limb-prob*)
                        *cumulative-limb-probs*
                        :test #'<)))
    (cond
      ((or (/= limb +wear-shield+)
           (get-eq ch +wear-shield+))
       limb)
      ((randomly-true 3)
       +wear-arms+)
      (t
       +wear-body+))))

(defun uncovered-implants (ch kind)
   (loop
      for implant across (implants-of ch)
      as equip across (equipment-of ch)
      when (and implant
                (or (null kind) (is-obj-kind implant kind))
                (null equip))
      collect implant))


(defun get-random-uncovered-implant (ch kind)
  "Return a random uncovered implant worn by CH of the given KIND,
which may be NIL."
  (random-elt (uncovered-implants ch kind)))

(defun get-next-weapon (ch type)
  "Pick the next weapon that the creature will strike with"
  (let* ((wielded (get-eq ch +wear-wield+))
         (dual-wielded (get-eq ch +wear-wield-2+))
         (dual-prob (if (and wielded dual-wielded)
                        (* (- (weight-of wielded) (weight-of dual-wielded)) 2)
                        0)))
    (cond
      ;; Specifically using implant skills
      ((or (= type +skill-implant-w+)
           (= type +skill-adv-implant-w+))
       (get-random-uncovered-implant ch +item-weapon+))
      ;; Dual wielding
      ((and (obj-is-weapon dual-wielded)
            (> (+ (/ (* 2 (skill-of ch (if (affected-by-spell ch +skill-neural-bridging+)
                                           +skill-neural-bridging+
                                           +skill-second-weapon+))) 3) dual-prob)
               (random-range 50 150)))
       dual-wielded)
      ;; Normal wield
      ((and wielded (randomly-true))
       wielded)
      ;; Check equipment on hands
      ((and (obj-is-weapon (get-eq ch +wear-hands+))
            (randomly-true 6))
       (get-eq ch +wear-hands+))
      ;; Check for implanted weapons in hands
      ((and (obj-is-weapon (get-implant ch +wear-hands+))
            (randomly-true 3))
       (get-implant ch +wear-hands+))
      ;; Check for weapon on feet
      ((and (obj-is-weapon (get-eq ch +wear-feet+))
            (> (skill-of ch +skill-kick+)
               (random-range 10 170)))
       (get-eq ch +wear-feet+))
      ;; Check for weapon on head
      ((and (obj-is-weapon (get-eq ch +wear-feet+))
            (randomly-true 3))
       (get-eq ch +wear-head+))
      ;; Default to wielded weapon, if one
      (wielded
       wielded)
      ;; Just EQ on the bare hands, then
      (t
       (get-eq ch +wear-hands+)))))

(defun check-reputations (ch vict)
  "Return T if innocence protections should be enforced.  Return NIL
if the players' reputations allow it."
  (when (arena-combat-p ch vict)
    (return-from check-reputations nil))
  (when (immortal-level-p ch)
    (return-from check-reputations nil))
  (when (is-npc vict)
    (return-from check-reputations nil))
  (let ((ch-master (loop for tch = ch then (master-of tch)
                        while (master-of tch)
                        finally (return tch)))
        (vict-master (loop for tch = vict then (master-of tch)
                        while (master-of tch)
                        finally (return tch))))
    (when (is-npc ch-master)
      (return-from check-reputations nil))
    (when (zerop (reputation-of ch-master))
      (send-to-char ch "You are currently an innocent.  If you wish to be a player killer, type PK on yes.~%")
      (return-from check-reputations t))
    (when (zerop (reputation-of vict-master))
      (send-to-char ch "~a is an innocent and is immune to player violence.~%" (name-of vict))
      (send-to-char vict "~a has just tried to attack you but was prevented by your innocence.~%" (name-of ch))
      (return-from check-reputations t))
    nil))

(defun backstab-multiplier (ch)
  (+ 2
     (floor (1+ (level-of ch)) 10)
     (floor (* 6 (max 0 (- (get-skill-bonus ch +skill-backstab+) 50))) 50)))

(defun do-casting-weapon (ch weapon)
  (let ((spellnum (aref (value-of weapon) 0))
        (room (in-room-of ch))
        (wielded (or (eql (worn-on-of weapon) +wear-wield+)
                     (eql (worn-on-of weapon) +wear-wield-2+))))
    (unless (< 0 spellnum +top-spell-define+)
      (slog "Invalid spell number detected on weapon ~d~%" (vnum-of weapon))
      (return-from do-casting-weapon))

    (unless (or (and (or (spell-is-magic spellnum)
                         (is-obj-stat weapon +item-magic+))
                     (room-flagged room +room-nomagic+))
                (and (spell-is-physics spellnum)
                     (room-flagged room +room-noscience+))
                (and (spell-is-psionic spellnum)
                     (room-flagged room +room-nopsionics+))
                (not (randomly-true (max 2 (- 100 (level-of ch)
                                              (int-of ch)
                                              (floor (check-skill ch spellnum) 8))))))
      (let ((action-msg (or (action-desc-of weapon)
                            (format nil "$p begins to hum and shake~@[ in your hand~]!" wielded)))
            (room-emit (format nil "$p begins to hum and shake~@[ in $n's hand~]!" wielded)))
        (send-to-char ch "&c")
        (act ch :item weapon :subject-emit action-msg :place-emit room-emit)
        (send-to-char ch "&n")

        (cond
          ((and (not (immortalp ch))
                (or (is-dwarf ch)
                    (is-cyborg ch)
                    (and (or (is-obj-stat weapon +item-magic+)
                             (spell-is-magic spellnum))
                         (randomly-true (+ (int-of ch) 3)))))
           ;; Drop the weapon if wielded
           (cond
             (wielded
              (act ch :item weapon
                   :subject-emit "$p shocks you!  You are forced to drop it!"
                   :place-emit "$n cries out as $p shocks $m!")
              (if (> (random-range 0 20)
                     (dex-of ch))
                  (obj-to-room (unequip-char ch (worn-on-of weapon) :worn nil) room)
                  (obj-to-char (unequip-char ch (worn-on-of weapon) :worn nil) ch)))
             (t
              (act ch :item weapon
                   :subject-emit "$p blasts the hell out of you!  OUCH!"
                   :place-emit "$n cries out as $p shocks $m!")
              (damage-creature nil ch (dice 3 4) nil +spell-shocking-grasp+ (worn-on-of weapon)))))
          ((or (spell-flagged spellnum +mag-damage+)
               (violentp (aref *spell-info* spellnum))
               (logtest (targets-of (aref *spell-info* spellnum)) +tar-unpleasant+))
           ;; casting it at an opponent
           (when (fighting-of ch)
             (call-magic ch (random-elt (fighting-of ch)) nil 0 spellnum
                         (level-of ch) +cast-wand+)))
          (t
           ;; casting it on self
           (call-magic ch ch nil 0 spellnum (level-of ch) +cast-wand+)))))))

(defun attack (ch victim type)
  (let ((w-type type)
        (damage 0))
    (unless (eql (in-room-of ch)
                 (in-room-of victim))
      (remove-combat ch victim)
      (remove-combat victim ch)
      (return-from attack))

    (when (and ch victim (not (can-attack ch victim)))
      (return-from attack))

    (when (and (aff2-flagged ch +aff2-petrified+)
               (< (level-of ch)
                  +lvl-element+))
      (act ch :target victim
           :subject-emit (random-elt '("You want to fight back against $N's attack, but cannot!"
                                       "You have been turned to stone and cannot fight!"
                                       "You cannot fight back!  You are petrified!")))
      (return-from attack))

    (when (and (is-newbie victim)
               (is-pc ch)
               (< (level-of ch)
                  +lvl-god+)
               (is-npc victim)
               (not *mini-mud*))
      (act ch :target victim
           :subject-emit "$N is currently under new character protection."
           :target-emit "You are protected by the gods against $n's attack!")
      (slog "~a protected against ~a ( hit ) at ~a" (name-of victim)
            (name-of ch)
            (number-of (in-room-of victim)))
      (remove-combat ch victim)
      (remove-combat victim ch)
      (return-from attack))

    (when (check-reputations ch victim)
      (remove-combat ch victim)
      (remove-combat victim ch)
      (return-from attack))

    (when (mounted-of ch)
      (send-to-char ch "You had better dismount first.~%")
      (return-from attack))

    (when (mounted-of victim)
      (perform-dismount victim)
      (act victim :target ch
           :subject-emit "You are knocked from your mount by $N's attack!"))

    ;; Deduct move points for the effort
    (when (and (is-pc ch) (> (move-of ch) 20))
      (decf (move-of ch))
      (when (and (is-drow ch) (room-is-sunny (in-room-of ch)))
        (decf (move-of ch)))
      (when (and (is-cyborg ch) (plusp (broken-component-of ch)))
        (decf (move-of ch))))

    (let ((weapon (get-next-weapon ch type)))
      (when weapon
        (cond
          ((and (is-energy-gun weapon)
                (contains-of weapon)
                (plusp (cur-energy (first (contains-of weapon)))))
           (setf w-type (+ (gun-type weapon) +type-egun-laser+))
           (let ((cost (min (cur-energy (first (contains-of weapon)))
                            (gun-discharge weapon))))
             (decf (cur-energy (first (contains-of weapon)))
                   cost)
             (when (zerop (cur-energy (first (contains-of weapon))))
               (act ch
                    :item weapon
                    :subject-emit "$p has been depleted of fuel.  Replace cell before further use."))))
          ((or (is-energy-gun weapon)
               (is-gun weapon))
           (setf w-type +type-bludgeon+))
          ((is-obj-kind weapon +item-weapon+)
           (setf w-type (+ +type-hit+ (aref (value-of weapon) 3))))
          (t
           (setf w-type +type-hit+))))

      (when (null weapon)
        (cond
          ((and (is-npc ch)
                (plusp (attack-type-of (shared-of ch))))
           (setf w-type (+ +type-hit+ (attack-type-of (shared-of ch)))))
          ((and (is-tabaxi ch) (randomly-true))
           (setf w-type +type-claw+))
          (t
           (setf w-type +type-hit+))))

      (let ((thaco (calculate-thaco ch victim weapon))
            (victim-ac (max (armor-of victim)
                            (- (floor (* (level-of ch) 4) 15))))
            (diceroll (random-range 1 20)))
        (when (pref-flagged ch +pref-debug+)
          (send-to-char ch "&c[HIT] thac0:~a   roll:~a  AC:~a&n~%" thaco diceroll victim-ac))
        (when (and (= diceroll 1)
                   (>= (position-of victim) +pos-fighting+)
                   (> (skill-of victim +skill-counter-attack+) 70))
          (send-to-char victim "You launch a counter attack!~%")
          (attack victim ch +type-undefined+)
          (return-from attack))

        ;; Decide whether this is a hit or a miss
        (when (and (< diceroll 20)
                   (awakep victim)
                   (or (= diceroll 1)
                       (> (- thaco diceroll) victim-ac)))
          ;; Miss - deal 0 damage to emit miss message
          (if (member type (list +skill-backstab+
                                 +skill-circle+
                                 +skill-second-weapon+
                                 +skill-cleave+))
              (damage-creature ch victim 0 weapon type (choose-random-limb victim))
              (damage-creature ch victim 0 weapon w-type (choose-random-limb victim)))

          ;; Start the fight!
          (pushnew ch (fighting-of victim))
          (pushnew victim (fighting-of ch))
          (return-from attack))

        ;; It's a HIT
        (when (> (skill-of ch +skill-dbl-attack+) 60)
          (gain-skill-proficiency ch +skill-dbl-attack+))
        (when (> (skill-of ch +skill-triple-attack+) 60)
          (gain-skill-proficiency ch +skill-triple-attack+))
        (when (> (skill-of ch +skill-neural-bridging+) 60)
          (gain-skill-proficiency ch +skill-neural-bridging+))

        ;; okay, it's a hit.  calculate limb
        (let ((limb (cond
                      ((or (= type +skill-backstab+)
                           (= type +skill-circle+))
                       +wear-back+)
                      ((or (= type +skill-impale+)
                           (= type +skill-lunge-punch+))
                       +wear-body+)
                      ((= type +skill-behead+)
                       +wear-neck-1+)
                      (t
                       (choose-random-limb victim)))))

          ;; Calculate damage
          (cond
            ((and weapon
                  (<= +type-egun-laser+ w-type +type-egun-top+))
             ;; ranged, dex base damage
             (setf damage (+ (getf (aref +dex-app+ (dex-of ch)) :to-dam)
                             (hitroll-of ch))))
            (t
             ;; melee, str base damage
             (setf damage (+ (getf (aref +str-app+ (str-of ch)) :to-dam)
                             (damroll-of ch)))))

          (cond
            (weapon
             ;; Calculate weapon damage
             (when (and (is-evil victim)
                        (is-obj-stat weapon +item-bless+))
               (incf damage 5))
             (when (and (is-good victim)
                        (is-obj-stat weapon +item-damned+))
               (incf damage 5))

             (cond
               ((or (is-obj-kind weapon +item-weapon+)
                    (is-energy-gun weapon))
                (incf damage (dice (aref (value-of weapon) 1)
                                   (aref (value-of weapon) 2)))
                (let ((tmp-dam damage))
                  (when (invalid-char-class ch weapon)
                    (setf damage (floor damage 2)))
                  (when (is-npc ch)
                    (incf tmp-dam (dice (damnodice-of (shared-of ch))
                                        (damsizedice-of (shared-of ch))))
                    (setf damage (max tmp-dam damage)))
                  (when (plusp (vnum-of weapon))
                    (let ((spec (find (vnum-of weapon) (remove-if-not #'identity (weap-spec-of ch)) :key 'vnum-of)))
                      (when spec
                        (incf damage (second spec)))))
                  (when (and (is-two-hand weapon)
                             (is-barb ch)
                             (not (is-energy-gun weapon))
                             (eql (worn-on-of weapon)
                                  +wear-wield+))
                    (let ((dam-add (floor (weight-of weapon) 4)))
                      (when (> (skill-of ch +skill-discipline-of-steel+) 60)
                        (let ((bonus (get-skill-bonus ch +skill-discipline-of-steel+))
                              (weight (weight-of weapon)))
                          (incf dam-add (floor (* bonus weight)
                                               100)))
                        (incf damage dam-add))))))
               ((is-obj-kind weapon +item-armor+)
                (incf damage (floor (aref (value-of weapon) 0) 3)))
               (t
                (incf damage (+ (dice 1 4) (random-range 0 (weight-of weapon)))))))
            (t
             ;; Bare hand damage
             (incf damage (random-range 0 3))
             (when (is-monk ch)
               (incf damage (random-range (floor (level-of ch) 4)
                                          (level-of ch))))
             (when (is-barb ch)
               (incf damage (random-range 0 (floor (level-of ch) 2))))))

          (when (and weapon
                     (is-energy-gun weapon)
                     (not (>= +type-egun-laser+ w-type +type-egun-top+)))
            ;; Bludgeoning with the gun
            (setf damage (floor damage 10)))

          ;; Enforce level limit and minimum 1 pt damage
          (setf damage (pin damage 1 (+ 30 (* (level-of ch) 8))))
          (cond
            ((eql type +skill-cleave+)
             (setf damage (* damage
                             (if (> (skill-of ch +skill-great-cleave+) 50)
                                 (+ 5 (floor (remort-gen-of ch) 5)) 4))))

            ((eql type +skill-backstab+)
             (gain-skill-proficiency ch type)
             (when (is-thief ch)
               (setf damage (* damage (backstab-multiplier ch)))
               (incf damage (random-range 0 (floor (- (skill-of ch +skill-backstab+)
                                                      (learned ch)) 2))))

             (damage-creature ch victim damage weapon +skill-backstab+ +wear-back+)
             (return-from attack))
            ((eql type +skill-circle+)
             (gain-skill-proficiency ch type)
             (when (is-thief ch)
               (setf damage (* damage (max 2 (floor (backstab-multiplier ch) 2))))
               (incf damage (random-range 0 (floor (- (skill-of ch +skill-circle+)
                                                      (learned ch)) 2))))

             (damage-creature ch victim damage weapon +skill-circle+ +wear-back+)
             (return-from attack))

            (t
             ;; Normal hit damage
             (let ((ablaze-level 0))
               (when (and weapon
                          (is-obj-kind weapon +item-weapon+)
                          (not (is-energy-gun weapon)))
                 (when (and (>= (aref (value-of weapon) 3) 0)
                            (< (aref (value-of weapon) 3) (- +top-attacktype+ +type-hit+))
                            (is-barb ch))
                   (let ((skill (aref +weapon-proficiencies+ (aref (value-of weapon) 3))))
                     (when (plusp skill)
                       (gain-skill-proficiency ch skill))))
                 (when (is-obj-stat2 weapon +item2-ablaze+)
                   (let ((af (affected-by-spell weapon +spell-flame-of-faith+)))
                     (when af
                       (incf damage (random-range 1 10))
                       (setf ablaze-level (level-of af))))))
               (when (and (>= +type-egun-laser+ w-type +type-egun-top+)
                          (> (skill-of ch +skill-energy-weapons+) 60))
                 (gain-skill-proficiency ch +skill-energy-weapons+))

               ;; Finally deal the damage
               (damage-creature ch victim damage weapon w-type limb)

               ;; Ignite the victim if applicable
               (when (and (plusp ablaze-level)
                          (not (aff2-flagged victim +aff2-ablaze+))
                          (not (mag-savingthrow victim 10 +saving-spell+))
                          (not (char-withstands-fire victim)))
                 (act victim :all-emit "$y body suddenly ignites into flame!")
                 (ignite victim))

               ;; Hitting a rust monster with something metal
               (when (and weapon
                          (is-ferrous weapon)
                          (is-npc victim)
                          (eql (func-of (shared-of victim)) 'rust-monster)
                          (or (not (is-obj-stat weapon +item-magic+))
                              (not (mag-savingthrow ch (level-of victim) +saving-rod+)))
                          (or (not (is-obj-stat weapon +item-magic-nodispel+))
                              (not (mag-savingthrow ch (level-of victim) +saving-rod+))))
                 (act ch :target victim :item weapon
                      :subject-emit "$p spontaneously oxidizes and crumbles into a pile of dust!"
                      :place-emit "$p crumbles into rust on contact with $N!")
                 (extract-obj weapon)
                 (let ((weapon (read-object +rustpile-vnum+)))
                   (when weapon
                     (obj-to-room weapon (in-room-of ch)))))

               ;; Casting weapons
               (when (and weapon (is-obj-kind weapon +item-weapon+))
                 (cond
                   ((func-of (shared-of weapon))
                    (funcall (func-of (shared-of weapon)) ch weapon 0 nil +special-combat+))
                   ((is-obj-stat2 weapon +item2-cast-weapon+)
                    (do-casting-weapon ch weapon))))))))))

    ;; Start the fight!
    (pushnew ch (fighting-of victim))
    (pushnew victim (fighting-of ch))))

(defun debug-to-chars (chars fmt &rest args)
  (dolist (tch chars)
    (when (pref-flagged tch +pref-debug+)
      (send-to-char tch "~?" fmt args))))

(defun perform-creature-violence (ch)
  (let ((prob (calculate-attack-probability ch))
        (die-roll (random-range 0 300)))

    (debug-to-chars (cons ch (fighting-of ch))
                    "&c[COMBAT] ~a   prob:~a   roll:~a   wait:~a&n~%"
                    (name-of ch) (min 100 (+ prob 15)) die-roll (wait-of ch))

    ;; Handle regular hits
    (when (>= (min 100 (+ prob 15)) die-roll)
      (handler-case
          (loop
             for time from 0 upto (min 3 (floor (level-of ch) 8))
             as die-roll = (random-range (* time 24) (* time 40))
             as target = (random-elt (fighting-of ch))
             do
               (debug-to-chars (cons ch (fighting-of ch))
                               "&c[ATTACK] ~a -> ~a  prob:~a   roll:~a   wait:~a&n~%"
                               (name-of ch) (name-of target) prob die-roll (wait-of ch))
               (cond
                 ((< (position-of ch) +pos-fighting+)
                  (when (< (wait-of ch) 10)
                    (send-to-char ch "You can't fight while sitting!!~%")))
                 ((>= prob die-roll)
                  (attack ch target +type-undefined+))))
        (creature-died (death)
          (when (eql (creature-of death) ch)
            (return-from perform-creature-violence)))))

    ;; Handle cyborg normal implant hits
    (when (and (is-cyborg ch) (fighting-of ch) (uncovered-implants ch +item-weapon+))
      (unless (fighting-of ch)
        (when (< (random-range 1 100) (skill-of ch +skill-implant-w+))
          (let ((implant-prob 25))
            (when (> (skill-of ch +skill-implant-w+) 100)
              (incf implant-prob (+ (remort-gen-of ch)
                                    (floor (- (skill-of ch +skill-implant-w+) 100) 2))))
            (when (< (random-range 0 100) implant-prob)
              (handler-case
                  (attack ch (random-elt (fighting-of ch)) +skill-implant-w+)
                (creature-died (death)
                  (when (eql (creature-of death) ch)
                    (return-from perform-creature-violence))))))))

      ;; Handle cyborg advanced implant hits
      (when (< (position-of ch) +pos-fighting+)
        (when (< (wait-of ch) 10)
          (send-to-char ch "You can't fight while sitting!!~%"))
        (return-from perform-creature-violence))

      (when (< (random-range 1 100) (skill-of ch +skill-adv-implant-w+))
        (let ((implant-prob 25))
          (when (> (skill-of ch +skill-adv-implant-w+) 100)
            (incf implant-prob (+ (remort-gen-of ch)
                                  (floor (- (skill-of ch +skill-adv-implant-w+) 100) 2))))
          (when (< (random-range 0 100) implant-prob)
            (handler-case
                (attack ch (random-elt (fighting-of ch)) +skill-adv-implant-w+)
              (creature-died (death)
                (when (eql (creature-of death) ch)
                  (return-from perform-creature-violence))))))))

    (when (and (fighting-of ch)
               (is-npc ch)
               (eql (position-of ch) +pos-fighting+)
               (zerop (wait-of ch))
               (>= (min 100 prob) (random-range 0 300)))
      (mobile-battle-activity ch))))

(defun stop-separated-fights (ch)
  (dolist (tch (copy-list (fighting-of ch)))
    (unless (eql (in-room-of ch) (in-room-of tch))
      (remove-combat ch tch))
    (when (eql ch tch)
      (remove-combat ch tch))))

(defun perform-violence ()
  "Control every fight in the game"
  (dolist (ch *characters*)
    (stop-separated-fights ch)
    (unless (or (eql (position-of ch) +pos-dead+)
                (null (fighting-of ch)))
      (with-simple-restart (continue "Continue from signal in combat update")
        (if *break-on-error*
            (perform-creature-violence ch)
            (handler-bind ((error (lambda (str)
                                    (errlog "System error: ~a" str))))
              (perform-creature-violence ch)))))))

