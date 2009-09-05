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

(defun damage-eq (ch obj amount)
  nil)

;; damage kinds are as follows:
;;   crushing slashing impaling fire ice heat cold electric disease
;;   poison radiance necros

;; damage modifiers are as follows:
;;   magic divine psionic
(defun damage-creature (victim amount kind pos)
  nil)
