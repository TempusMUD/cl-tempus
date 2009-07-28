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

    (dolist (af (copy-list (affected-of ch)))
      (when (clear-at-death af)
        (affect-remove ch af)))

    (setf (aff2-flags ch) (logandc2 (aff2-flags ch) +aff2-petrified+))

    (when (is-npc ch)
      (incf (kills-of (shared-of ch))))

    (cond
      ((is-arena-combat killer ch)
       (arena-die ch))
      ((and (is-npk-combat killer ch)
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
