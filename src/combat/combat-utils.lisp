(in-package #:tempus)

(defun apply-soil-to-char (ch obj kind pos)
  (when (eql pos +wear-random+)
    (setf pos (random-elt (loop
                             for idx upto +num-wears+
                             when (and (not (illegal-soilpos pos))
                                       (not (and (null (get-eq ch idx)) (char-soiled ch idx kind)))
                                       (not (and (get-eq ch idx) (obj-soiled (get-eq ch idx) kind))))
                             collect idx))))

  (unless (illegal-soilpos pos)
    (let ((eq (get-eq ch pos)))
      (cond
        ((and eq
              (or (eql eq obj) (null obj))
              (not (is-obj-stat2 eq +item2-nosoil+))
              (not (obj-soiled eq kind)))
         (setf (ldb (byte kind 1) (soilage-of eq)) 1))
        ((not (char-soiled ch pos kind))
         (setf (ldb (byte kind 1) (soilage-of ch)) 1)))))

  (when (and (eql kind +soil-blood+)
             obj
             (eql (vnum-of obj) +blood-vnum+))
    (incf (timer-of obj) (max 1 (- (timer-of obj) 5))))

  pos)

(defun update-pos (victim)
  (cond
    ((<= (hitp-of victim) -11)
     (setf (position-of victim) +pos-dead+))
    ((<= (hitp-of victim) -6)
     (setf (position-of victim) +pos-mortallyw+))
    ((<= (hitp-of victim) -3)
     (setf (position-of victim) +pos-incap+))
    ((<= (hitp-of victim) 0)
     (setf (position-of victim) +pos-stunned+))
    ((= (position-of victim) +pos-sleeping+)
     ;; wake them up from their nap
     ;; TODO: check implementation of Creature::setPosition
     (setf (position-of victim) +pos-resting+))
    ((and (or (= (position-of victim) +pos-standing+)
              (= (position-of victim) +pos-flying+))
          (fighting-of victim))
     ;; if everything is normal and they're fighting, set them fighting
     (setf (position-of victim) +pos-fighting+))
    ((and (> (position-of victim) +pos-stunned+)
          (< (position-of victim) +pos-fighting+)
          (fighting-of victim))
     ;; if they're alive, not stunned, in a fight, and not +pos-fighting+
     (when (and (is-npc victim) (zerop (wait-of victim)))
       (cond
         ((< (position-of victim) +pos-fighting+)
          (when (or (not (aff3-flagged victim +aff3-gravity-well+))
                    (< (random-range 1 20) (str-of victim)))
            (setf (position-of victim) +pos-fighting+)
            (when (= (position-of victim) +pos-fighting+)
              (act victim :place-emit "$n scrambles to $s feet!")))
          (wait-state victim +pulse-violence+))
         (t
          (setf (position-of victim) +pos-fighting+)))))
    ((or (not (is-npc victim)) (plusp (wait-of victim)))
     ;; handle players or waiting mobs
     (when (= (position-of victim) +pos-stunned+)
       ;; wear off being stunned
       (setf (position-of victim) +pos-resting+)))
    ((and (in-room-of victim)
          (room-is-open-air (in-room-of victim))
          (not (aff3-flagged victim +aff3-gravity-well+))
          (/= (position-of victim) +pos-flying+))
     ;; set flying if in open air
     (setf (position-of victim) +pos-flying+))
    ((and (aff3-flagged victim +aff3-gravity-well+)
          (>= (random-range 1 20) (str-of victim)))
     ;; overcome by gravity well
     nil)
    ((and (< (position-of victim) +pos-fighting+) (fighting-of victim))
     ;; getting up while fighting
     (act victim :place-emit "$n scrambles to $s feet!")
     (setf (position-of victim) +pos-fighting+)
     (wait-state victim +pulse-violence+))
    ((< (position-of victim) +pos-fighting+)
     ;; getting up normally
     (act victim :place-emit "$n stands up.")
     (setf (position-of victim) +pos-standing+)
     (wait-state victim +pulse-violence+))))
