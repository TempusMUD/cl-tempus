(in-package #:tempus)

(defun perform-cyborg-activate (ch skill)
  "TODO: Implement perform-cyborg-activate"
  (cond
    ((zerop (check-skill ch skill))
     (send-to-char ch "You do not have this program in memory.~%"))
    ((< (check-skill ch skill) 40)
     (send-to-char ch "Partial installation insufficient for operation.~%"))
    ((affected-by-spell ch skill)
     (let ((skill-name (spell-to-str skill)))
       (send-to-char ch "&cERROR:&n ~a ~a already activated.~%"
                     skill-name
                     (is-are skill-name))))
    (t
     (let ((affs nil)
           (to-char nil)
           (to-room nil)
           (opposite-skill nil))
       (flet ((add-affect (&key location modifier bitvector aff-index)
                (push (make-instance 'affected-type
                                     :kind skill
                                     :duration -1
                                     :owner ch
                                     :level (level-of ch)
                                     :modifier (or modifier 0)
                                     :location (or location 0)
                                     :bitvector (or bitvector 0)
                                     :aff-index (or aff-index 0))
                      affs)))

         (alexandria:switch (skill)
           (+skill-adrenal-maximizer+
            (add-affect :location +apply-move+ :modifier -30)
            (add-affect :bitvector +aff-adrenaline+
                        :aff-index 1
                        :location +apply-speed+
                        :modifier (+  1
                                      (floor (level-of ch) 8)
                                      (floor (remort-gen-of ch) 2)))
            (setf to-char "Shukutei adrenal maximizations enabled."))
           (+skill-energy-field+
            (add-affect :bitvector +aff2-energy-field+
                        :aff-index 2
                        :location +apply-move+
                        :modifier -40)
            (setf to-char "Energy fields activated.")
            (setf to-room "A field of energy hums to life about $n's body."))
           (+skill-reflex-boost+
            (add-affect :bitvector +aff2-haste+
                        :aff-index 2
                        :location +apply-move+
                        :modifier -40)
            (setf to-char "Activating reflex boosters."))
           (+skill-melee-combat-tac+
            (add-affect :location +apply-move+ :modifier -30)
            (setf to-char "Activating melee combat tactics."))
           (+skill-power-boost+
            (add-affect :location +apply-move+ :modifier -40)
            (add-affect :location +apply-str+ :modifier (1+ (floor (level-of ch)
                                                                   16)))
            (setf to-char "Power boost enabled.")
            (setf to-room "$n boosts $s power levels!  Look out!"))
           (+skill-motion-sensor+
            (add-affect :location +apply-move+ :modifier -30)
            (setf to-char "Activating motion sensors."))
           (+skill-damage-control+
            (add-affect :bitvector +aff3-damage-control+
                        :aff-index 3
                        :location +apply-move+
                        :modifier -30)
            (setf to-char "Activating damage control systems."))
           (+skill-hyperscan+
            (add-affect :location +apply-move+ :modifier -10)
            (setf to-char "Activating hyperscanning device."))
           (+skill-stasis+
            (cond
              ((>= (position-of ch) +pos-flying+)
                (send-to-char ch "Go into stasis while flying?!?!?~%"))
              (t
               (setf (aff3-flags-of ch) (logior (aff3-flags-of ch) +aff3-stasis+))
               (setf (position-of ch)
                     +pos-sleeping+)
               (wait-state ch (rl-sec 10))
               (act ch
                    :subject-emit "Entering static state.  Halting system processes."
                    :place-emit "$n lies down and enters a static state."))))
           (+skill-radionegation+
            (add-affect :location +apply-move+ :modifier -15)
            (setf to-char "Radionegation device activated.")
            (setf to-room "$n starts humming loudly."))
           (+skill-neural-bridging+
            (add-affect :location +apply-move+ :modifier -37)
            (setf to-char "Activating cogenic neural bridging."))
           (+skill-offensive-pos+
            (add-affect :location +apply-ac+ :modifier (+ (level-of ch)
                                                          (* 2 (remort-gen-of ch))))
            (add-affect :location +apply-damroll+
                        :modifier (floor (+ (level-of ch) (* 2 (remort-gen-of ch)))
                                         10))
            (add-affect :location +apply-hitroll+
                        :modifier (floor (+ (level-of ch) (* 2 (remort-gen-of ch)))
                                         7))

            (setf to-char "Offensive posturing enabled.")
            (setf opposite-skill +skill-defensive-pos+))
           (+skill-defensive-pos+
            (add-affect :location +apply-ac+
                        :modifier (- (+ (level-of ch) (* 2 (remort-gen-of ch)))))
            (add-affect :location +apply-damroll+
                        :modifier (- (floor (+ (level-of ch) (* 2 (remort-gen-of ch))) 10)))
            (add-affect :location +apply-hitroll+
                        :modifier (- (floor (+ (level-of ch) (* 2 (remort-gen-of ch))) 7)))
            (setf to-char "Defensive posturing enabled.")
            (setf opposite-skill +skill-offensive-pos+)
            nil)
           (+skill-nanite-reconstruction+
            (add-affect :location +apply-move+ :modifier -300)
            (setf to-char "Nanite reconstruction in progress..")
            nil)
           (t
            (send-to-char ch "ERROR: unknown mode occurred in switch.~%"))))
       (cond
         ((and opposite-skill
               (affected-by-spell ch opposite-skill))
          (let ((skill-name (spell-to-str opposite-skill)))
            (send-to-char ch "&cERROR:&n ~a ~a already activated.~%"
                          skill-name
                          (is-are skill-name))))
         ((minusp (+ (move-of ch)
                     (reduce '+ (remove-if-not
                                 (alexandria:curry 'eql +apply-move+)
                                 affs)
                             :key 'modifier-of)))
            (send-to-char ch "&cERROR:&n Energy levels too low to activate ~a.~%"
                          (spell-to-str skill)))
         (t
          (dolist (aff affs)
            (affect-to-char ch aff))
          (when to-char
            (send-to-char ch "~a~%" to-char))
          (when to-room
            (act ch :place-emit to-room))

          (when (and (eql +skill-energy-field+
                          (room-is-watery (in-room-of ch))))
            (dolist (tch (copy-list (people-of (in-room-of ch))))
              (damage-creature ch tch (dice 4 (level-of ch)) nil
                               +skill-energy-field+ nil)
              (wait-state tch (rl-sec 1)))
            (send-to-char ch "DANGER: Hazardous short detected!!  Energy fields shutting down.~%"))))))))
