(in-package #:tempus)

(defparameter +rubble-vnum+ 16151)
(defparameter *cavern-zones* (make-hash-table))

(define-special abandoned-cavern (trigger self ch command vars) (+spec-rm+)
  (declare (ignore command vars))
  (when (and (eql trigger 'command)
             (not (immortalp ch))
             (not (aff-flagged ch +aff-sneak+)))
    (let ((count (incf (gethash (zone-of self) *cavern-zones* 0))))
      (cond
        ((> count 30)
         (setf (gethash (zone-of self) *cavern-zones*) 0)
         (act ch :all-emit "The cavern begins to shake and rocks start falling from the ceiling!")
         (dolist (tch (copy-list (people-of self)))
           (when (and (not (immortalp tch))
                      (> (random-range 5 30)
                         (dex-of tch)))
             (act ch
                  :subject-emit "A shower of rubble buries you alive and you die a horrible death!!"
                  :place-emit "A shower of rubble crushes $n!")
             (slog "~a killed in a cave-in at ~d" (name-of tch)
                   (number-of self))

             (let ((rubble (read-object +rubble-vnum+)))
               (when rubble
                 (transfer-items-to-corpse tch nil rubble))
               (obj-to-room rubble self))))
         (loop
            for exit across (dir-option-of self)
            for dir from 0
            when (and exit
                      (to-room-of exit)
                      (real-room (to-room-of exit)))
            do
              (send-to-room (real-room (to-room-of exit))
                            "You hear the sound of a cave-in from ~a!~%"
                            (aref +from-dirs+ dir))))
        ((and (> count 20)
              (randomly-true 5))
         (send-to-room self "Large rocks begin to fall from the ceiling.~%"))
        ((and (> count 10)
              (randomly-true 10))
         (send-to-room self "Small rocks begin to fall from the ceiling.~%"))
        ((and (> count (random-range 0 15))
              (< count 10))
         (send-to-room self "You notice a faint tremor.~%"))
        ((randomly-true 16)
         (send-to-room self "You see the glimmer of gold from deep within the cavern.~%"))
        ((randomly-true 16)
         (send-to-room self "You see some treasure shining deep in the cavern.~%"))
        ((randomly-true 16)
         (send-to-room self "You hear a lady calling for help deeper in the cavern.~%"))
        ((randomly-true 16)
         (send-to-room self "You notice sunlight coming from somewhere deeper in the cavern.~%"))))))
