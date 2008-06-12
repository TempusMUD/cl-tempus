(in-package #:tempus)

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

(defun char-from-room (ch)
  (setf (people-of (in-room-of ch)) (delete ch (people-of (in-room-of ch))))
  (setf (in-room-of ch) nil))

(defun printbits (bits descriptions)
  (format nil "~{~a~^ ~}"
          (loop for idx from 0
             for descrip across descriptions
             when (logtest bits (ash 1 idx))
             collect descrip)))