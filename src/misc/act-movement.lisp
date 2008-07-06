(in-package #:tempus)

(defun exit (ch dir)
  (aref (dir-option-of (in-room-of ch)) dir))

(defun first-word (str)
  (subseq str 0 (min (length str) (position #\space str))))

(defun is-carrying-boat (ch)
  (find +item-boat+ (carrying-of ch) :key #'kind-of))

(defun do-simple-move (ch dir mode need-specials-check)
  (declare (ignore mode need-specials-check))

  (when (plr-flagged ch +plr-afk+)
    (send-to-char ch "You are no longer afk.~%")
    (setf (plr-bits-of ch) (logandc2 (plr-bits-of ch) +plr-afk+))
    (setf (afk-reason-of ch) nil))

  (when (and (aff-flagged ch +aff-charm+)
             (master-of ch)
             (eql (in-room-of ch) (in-room-of (master-of ch))))
    (send-to-char ch "The thought of leaving your master makes you weep.~%")
    (if (is-undead ch)
        (act ch :place-emit "$n makes a hollow moaning sound.")
        (act ch :place-emit "$n looks like $e wants to go somewhere."))
    (return-from do-simple-move 1))

  (let ((af (affected-by-spell ch +spell-entangle+)))
    (when af
      (when (zerop (move-of ch))
        (send-to-char ch "You are too exhausted to break free from the entangling vegetation.~%")
        (return-from do-simple-move 1))

      (if (= (terrain-of (in-room-of ch)) +sect-city+)
          (act ch :all-emit "$n struggle$% against the entangling vegetation!")
          (act ch :all-emit "$n struggle$% against the entangling vines!"))

      (setf (move-of ch) (max 0 (- (move-of 10))))
      (wait-state ch (rl-sec 2))

      (decf (duration-of af) (* (str-app-type-to-dam (aref +str-app+ (strength-apply-index ch))) 2))
      (unless (plusp (duration-of af))
        (act ch :subject-emit "You break free!"
             :place-emit "$n breaks free from $s entanglement!")
        (affect-remove ch af))
      (return-from do-simple-move 1))

    (let ((dest (real-room (to-room-of (exit ch dir)))))
      (when (and (room-flagged dest +room-house+)
                 (not (can-enter-house ch (number-of dest))))
        (send-to-char ch "That's private property -- no trespassing!~%")
        (return-from do-simple-move 1))

#+nil      (when (and (room-flagged dest +room-godroom+)
                 (not (is-member ch "WizardFull")))
        (send-to-char ch "You cannot set foot in that ultracosmic place.~%")
        (return-from do-simple-move 1))

      (when (and (<= +lvl-ambassador+ (level-of ch) +lvl-spirit+)
                 (room-flagged dest +room-death+)
                 (zone-approvedp (zone-of dest)))
        (send-to-char ch "You are repelled by an invisible order-shield.~%")
        (return-from do-simple-move 1))

      ;; If this room or the one we're going to needs a boat, check for one
      (when (and (= (terrain-of dest) +sect-water-noswim+)
                 (/= (terrain-of (in-room-of ch)) +sect-underwater+))
        (unless (or (= (position-of ch) +pos-flying+)
                    (is-fish ch)
                    (aff-flagged ch +aff-waterwalk+)
                    (is-carrying-boat ch))
          (send-to-char ch "You need a boat to go there.~%"))
        
        (when (= (position-of ch) +pos-flying+)        
          (send-to-char ch "You fly over the waters.~%")))

      (cond
        ((and (zerop (random-range 0 3))
              (or (= (terrain-of (in-room-of ch)) +sect-city+)
                  (= (terrain-of (in-room-of ch)) +sect-inside+)
                  (= (terrain-of (in-room-of ch)) +sect-road+)))
         (act ch :place-emit (format nil "$n strolls ~a." (aref +to-dirs+ dir))))
        ((zerop (random-range 0 2))
         (act ch :place-emit (format nil "$n walks ~a." (aref +to-dirs+ dir))))
        ((zerop (random-range 0 2))
         (act ch :place-emit (format nil "$n departs ~award." (aref +dirs+ dir))))
        (t
         (act ch :place-emit (format nil "$n leaves ~a." (aref +to-dirs+ dir)))))
       
      (char-from-room ch)

      (char-to-room ch dest)

      (cond
        ((zerop (random-range 0 3))
         (act ch :place-emit (format nil "$n walks in from ~a." (aref +to-dirs+ dir))))
        ((and (zerop (random-range 0 2))
              (or (= (terrain-of (in-room-of ch)) +sect-city+)
                  (= (terrain-of (in-room-of ch)) +sect-inside+)
                  (= (terrain-of (in-room-of ch)) +sect-road+)))
         (act ch :place-emit (format nil "$n strolls in from ~a." (aref +to-dirs+ dir))))
        (t
         (act ch :place-emit (format nil "$n has arrived from ~a." (aref +dirs+ dir)))))
      (when (link-of ch)
        (look-at-room ch (in-room-of ch) nil)))))

(defun perform-move (ch dir mode need-specials-check)
  (when (or (null ch)
            (null (in-room-of ch))
            (minusp dir)
            (>= dir +num-of-dirs+))
    (return-from perform-move))

  (let ((exit (exit ch dir)))
    (cond
      ((or (null exit)
           (zerop (to-room-of exit))
           (null (real-room (to-room-of exit)))
           (logtest (exit-info-of exit) +ex-nopass+)
           (and (logtest (exit-info-of exit) (logior +ex-secret+ +ex-hidden+))
                (logtest (exit-info-of exit) +ex-closed+)
                (not (immortalp ch))
                (not (noncorporealp ch))))
       (send-to-char ch "~a~%"
                     (case (random-range 0 5)
                       (0 "Alas, you cannot go that way...")
                       (1 "You don't seem to be able to go that way.")
                       (2 "Sorry, you can't go in that direction!")
                       (3 "There is no way to move in that direction.")
                       (4 "You can't go that way, slick...")
                       (t "You'll have to choose another direction."))))
      ((and (logtest (exit-info-of exit) +ex-closed+)
            (not (noncorporealp ch))
            (immortalp ch))
       (if (keyword-of exit)
           (let ((exit-name (first-word (keyword-of exit))))
             (send-to-char ch "The ~a seem~a to be closed.~%"
                           exit-name
                           (if (and (not (string= exit-name "porticullis"))
                                    (char= (char exit-name (1- (length exit-name)))
                                           #\s))
                               "" "s")))
           (send-to-char ch "It seems to be closed.~%")))
      (t
       (do-simple-move ch dir mode need-specials-check)))))

(defcommand (ch "north") (:direction :standing)
  (perform-move ch 0 nil t))
(defcommand (ch "east") (:direction :standing)
  (perform-move ch 1 nil t))
(defcommand (ch "south") (:direction :standing)
  (perform-move ch 2 nil t))
(defcommand (ch "west") (:direction :standing)
  (perform-move ch 3 nil t))
(defcommand (ch "up") (:direction :standing)
  (perform-move ch 4 nil t))
(defcommand (ch "down") (:direction :standing)
  (perform-move ch 5 nil t))
(defcommand (ch "future") (:direction :standing)
  (perform-move ch 6 nil t))
(defcommand (ch "past") (:direction :standing)
  (perform-move ch 7 nil t))
