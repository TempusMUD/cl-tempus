(in-package #:tempus)

(defun abs-exit (room dir)
  (aref (dir-option-of room) dir))
(defun exit (ch dir)
  (abs-exit (in-room-of ch) dir))

(defun first-word (str)
  (let ((space-pos (position #\space str)))
    (if space-pos
        (subseq str 0 space-pos)
        str)))

(defun find-door (ch args action)
  (destructuring-bind (type &optional dir)
      (cl-ppcre:split "\\s+" args)
    (if dir
        (let ((door (position dir +dirs+ :test 'string-abbrev)))
          (cond
            ((null door)
             (send-to-char ch "That's not a direction.~%")
             nil)
            ((null (exit ch door))
             (send-to-char ch "I don't see how you can ~a anything there.~%" action)
             nil)
            ((null (keyword-of (exit ch door)))
             door)
            ((not (string-abbrev type (keyword-of (exit ch door))))
             (send-to-char ch "I see no ~a there.~%" type)
             nil)
            (t
             door)))
        (loop for door from 0 upto (1- +num-of-dirs+) do
             (when (and (exit ch door)
                        (keyword-of (exit ch door))
                        (string-abbrev type (keyword-of (exit ch door))))
                 (return-from find-door door))
             finally (progn
                       (send-to-char ch "I see no ~a there.~%" type)
                       nil)))))

(defun is-carrying-boat (ch)
  (find +item-boat+ (carrying-of ch) :key #'kind-of))

(defun do-simple-move (ch dir mode need-specials-check)
  (declare (ignore need-specials-check))

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

      (decf (duration-of af) (* (str-app-type-to-dam (aref +str-app+ (str-of ch))) 2))
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
          (send-to-char ch "You need a boat to go there.~%")
          (return-from do-simple-move 1))

        (when (= (position-of ch) +pos-flying+)
          (send-to-char ch "You fly over the waters.~%")))

      ;; If this room or the one we're going to needs wings, check for some
      (when (and (room-is-open-air (in-room-of ch))
                 (/= (position-of ch) +pos-flying+)
                 (or (null (mounted-of ch))
                     (/= (position-of (mounted-of ch)) +pos-flying+)))
        (act ch
             :all-emit "$n scramble$% wildly for a grasp of thin air!")
        (return-from do-simple-move 1))
      (when (and (room-is-open-air dest)
                 (not (zone-is-nograv (zone-of (in-room-of ch))))
                 (not (eql mode :jump))
                 (/= (position-of ch) +pos-flying+)
                 (or (not (mounted-of ch))
                     (/= (position-of (mounted-of ch)) +pos-flying+)))
        (send-to-char ch "You need to be flying to go there.~%")
        (when (/= dir +up+)
          (send-to-char ch "You can 'jump' in that direction however...~%"))
        (return-from do-simple-move 1))

      (cond
        ((or (aff-flagged ch +aff-blur+)
             (and (mounted-of ch) (aff-flagged (mounted-of ch) +aff-blur+)))
         (act ch :place-emit (format nil "A blurred, shifted image leaves ~a."
                                     (aref +to-dirs+ dir))))
        ((eql mode :jump)
         (act ch :place-emit (format nil
                                     (if (randomly-true)
                                         "$n jumps ~a."
                                         "$n takes a flying leap ~a.")
                                     (aref +to-dirs+ dir))))
        ((eql mode :flee)
         (act ch :place-emit (format nil "$n flees ~a." (aref +to-dirs+ dir))))
        ((eql mode :retreat)
         (act ch :place-emit (format nil "$n retreats ~a." (aref +to-dirs+ dir))))
        ((eql mode :crawl)
         (act ch :place-emit (format nil "$n crawls slowly ~a." (aref +to-dirs+ dir))))
        ((or (eql (position-of ch)
                  +pos-flying+)
             (room-is-open-air (in-room-of ch))
             (room-is-open-air dest))
         (act ch :place-emit (format nil "$n flies ~a." (aref +to-dirs+ dir))))
        ((eql (terrain-of (in-room-of ch)) +sect-astral+)
         (act ch :place-emit (format nil "$n travels what seems to be ~a."
                                     (aref +to-dirs+ dir))))
        ((and (zerop (random-range 0 3))
              (or (= (terrain-of (in-room-of ch)) +sect-city+)
                  (= (terrain-of (in-room-of ch)) +sect-inside+)
                  (= (terrain-of (in-room-of ch)) +sect-road+)))
         (act ch :place-emit (format nil "$n strolls ~a." (aref +to-dirs+ dir))))
        ((zerop (random-range 0 2))
         (act ch :place-emit (format nil "$n walks ~a." (aref +to-dirs+ dir))))
        ((zerop (random-range 0 2))
         (act ch :place-emit (format nil "$n departs ~a." (aref +to-dirs+ dir))))
        (t
         (act ch :place-emit (format nil "$n leaves ~a." (aref +to-dirs+ dir)))))

      (char-from-room ch t)

      (char-to-room ch dest)

      (cond
        ((or (aff-flagged ch +aff-blur+)
             (and (mounted-of ch) (aff-flagged (mounted-of ch) +aff-blur+)))
         (act ch :place-emit (format nil "A blurred, shifted image arrives from ~a."
                                     (aref +from-dirs+ dir))))
        ((eql mode :jump)
         (act ch :place-emit (format nil
                                     (if (randomly-true)
                                         "$n jumps in from ~a."
                                         "$n leaps in from ~a.")
                                     (aref +from-dirs+ dir))))
        ((eql mode :flee)
         (act ch :place-emit (format nil "$n runs in ~a." (aref +from-dirs+ dir))))
        ((eql mode :drag)
         (act ch :place-emit (format nil "$n is dragged in from ~a." (aref +from-dirs+ dir))))
        ((eql mode :crawl)
         (act ch :place-emit (format nil "$n crawls slowly in from ~a." (aref +from-dirs+ dir))))
        ((or (eql (position-of ch)
                  +pos-flying+)
             (room-is-open-air (in-room-of ch))
             (room-is-open-air dest))
         (act ch :place-emit (format nil "$n flies in from ~a." (aref +from-dirs+ dir))))
        ((eql (terrain-of (in-room-of ch)) +sect-astral+)
         (act ch :place-emit (format nil (if (randomly-true)
                                             "$n arrives from what appears to be ~a."
                                             "$n moves into view from what might be ~a.")
                                     (aref +from-dirs+ dir))))
        ((zerop (random-range 0 3))
         (act ch :place-emit (format nil "$n walks in from ~a." (aref +from-dirs+ dir))))
        ((and (zerop (random-range 0 2))
              (or (= (terrain-of (in-room-of ch)) +sect-city+)
                  (= (terrain-of (in-room-of ch)) +sect-inside+)
                  (= (terrain-of (in-room-of ch)) +sect-road+)))
         (act ch :place-emit (format nil "$n strolls in from ~a." (aref +from-dirs+ dir))))
        (t
         (act ch :place-emit (format nil "$n has arrived from ~a." (aref +from-dirs+ dir)))))
      (when (link-of ch)
        (look-at-room ch (in-room-of ch) nil))
      0)))

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
                     (random-elt
                      '("Alas, you cannot go that way..."
                        "You don't seem to be able to go that way."
                        "Sorry, you can't go in that direction!"
                        "There is no way to move in that direction."
                        "You can't go that way, slick..."
                        "You'll have to choose another direction."))))
      ((and (logtest (exit-info-of exit) +ex-closed+)
            (not (noncorporealp ch))
            (not (immortalp ch)))
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

(defcommand (ch "stand") (:sleeping)
  (cond
    ((or (= (position-of ch) +pos-standing+)
         (= (position-of ch) +pos-fighting+))
     (send-to-char ch "You are already standing.~%"))
    ((= (position-of ch) +pos-mounted+)
     (send-to-char ch "You should dismount first.~%"))
    ((and (aff3-flagged ch +aff3-gravity-well+)
          (/= (position-of ch) +pos-flying+)
          (>= (random-range 1 20) (str-of ch)))
     (act ch :all-emit "The gravity well drive$% $n into the ground as $e attempt$% to stand.")
     (setf (position-of ch) +pos-resting+))
    ((and (aff3-flagged ch +aff3-gravity-well+)
          (/= (position-of ch) +pos-flying+))
     (act ch
          :subject-emit "You defy the probability waves of the gravity well and struggle to your feet."
          :place-emit "$n defies the gravity well and struggles to $s feet.")
     (setf (position-of ch) +pos-standing+))
    ((= (position-of ch) +pos-sleeping+)
     (act ch :all-emit "$n wake$% up, and stagger$% to $s feet.")
     (setf (position-of ch) +pos-standing+))
    ((= (position-of ch) +pos-resting+)
     (act ch :all-emit "$n stop$% resting, and clamber$% onto $s feet.")
     (setf (position-of ch) +pos-standing+))
    ((= (position-of ch) +pos-sitting+)
     (act ch :all-emit "$n clamber$% to $s feet.")
     (setf (position-of ch) +pos-standing+))
    ((/= (position-of ch) +pos-flying+)
     (act ch :all-emit "$n stop$% floating around, and put$% $s feet on the ground.")
     (setf (position-of ch) +pos-standing+))
    ;; creature must be flying at this point
    ((is-race ch +race-gaseous+)
     (send-to-char ch "You don't have legs.~%"))
    ((room-is-open-air (in-room-of ch))
     (send-to-char ch "You can't stand on air, silly!~%"))
    ((and (= (terrain-of (in-room-of ch)) +sect-water-noswim+)
          (or (aff-flagged ch +aff-waterwalk+)
              (is-carrying-boat ch)))
     (send-to-char ch "You can't land here, on the water!~%"))
    (t
     (cond
       ((or (= (terrain-of (in-room-of ch)) +sect-water-swim+)
            (= (terrain-of (in-room-of ch)) +sect-water-noswim+))
        (act ch
             :subject-emit "You settle lightly to the surface."
             :place-emit "$n drifts downward and stands upon the waters."))
       ((= (terrain-of (in-room-of ch)) +sect-swamp+)
        (act ch
             :subject-emit "You settle lightly onto the swampy ground."
             :place-emit "$n drifts downward and stands upon the swampy ground."))
       ((= (terrain-of (in-room-of ch)) +sect-blood+)
        (act ch
             :subject-emit "You settle lightly onto the blood soaked ground."
             :place-emit "$n drifts downward and stands on the blood soaked ground."))
       ((= (terrain-of (in-room-of ch)) +sect-muddy+)
        (act ch
             :subject-emit "You settle lightly into the mud."
             :place-emit "$n drifts downward and stands on the mud."))
       ((= (terrain-of (in-room-of ch)) +sect-desert+)
        (act ch
             :subject-emit "You settle lightly to the sands."
             :place-emit "$n drifts downward and stands upon the sand."))
       ((= (terrain-of (in-room-of ch)) +sect-fire-river+)
        (act ch
             :subject-emit "You settle lightly into the fires."
             :place-emit "$n drifts downward and stands within the fire."))
       (t
        (act ch
             :subject-emit "You settle lightly to the ground."
             :place-emit "$n drifts downward and stands on the ground.")))
     (setf (position-of ch) +pos-standing+))))

(defun perform-fly (ch)
  (cond
    ((and (aff3-flagged ch +aff3-gravity-well+)
          (>= (random-range 1 20) (str-of ch)))
     (act ch :subject-emit "The gravity well holds you fast to the ground!"))
    ((or (= (position-of ch) +pos-standing+)
         (= (position-of ch) +pos-sitting+)
         (= (position-of ch) +pos-resting+))
     (act ch
          :subject-emit "Your feet lift from the ground."
          :place-emit "$n begins to float above the ground.")
     (setf (position-of ch) +pos-flying+))
    ((= (position-of ch) +pos-sleeping+)
     (act ch :subject-emit "You have to wake up first!"))
    ((= (position-of ch) +pos-fighting+)
     (act ch :subject-emit "You can't fly until you beat this fool off of you!"))
    ((= (position-of ch) +pos-flying+)
     (send-to-char ch "You are already in flight.~%"))
    ((= (position-of ch) +pos-mounted+)
     (when (mounted-of ch)
       (act ch :target (mounted-of ch) :all-emit "$n rise$% off of $N.")
       (setf (aff2-flags-of (mounted-of ch)) (logandc2 (aff2-flags-of (mounted-of ch)) +aff2-mounted+)))
     (setf (mounted-of ch) nil)
     (setf (position-of ch) +pos-flying+))
    (t
     (act ch :all-emit "$n stop$% floating around and puts $s feet on the ground.")
     (setf (position-of ch) +pos-standing+))))

(defcommand (ch "fly") (:sleeping)
  (perform-fly ch))

(defcommand (ch "rest") (:sleeping)
  (cond
    ((= (position-of ch) +pos-standing+)
     (act ch
          :subject-emit "You sit down and lay back into a relaxed position."
          :place-emit "$n sits down and rests.")
     (setf (position-of ch) +pos-resting+))
    ((= (position-of ch) +pos-sitting+)
     (act ch
          :subject-emit "You lay back and rest your tired bones."
          :place-emit "$n rests.")
     (setf (position-of ch) +pos-resting+))
    ((= (position-of ch) +pos-resting+)
     (send-to-char ch "You are already resting.~%"))
    ((= (position-of ch) +pos-sleeping+)
     (send-to-char ch "You have to wake up first.~%"))
    ((= (position-of ch) +pos-fighting+)
     (send-to-char ch "Rest while fighting?  Are you MAD?~%"))
    ((= (position-of ch) +pos-flying+)
     (send-to-char ch "You better not try that while flying.~%"))
    ((= (position-of ch) +pos-mounted+)
     (act ch :target (mounted-of ch)
          :subject-emit "You had better get off $N first."))
    (t
     (act ch
          :subject-emit "You stop floating around, and stop to rest your tired bones."
          :place-emit "$n stops floating around and rests.")
     (setf (position-of ch) +pos-resting+))))

(defcommand (ch "sit") (:sleeping)
  (cond
    ((= (position-of ch) +pos-standing+)
     (act ch
          :subject-emit "You sit down."
          :place-emit "$n sits down.")
     (setf (position-of ch) +pos-sitting+))
    ((= (position-of ch) +pos-sitting+)
     (send-to-char ch "You're sitting already.~%"))
    ((= (position-of ch) +pos-resting+)
     (act ch
          :subject-emit "You stop resting and sit up."
          :place-emit "$n stops resting.")
     (setf (position-of ch) +pos-sitting+))
    ((= (position-of ch) +pos-sleeping+)
     (send-to-char ch "You have to wake up first.~%"))
    ((= (position-of ch) +pos-fighting+)
     (send-to-char ch "Sit down while fighting?  Are you MAD?~%"))
    ((= (position-of ch) +pos-flying+)
     (send-to-char ch "That's probably not a good idea while flying.~%"))
    ((= (position-of ch) +pos-mounted+)
     (act ch :target (mounted-of ch)
          :subject-emit "You are already seated on $N."))
    (t
     (act ch
          :subject-emit "You stop floating around, and sit down."
          :place-emit "$n stops floating around and sits down.")
     (setf (position-of ch) +pos-sitting+))))

(defcommand (ch "sleep") ()
  (cond
    ((aff2-flagged ch +aff2-berserk+)
     (send-to-char ch "What, sleep while in a berserk rage??~%"))
    ((aff-flagged ch +aff-adrenaline+)
     (send-to-char ch "You can't seem to relax.~%"))
    ((or (= (position-of ch) +pos-standing+)
         (= (position-of ch) +pos-sitting+)
         (= (position-of ch) +pos-resting+))
     (send-to-char ch "You go to sleep.~%")
     (act ch :place-emit "$n lies down and falls asleep.")
     (setf (position-of ch) +pos-sleeping+))
    ((= (position-of ch) +pos-sleeping+)
     (send-to-char ch "You are already sound asleep.~%"))
    ((= (position-of ch) +pos-fighting+)
     (send-to-char ch "Sleep while fighting?  Are you MAD?~%"))
    ((= (position-of ch) +pos-flying+)
     (send-to-char ch "That's a really bad idea while flying!~%"))
    ((= (position-of ch) +pos-mounted+)
     (act ch :target (mounted-of ch)
          :subject-emit "Better not sleep while mounted on $N."))
    (t
     (send-to-char ch "You stop floating around, and lie down to sleep.~%")
     (setf (position-of ch) +pos-sleeping+))))

(defcommand (ch "wake") ()
  (cond
    ((aff-flagged ch +aff-sleep+)
     (send-to-char ch "You can't wake up!~%"))
    ((> (position-of ch) +pos-sleeping+)
     (send-to-char ch "You are already awake...~%"))
    ((aff3-flagged ch +aff3-stasis+)
     (act ch :subject-emit "Reactivating processes..."
          :place-emit "$n awakens.")
     (setf (aff3-flags-of ch) (logandc2 (aff3-flags-of ch) +aff3-stasis+))
     (wait-state ch (* +pulse-violence+ 3))
     (setf (position-of ch) +pos-sitting+))
    (t
     (act ch :subject-emit "You awaken, and sit up."
          :place-emit "$n awakens.")
     (setf (position-of ch) +pos-sitting+))))

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

(defcommand (ch "move") (:standing)
  (send-to-char ch "In which direction?~%"))
(defcommand (ch "move" dir) (:standing)
  (let ((dirnum (position dir +dirs+ :test 'string-abbrev)))
    (cond
      ((null dirnum)
       (send-to-char ch "'~a' is not a valid direction.~%" dir))
      (t
       (perform-move ch dirnum nil t)))))

(defcommand (ch "crawl") (:standing)
  (send-to-char ch "In which direction?~%"))
(defcommand (ch "crawl" dir) (:standing)
  (let ((dirnum (position dir +dirs+ :test 'string-abbrev)))
    (cond
      ((null dirnum)
       (send-to-char ch "'~a' is not a valid direction.~%" dir))
      (t
       (perform-move ch dirnum :crawl t)))))

(defcommand (ch "jump") (:standing)
  (send-to-char ch "In which direction?~%"))
(defcommand (ch "jump" dir) (:standing)
  (let ((dirnum (position dir +dirs+ :test 'string-abbrev)))
    (cond
      ((null dirnum)
       (send-to-char ch "'~a' is not a valid direction.~%" dir))
      (t
       (perform-move ch dirnum :jump t)))))

(defun obj-is-openable (obj)
  (and (logtest (aref (value-of obj) 1) +cont-closeable+)
          (or (and (is-obj-kind obj +item-container+)
                   (zerop (aref (value-of obj) 3)))
              (and (is-obj-kind obj +item-v-window+)
                   (car-openable obj))
              (is-obj-kind obj +item-vehicle+)
              (is-obj-kind obj +item-portal+)
              (is-obj-kind obj +item-scuba-mask+)
              (is-obj-kind obj +item-window+))))

(defun perform-object-unlock (ch obj)
  (cond
    ((not (obj-is-openable obj))
     (send-to-char ch "You can't unlock that!~%"))
    ((not (logtest (aref (value-of obj) 1) +cont-locked+))
     (send-to-char ch "But it's currently unlocked!~%"))
    (t
     (setf (aref (value-of obj) 1)
           (logandc2 (aref (value-of obj) 1) +cont-locked+))
     (act ch :item obj
          :subject-emit "*Click*"
          :place-emit "$n opens $p.")
     (when (is-vehicle obj)
       (let ((other-room (real-room (room-number obj))))
         (when (people-of other-room)
           (send-to-room other-room "The door of the ~a is unlocked from the outside.~%"
                         (first-word (aliases-of obj))))))
     (wait-state ch 30))))

(defun perform-object-pick (ch obj)
  (let ((lockpick (find-lockpick ch)))
    (cond
      ((not (obj-is-openable obj))
       (send-to-char ch "You can't unlock that!~%"))
      ((not (logtest (aref (value-of obj) 1) +cont-closed+))
       (send-to-char ch "But it's currently open!~%"))
      ((not (logtest (aref (value-of obj) 1) +cont-locked+))
       (send-to-char ch "But it's currently unlocked!~%"))
      ((and (null lockpick)
            (not (immortalp ch)))
       (send-to-char ch "You need to be holding a lockpicking tool, you deviant!~%"))
      ((> (random-range 2 101)
          (+ (if lockpick (obj-val-of lockpick 1) 15)
             (check-skill ch +skill-pick-lock+)))
       (send-to-char ch "You failed to pick the lock.~%")
       (wait-state ch 4))
      (t
       (setf (aref (value-of obj) 1)
             (logandc2 (aref (value-of obj) 1) +cont-locked+))
       (act ch :item obj
            :subject-emit "The lock quickly yields to your skills."
            :place-emit "$n skillfully picks the lock on $p.")
       (when (is-vehicle obj)
         (let ((other-room (real-room (room-number obj))))
           (when (people-of other-room)
             (send-to-room other-room "The door of the ~a is unlocked from the outside.~%"
                           (first-word (aliases-of obj))))))
       (wait-state ch 30)))))

(defun perform-object-open (ch obj)
  (cond
    ((not (obj-is-openable obj))
     (send-to-char ch "You can't open that!~%"))
    ((not (logtest (aref (value-of obj) 1) +cont-closed+))
     (send-to-char ch "But it's currently open!~%"))
    ((logtest (aref (value-of obj) 1) +cont-locked+)
     (send-to-char ch "It seems to be locked.~%"))
    (t
     (setf (aref (value-of obj) 1)
           (logandc2 (aref (value-of obj) 1) +cont-closed+))
     (act ch :item obj
          :subject-emit "Okay, opened."
          :place-emit "$n opens $p.")
     (when (is-vehicle obj)
       (let ((other-room (real-room (room-number obj))))
         (when (people-of other-room)
           (send-to-room other-room "The door of the ~a is opened from the outside.~%"
                         (first-word (aliases-of obj))))))
     (wait-state ch 7))))

(defun perform-object-close (ch obj)
  (cond
    ((not (obj-is-openable obj))
     (send-to-char ch "You can't close that!~%"))
    ((logtest (aref (value-of obj) 1) +cont-closed+)
     (send-to-char ch "But it's currently closed!~%"))
    (t
     (setf (aref (value-of obj) 1)
           (logior (aref (value-of obj) 1) +cont-closed+))
     (act ch :item obj
          :subject-emit "Okay, closed."
          :place-emit "$n closes $p.")
     (when (is-vehicle obj)
       (let ((other-room (real-room (room-number obj))))
         (when (people-of other-room)
           (send-to-room other-room "The door of the ~a is closed from the outside.~%"
                         (first-word (aliases-of obj))))))
     (wait-state ch 7))))

(defun perform-door-open (ch door)
  (let* ((exit (exit ch door))
         (doorname (if (keyword-of exit)
                       (first-word (keyword-of exit))
                       "door")))
    (cond
      ((not (logtest (exit-info-of exit) +ex-isdoor+))
       (send-to-char ch "You can't open that!~%"))
      ((not (logtest (exit-info-of exit) +ex-closed+))
       (send-to-char ch "But it's currently open!~%"))
      ((logtest (exit-info-of exit) +ex-locked+)
       (send-to-char ch "It seems to be locked.~%"))
      ((and (logtest (exit-info-of exit) +ex-heavy-door+)
            (< (move-of ch)
               10))
       (send-to-char ch "You are too exhausted.~%"))
      ((and (logtest (exit-info-of exit) +ex-heavy-door+)
            (< (+ (dice 2 7)
                  (str-app-type-to-dam (aref +str-app+ (str-of ch))))
               12))
       (case (random-range 0 3)
         (0
          (send-to-char ch "The ~a ~a too heavy for you to open!~%"
                        doorname (is-are doorname))
          (act ch :place-emit (format nil "$n attempts to open the ~a" doorname)))
         (1
          (send-to-char ch "You push against the ~a, but ~a barely budge~a.~%"
                        doorname (it-they doorname) (if (pluralp doorname)
                                                        "" "s"))
          (act ch :place-emit (format nil "$n pushes against the ~a, but ~a barely budge~a."
                                      doorname (it-they doorname) (if (pluralp doorname)
                                                                      "" "s"))))
         (2
          (send-to-char ch "You strain against the ~a, to no avail.~%"
                        doorname)
          (act ch :place-emit (format nil "$n strains against the ~a, to no avail." doorname)))
         (t
          (send-to-char ch "You throw yourself against the heavy ~a in an attempt to open the ~a.~%"
                        doorname (it-them doorname))
          (act ch :place-emit (format nil "$n throws $mself against the heavy ~a in an attempt to open ~a."
                                      doorname (it-them doorname)))))
       (wait-state ch +pulse-violence+)
       (decf (move-of ch) 10))
      (t
       (let* ((other-room (real-room (to-room-of exit)))
              (return-exit (abs-exit other-room (aref +rev-dir+ door)))
              (back (and other-room
                         (when (eql (to-room-of return-exit) (number-of (in-room-of ch)))
                           other-room))))
         (setf (exit-info-of exit) (logandc2 (exit-info-of exit) +ex-closed+))
         (when back
           (setf (exit-info-of return-exit) (logandc2 (exit-info-of exit) +ex-closed+))
           (send-to-room other-room "The ~a is opened from the other side.~%" doorname))
         (act ch
              :subject-emit "Okay, opened."
              :place-emit (format nil "$n opens the ~a." doorname))
         (wait-state ch (if (logtest (exit-info-of exit) +ex-heavy-door+)
                            30 15)))))))

(defun perform-door-close (ch door)
  (let* ((exit (exit ch door))
         (doorname (if (keyword-of exit)
                       (first-word (keyword-of exit))
                       "door")))
    (cond
      ((not (logtest (exit-info-of exit) +ex-isdoor+))
       (send-to-char ch "You can't close that!~%"))
      ((logtest (exit-info-of exit) +ex-closed+)
       (send-to-char ch "But it's currently closed!~%"))
      ((and (logtest (exit-info-of exit) +ex-heavy-door+)
            (< (move-of ch)
               10))
       (send-to-char ch "You are too exhausted.~%"))
      ((and (logtest (exit-info-of exit) +ex-heavy-door+)
            (< (+ (dice 2 7)
                  (str-app-type-to-dam (aref +str-app+ (str-of ch))))
               12))
       (case (random-range 0 3)
         (0
          (send-to-char ch "The ~a ~a too heavy for you to close!~%"
                        doorname (is-are doorname))
          (act ch :place-emit (format nil "$n attempts to close the ~a" doorname)))
         (1
          (send-to-char ch "You push against the ~a, but ~a barely budge~a.~%"
                        doorname (it-they doorname) (if (pluralp doorname)
                                                        "" "s"))
          (act ch :place-emit (format nil "$n pushes against the ~a, but ~a barely budge~a."
                                      doorname (it-they doorname) (if (pluralp doorname)
                                                                      "" "s"))))
         (2
          (send-to-char ch "You strain against the ~a, to no avail.~%"
                        doorname)
          (act ch :place-emit (format nil "$n strains against the ~a, to no avail." doorname)))
         (t
          (send-to-char ch "You throw yourself against the heavy ~a in an attempt to close the ~a.~%"
                        doorname (it-them doorname))
          (act ch :place-emit (format nil "$n throws $mself against the heavy ~a in an attempt to close ~a."
                                      doorname (it-them doorname)))))
       (wait-state ch +pulse-violence+)
       (decf (move-of ch) 10))
      (t
       (let* ((other-room (real-room (to-room-of exit)))
              (return-exit (abs-exit other-room (aref +rev-dir+ door)))
              (back (and other-room
                         (when (eql (to-room-of return-exit) (number-of (in-room-of ch)))
                           other-room))))
         (setf (exit-info-of exit) (logior (exit-info-of exit) +ex-closed+))
         (when back
           (setf (exit-info-of return-exit) (logior (exit-info-of exit) +ex-closed+))
           (send-to-room other-room "The ~a is closed from the other side.~%" doorname))
         (act ch
              :subject-emit "Okay, closed."
              :place-emit (format nil "$n closes the ~a." doorname))
         (wait-state ch (if (logtest (exit-info-of exit) +ex-heavy-door+)
                            30 15)))))))

(defun find-key (ch key-vnum)
  (flet ((unbroken-match (obj)
                 (if (and (= (vnum-of obj) key-vnum)
                      (not (is-obj-stat2 obj +item2-broken+)))
                     obj
                     nil)))
    (or (find-if #'unbroken-match (carrying-of ch))
        (and (get-eq ch +wear-hold+)
             (unbroken-match (get-eq ch +wear-hold+))))))

(defun find-lockpick (ch)
  (flet ((unbroken-match (obj)
                 (if (and (is-obj-kind obj +item-tool+)
                          (= (obj-val-of obj 0) +skill-pick-lock+)
                          (not (is-obj-stat2 obj +item2-broken+)))
                     obj
                     nil)))
    (or (find-if #'unbroken-match (carrying-of ch))
        (and (get-eq ch +wear-hold+)
             (unbroken-match (get-eq ch +wear-hold+))))))

(defun perform-door-unlock (ch door)
  (let* ((exit (exit ch door))
         (doorname (if (keyword-of exit)
                       (first-word (keyword-of exit))
                       "door")))
    (cond
      ((not (logtest (exit-info-of exit) +ex-isdoor+))
       (send-to-char ch "You can't unlock that!~%"))
      ((not (logtest (exit-info-of exit) +ex-closed+))
       (send-to-char ch "But it's currently open!"))
      ((not (logtest (exit-info-of exit) +ex-locked+))
       (send-to-char ch "Oh.. it wasn't locked, after all...~%"))
      ((and (not (find-key ch (key-of exit)))
            (not (immortalp ch)))
       (send-to-char ch "You don't seem to have the proper key.~%"))
      (t
       (let* ((other-room (real-room (to-room-of exit)))
              (return-exit (abs-exit other-room (aref +rev-dir+ door)))
              (back (and other-room
                         (when (eql (to-room-of return-exit) (number-of (in-room-of ch)))
                           other-room))))
         (setf (exit-info-of exit) (logandc2 (exit-info-of exit) +ex-locked+))
         (when back
           (setf (exit-info-of return-exit) (logandc2 (exit-info-of exit) +ex-locked+))
           (send-to-room other-room "You hear the ~a being unlocked from the other side.~%" doorname))
         (act ch
              :subject-emit "*Click*"
              :place-emit (format nil "$n unlocks the ~a." doorname))
         (wait-state ch 30))))))

(defun perform-door-pick (ch door)
  (let* ((exit (exit ch door))
         (doorname (if (keyword-of exit)
                       (first-word (keyword-of exit))
                       "door"))
         (lockpick (find-lockpick ch)))
    (cond
      ((not (plusp (check-skill ch +skill-pick-lock+)))
       (send-to-char ch "You don't know how.~%"))
      ((not (logtest (exit-info-of exit) +ex-isdoor+))
       (send-to-char ch "You can't pick that!~%"))
      ((logtest (exit-info-of exit) +ex-tech+)
       (send-to-char ch "This exit is not pickable to traditional means.~%"))
      ((zerop (key-of exit))
       (send-to-char ch "Odd - you can't seem to find a keyhole.~%"))
      ((logtest (exit-info-of exit) +ex-pickproof+)
       (send-to-char ch "It resists your attempts at picking it.~%"))
      ((not (logtest (exit-info-of exit) +ex-closed+))
       (send-to-char ch "But it's currently open!"))
      ((not (logtest (exit-info-of exit) +ex-locked+))
       (send-to-char ch "Oh.. it wasn't locked, after all...~%"))
      ((and (null lockpick)
            (not (immortalp ch)))
       (send-to-char ch "You need to be holding a lockpicking tool, you deviant!~%"))
      ((> (random-range 2 101)
          (+ (if lockpick (obj-val-of lockpick 1) 15)
             (check-skill ch +skill-pick-lock+)))
       (send-to-char ch "You failed to pick the lock.~%")
       (wait-state ch 4))
      (t
       (let* ((other-room (real-room (to-room-of exit)))
              (return-exit (abs-exit other-room (aref +rev-dir+ door)))
              (back (and other-room
                         (when (eql (to-room-of return-exit) (number-of (in-room-of ch)))
                           other-room))))
         (setf (exit-info-of exit) (logandc2 (exit-info-of exit) +ex-locked+))
         (when back
           (setf (exit-info-of return-exit) (logandc2 (exit-info-of exit) +ex-locked+))
           (send-to-room other-room "You hear the ~a being unlocked from the other side.~%" doorname))
         (act ch
              :subject-emit "The lock quickly yields to your skills.~%"
              :place-emit (format nil "$n skillfully picks the lock on ~a." doorname))
         (wait-state ch 30))))))

(defcommand (ch "open") (:standing)
  (send-to-char ch "Open what?~%"))

(defcommand (ch "open" target) (:standing)
  (let ((objs (resolve-alias ch target
                             (append (carrying-of ch)
                                     (contents-of (in-room-of ch))))))
    (cond
      ((cdr objs)
       (send-to-char ch "You can only open one thing at a time.~%"))
      (objs
       (perform-object-open ch (first objs)))
      (t
       (let ((door (find-door ch target "open")))
         (when door
           (perform-door-open ch door)))))))

(defcommand (ch "close") (:standing)
  (send-to-char ch "Close what?~%"))

(defcommand (ch "close" target) (:standing)
  (let ((objs (resolve-alias ch target
                             (append (carrying-of ch)
                                     (contents-of (in-room-of ch))))))
    (cond
      ((cdr objs)
       (send-to-char ch "You can only close one thing at a time.~%"))
      (objs
       (perform-object-close ch (first objs)))
      (t
       (let ((door (find-door ch target "open")))
         (when door
           (perform-door-close ch door)))))))

(defcommand (ch "unlock") (:standing)
  (send-to-char ch "Unlock what?~%"))

(defcommand (ch "unlock" target) (:standing)
  (let ((objs (resolve-alias ch target
                             (append (carrying-of ch)
                                     (contents-of (in-room-of ch))))))
    (cond
      ((cdr objs)
       (send-to-char ch "You can only unlock one thing at a time.~%"))
      (objs
       (perform-object-unlock ch (first objs)))
      (t
       (let ((door (find-door ch target "unlock")))
         (when door
           (perform-door-unlock ch door)))))))

(defcommand (ch "pick") (:standing)
  (send-to-char ch "Pick what?~%"))

(defcommand (ch "pick" target) (:standing)
  (let ((objs (resolve-alias ch target
                             (append (carrying-of ch)
                                     (contents-of (in-room-of ch))))))
    (cond
      ((cdr objs)
       (send-to-char ch "You can only pick one thing at a time.~%"))
      (objs
       (perform-object-pick ch (first objs)))
      (t
       (let ((door (find-door ch target "pick")))
         (when door
           (perform-door-pick ch door)))))))

(defun perform-door-bash (ch door)
  (let* ((exit (exit ch door))
         (door-name (if (keyword-of exit)
                        (first-word (keyword-of exit))
                        "door")))
    (cond
      ((not (logtest (exit-info-of exit) +ex-isdoor+))
       (send-to-char ch "You can't bash that!~%"))
      ((not (logtest (exit-info-of exit) +ex-closed+))
       (send-to-char ch "It's already open!~%"))
      ((< (move-of ch) 20)
       (send-to-char ch "You are too exhausted.~%"))
      (t
       (decf (move-of ch) 20)
       (let ((door-damage (min (damage-of exit)
                               (if (or (logtest (exit-info-of exit) +ex-pickproof+)
                                       (minusp (damage-of exit))
                                       (null (real-room (to-room-of exit))))
                                   0
                                   (* (dice 2 (floor (level-of ch) 4))
                                      (if (< (check-skill ch +skill-break-door+)
                                             (random-range 1 99))
                                          2
                                          1))))))
         (decf (damage-of exit) door-damage)
         (when (pref-flagged ch +pref-debug+)
           (send-to-char ch "&c[BASH] door-damage: ~d, durability left: ~d&n~%"
                         door-damage
                         (damage-of exit)))
         (damage-creature ch ch (dice 4 8) nil +type-undefined+ nil)

         (cond
           ((plusp (damage-of exit))
            (if (is-dead ch)
                (act ch
                     :subject-emit (format nil "You kill yourself as you hurl yourself against the ~a!"
                                           door-name)
                     :place-emit (format nil "$n throws $mself against the ~a, $s last futile effort."
                                         door-name))
                (act ch
                     :subject-emit (format nil "You slam yourself against the ~a!"
                                           door-name)
                     :place-emit (format nil "$n throws $mself against the ~a in an attempt to break it."
                                         door-name)))
            (send-to-room (in-room-of ch)
                          "The ~a looks ~a.~%"
                          door-name
                          (let ((percent (/ (damage-of exit)
                                            (maxdam-of exit))))
                            (cond
                              ((> percent 99/100)
                               "untouched")
                              ((> percent 90/100)
                               "virtually unharmed")
                              ((> percent 75/100)
                               "pretty scratched up")
                              ((> percent 50/100)
                               "in poor shape")
                              ((> percent 25/100)
                               "completely battered")
                              (t
                               "on the verge of breaking"))))

            )
           (t
            ;; success
            (act ch
                 :subject-emit (format nil "The ~a gives way under your powerful bash!" door-name)
                 :place-emit (format nil "$n bashes the ~a open with a powerful blow!" door-name))
            (open-door (in-room-of ch) exit)
            (unlock-door (in-room-of ch) exit)
            (when (and (not (is-dead ch))
                       (> (random-range 0 20) (dex-of ch)))
              (act ch :all-emit "$n stagger$% and fall$% down.")
              (setf (position-of ch) +pos-sitting+)))))))))
