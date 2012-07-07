(in-package #:tempus)

(defparameter +flow-none+ 0)
(defparameter +flow-wind+ 1)
(defparameter +flow-falling+ 2)
(defparameter +flow-river-surface+ 3)
(defparameter +flow-water+ 4)
(defparameter +flow-underwater+ 5)
(defparameter +flow-conduit+ 6)
(defparameter +flow-conveyor+ 7)
(defparameter +flow-lava+ 8)
(defparameter +flow-river-fire+ 9)
(defparameter +flow-volcanic+ 10)
(defparameter +flow-rotating+ 11)
(defparameter +flow-escalator+ 12)
(defparameter +flow-sinking-swamp+ 13)
(defparameter +flow-unseen-force+ 14)
(defparameter +flow-elemental-wind+ 15)
(defparameter +flow-quicksand+ 16)
(defparameter +flow-crowds+ 17)

(defparameter +flow-kinds+
  #(("None"
     "$n flows ~a."
     "$n flows in from ~a."
     "You flow ~a."
     "$p flows ~a."
     "$p flows in from ~a.")
    ("Wind"
     "$n is blown ~a by the wind."
     "$n blows in from ~a."
     "The wind blows you ~a."
     "$p is blown ~a by the wind."
     "$p blows in from ~a.")
    ("Falling"
     "$n falls ~a."
     "$n falls in from ~a."
     "You fall ~a."
     "$p falls ~a."
     "$p falls in from ~a.")
    ("River"
     "The current pulls $n ~a."
     "$n flows in from ~a on the current."
     "The current pulls you ~a."
     "The current pulls $p ~a."
     "$p flows in from ~a on the current.")
    ("Water Vortex"
     "$n is sucked ~a by the current!"
     "$n is sucked in from ~a by the current."
     "You are sucked ~a by the current!"
     "$p is sucked ~a by the current!"
     "$p is sucked in from ~a by the current.")
    ("Underwater"
     "The current pulls $n ~a."
     "$n flows in from ~a on the current."
     "The current pulls you ~a."
     "The current pulls $p ~a."
     "$p flows in from ~a on the current.")
    ("Conduit"
     "$n spirals off ~a through the conduit."
     "$n spirals in from ~a through the conduit."
     "You spiral ~a through the conduit."
     "$p spirals off ~a through the conduit."
     "$p spirals in from ~a through the conduit.")
    ("Conveyor"
     "$n moves off ~a on the conveyor."
     "$n moves in from ~a on the conveyor."
     "The conveyor pulls you ~a."
     "$p moves off ~a on the conveyor."
     "$p moves in from ~a on the conveyor.")
    ("Lava Flow"
     "$n is dragged ~a by the lava flow."
     "$n moves in from ~a with the lava flow."
     "The lava flow drags you relentlessly ~a."
     "$p flows ~a with the lava."
     "$p moves in from ~a with the lava flow.")
    ("River of Fire"
     "The fiery current pulls $n ~a."
     "$n flows in from ~a on the fiery current."
     "The fiery current pulls you ~a."
     "The fiery current pulls $p ~a."
     "$p flows in from ~a on the fiery current.")
    ("Volcanic Updraft"
     "$n is blown ~a by the hot updraft."
     "$n is blown in from ~a by the hot updraft."
     "The hot updraft blows you ~awards."
     "$p is blown ~a by the hot updraft."
     "$p is blown in from ~a by the hot updraft.")
    ("Rotating Disc"
     "The rotating disc takes $n ~award."
     "The rotating disc brings $n in from ~a."
     "The rotating disc takes you ~award."
     "The rotating disc takes $p ~award."
     "The rotating disc brings $p in from ~a.")
    ("Escalator"
     "$n moves ~a along the escalator."
     "$n comes into view from ~a riding the escalator."
     "You ride the escalator ~a."
     "$p moves ~a with the motion of the escalator."
     "$p is brought in from ~a on the escalator.")
    ("Sinking_Swamp"
     "$n is dragged ~a into the swamp."
     "$n is dragged in from ~a."
     "You sink suddenly into the swamp!"
     "$p is dragged ~a into the swamp."
     "$p is dragged in from ~a.")
    ("Unseen_Force"
     "$n is dragged ~a by an unseen force."
     "$n is dragged in from ~a."
     "You are dragged ~a by an unseen force!!"
     "$p is dragged ~a by an unseen force."
     "$p is dragged in from ~a by an unseen force.")
    ("Elemental_Wind"
     "$n is forced ~award by the strong elemental winds."
     "$n is forced in from ~a by the strong elemental winds."
     "The strong elemental winds force you ~a."
     "$p is pushed ~award by the strong elemental winds."
     "$p is pushed in from ~a by the strong elemental winds.")
    ("Quicksand"
     "$n struggles frantically as they $e is pulled ~a by the quicksand!"
     "The quicksand pulls $n in from the ~a as $e struggles to break free!"
     "You struggle frantically as the quicksand pulls you ~a!"
     "$p is pulled ~a by the quicksand."
     "$p is pulled in from ~a by the quicksand.")
    ("Crowds"
     "$n is beaten ~award by the bloodthirsty mob!"
     "A wild mob of spectators shoves $n in from ~a"
     "Your reluctance is overcome as the bloodthirsty mob shoves you ~a!"
     "Someone in the crowd picks up $p and pitches it ~a."
     "$p is thrown in from ~a by the angry mob.")))

(defun update-room-affects (room)
  (dolist (aff (copy-list (affects-of room)))
    (cond
      ((plusp (duration-of aff))
       (decf (duration-of aff)))
      (t
       (affect-from-room room aff)))))

(defun update-room-ambience (room)
  (when (logtest (flags-of (zone-of room)) +zone-evil-ambience+)
    (dolist (ch (people-of room))
      (when (and (is-pc ch)
                 (not (is-dead ch))
                 (> (alignment-of ch) -1000)
                 (not (affected-by-spell ch +spell-shield-of-righteousness+)))
        (decf (alignment-of ch))
        (check-eq-align ch))))
  (when (logtest (flags-of (zone-of room)) +zone-good-ambience+)
    (dolist (ch (people-of room))
      (when (and (is-pc ch)
                 (not (is-dead ch))
                 (< (alignment-of ch) 1000)
                 (not (affected-by-spell ch +spell-sphere-of-desecration+)))
        (incf (alignment-of ch))
        (check-eq-align ch)))))

(defmethod affected-by-flow ((ch creature) room dir)
  (let ((exit (abs-exit room dir)))
    (not (or (is-dead ch)
             (and (is-npc ch)
                  (or (mob-flagged ch +mob2-no-flow+)
                      (logtest (exit-info-of exit) +ex-nomob+)
                      (room-flagged (real-room (to-room-of exit)) +room-nomob+)))
             (plr-flagged ch +plr-halt+)
             (and (is-pc ch)
                  (null (link-of ch)))
             (and (not (immortal-level-p ch))
                  (room-flagged (real-room (to-room-of exit)) +room-godroom+))
             (and (eql (position-of ch) +pos-flying+)
                  (or (member (flow-kind-of room)
                              (list +flow-river-surface+
                                    +flow-lava+
                                    +flow-river-fire+
                                    +flow-falling+
                                    +flow-sinking-swamp+))
                      (is-dragon ch)))
             (and (aff-flagged ch +aff-waterwalk+)
                  (eql (flow-kind-of room)
                       +flow-sinking-swamp+))
             (not (can-enter-room ch (to-room-of exit)))))))

(defmethod affected-by-flow ((obj obj-data) room dir)
  (or (and (not (can-wear obj +item-wear-take+))
           (/= (vnum-of obj) +blood-vnum+)
           (/= (vnum-of obj) +ice-vnum+))
      (and (> (weight-of obj)
              (random-range 5 (* (flow-speed-of room) 10)))
           (zerop (random-range 0 (flow-speed-of room))))))

(defun flow-one-creature (ch room speed dir)
  (when (affected-by-flow ch room dir)
    (incf (flow-pulse-of ch))
    (when (>= (flow-pulse-of ch) speed)
      (setf (flow-pulse-of ch) 0)
      (let ((flow (aref +flow-kinds+ (flow-kind-of room))))
        (act ch
             :subject-emit (format nil (fourth flow) (aref +to-dirs+ dir))
             :place-emit (format nil (second flow) (aref +to-dirs+ dir)))
        (char-from-room ch t)
        (char-to-room ch (real-room (to-room-of (abs-exit room dir))) t)
        (look-at-room ch room nil)
        (act ch :place-emit (format nil (third flow) (aref +from-dirs+ dir)))))))

(defun flow-one-object (obj room speed dir)
  (when (affected-by-flow obj room dir)
    (incf (flow-pulse-of obj))
    (when (>= (flow-pulse-of obj) speed)
      (setf (flow-pulse-of obj) 0)
      (let  ((flow (aref +flow-kinds+ (flow-kind-of room))))
        (act nil :item obj
             :place-emit (format nil (fifth flow) (aref +to-dirs+ dir)))
        (obj-from-room obj)
        (obj-to-room obj (real-room (to-room-of (abs-exit room dir))))
        (act nil :item obj
             :place-emit (format nil (sixth flow) (aref +to-dirs+ dir)))))))

(defun update-room-flows (room)
  (let ((exit (abs-exit room (flow-dir-of room))))
    (when (and (plusp (flow-speed-of room))
               (not (null exit))
               (not (logtest (exit-info-of exit) +ex-closed+))
               (not (logtest (exit-info-of exit) +ex-nopass+))
               (not (null (real-room (to-room-of exit)))))
      (dolist (ch (copy-list (people-of room)))
        (unless (is-dead ch)
          (flow-one-creature ch room (flow-speed-of room) (flow-dir-of room))))
      (dolist (obj (copy-list (contents-of room)))
        (flow-one-object obj room (flow-speed-of room) (flow-dir-of room))))))

(defun update-rooms (update-func)
  (maphash (lambda (num room)
             (declare (ignore num))
             (unless (or (zone-flagged (zone-of room) +zone-frozen+)
                         (>= (idle-time-of (zone-of room)) +zone-idle-time+))
               (with-simple-restart (continue "Continue from signal in room update")
                 (if *production-mode*
                     (handler-bind ((error (lambda (str)
                                             (errlog "System error: ~a" str)
                                             (continue))))
                       (funcall update-func room))
                     (funcall update-func room)))))
           *rooms*))
