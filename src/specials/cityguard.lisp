(in-package #:tempus)

;; Cityguards have the following functions:
;; - stop all fights not involving a cityguard
;; - they respond to calls for help by going to the room the call originated
;; - when fighting, they call other guards
;; - when they detect another cityguard in combat, they stun the attacker
;; - when they detect a stunned criminal, they haul the person to jail
;; - when idle, they do various amusing things
;; - when the guard dies, specified mobs are loaded in the hq ready to move
;;   to the death point

(defparameter +jail-locker-vnum+ 3178)
(defvar *chars-under-arrest* nil
  "List of player IDs who are current under arrest by cityguards")

(defun char-is-arrested? (ch)
  (and (is-pc ch)
       (find (idnum-of ch) *chars-under-arrest*)))

(defun char-under-arrest (ch)
  (unless (or (is-npc ch)
              (char-is-arrested? ch))
    (push (idnum-of ch) *chars-under-arrest*)))

(defun char-arrest-pardoned (ch)
  (unless (is-npc ch)
    (setf *chars-under-arrest* (delete (idnum-of ch)
                                       *chars-under-arrest*))))

(defun fighting-cityguard? (ch)
  (find-if (lambda (tch)
             (and (is-npc tch)
                  (eql (func-of (shared-of tch)) 'special-cityguard)))
           (fighting-of ch)))

(defun summon-cityguards (room)
  (dolist (tch *characters*)
    (when (and
           (in-room-of tch)
           (not (eql (zone-of room)
                     (zone-of (in-room-of tch))))
           (eql (in-room-of tch) room)
           (not (eql (func-of (shared-of tch)) 'special-cityguard))
           (awakep tch)
           (randomly-true (* (find-distance (in-room-of tch) room) 2)))
      (setf (func-data-of tch) (number-of room)))))

(defun call-for-help (ch attacker)
  (let ((msg (if (eql (func-of (shared-of ch)) 'special-cityguard)
                 (case (random-range 0 10)
                   (0 "To arms!  To arms!  We are under attack!")
                   (1 "Protect the city!")
                   (2 (cond
                        ((is-good ch) "By Alron's will, we shall defend our fair city!")
                        ((is-evil ch) "By Veloth, this city is ours by right!")
                        (t "You shall not prevail!")))
                   (3 (format nil"Death to ~a!" (get-disguised-name ch attacker)))
                   (t "Aid me my comrades!"))
                 (case (random-range 0 10)
                   (0 (format nil "Help!  Help! ~a is trying to kill me!" (get-disguised-name ch attacker)))
                   (1 "I'm being attacked!  Save me!")
                   (2 "Help me!  I'm being attacked!")
                   (t "Will no one help me?!")))))
    (do-gen-comm ch "shout" msg))
  (summon-cityguards (in-room-of ch)))

(defun breakup-fight (ch vict1 vict2)
  (dolist (tch (people-of (in-room-of ch)))
    (cond
      ((eql tch ch)
       (send-to-char ch "You break up the fight between ~a and ~a.~%"
                     (desc vict1 tch)
                     (desc vict2 tch)))
      ((eql tch vict1)
       (send-to-char ch "~a gets between you and ~a, ending the fight!~%"
                     (desc ch tch)
                     (desc vict2 tch)))
      ((eql tch vict2)
       (send-to-char ch "~a gets between you and ~a, ending the fight!~%"
                     (desc ch tch)
                     (desc vict1 tch)))
      (t
       (send-to-char ch "~a gets between ~a and ~a, ending the fight!~%"
                     (desc ch tch)
                     (desc vict1 tch)
                     (desc vict2 tch)))))
  (remove-combat vict1 vict2)
  (remove-combat vict2 vict1))

(defun throw-char-in-jail (ch vict)
  (let* ((cell (random-elt
                (remove-if
                 'people-of
                 (mapcar
                  'real-room
                  '(10908 10910 10911 10921 10920 10919)))))
         (locker-room (real-room (1+ (number-of (in-room-of ch)))))
         (locker (if locker-room (read-object +jail-locker-vnum+) nil)))
    (when (and cell locker-room locker)
      (obj-to-room locker locker-room)
      (map nil (lambda (obj)
                 (when obj
                   (if (or (unrentable? obj)
                           (is-npc vict))
                       (extract-obj obj)
                       (obj-to-obj (unequip-char vict (worn-on-of obj) :worn nil) locker))))
           (equipment-of vict))
      (map nil (lambda (obj)
                 (when obj
                   (obj-from-char obj)
                   (if (or (unrentable? obj)
                           (is-npc vict))
                       (extract-obj obj)
                       (obj-to-obj obj locker))))
           (carrying-of vict))
      (setf (obj-val-of locker 0)
            (idnum-of vict))
      (cond
        ((contains-of locker)
         (act ch :target vict
              :target-emit "$n removes all your gear and stores it in a strongbox.")
         (let ((house (find-house-by-room (number-of (in-room-of locker)))))
           (when house
             (save-house house))))
        (t
         (extract-obj locker)))

      (act ch :target vict
           :target-emit "$n throws you into a cell and slams the door behind you!"
           :not-target-emit "$n throws $N into a cell and slams the door!")
      (char-from-room vict nil)
      (char-to-room vict cell nil)
      (act vict
           :subject-emit "You wake up in jail, your head pounding."
           :place-emit "$n is thrown into the cell, and the door slams shut behind $m!")
      (affect-from-char vict +spell-sleep+)
      (affect-from-char vict +spell-melatonic-flood+)
      (affect-from-char vict +skill-sleeper+)
      (setf (position-of vict) +pos-resting+)
      (when (eql (hunting-of ch) vict)
        (stop-hunting vict))
      (let ((torch (read-object 3030)))
        (obj-to-char torch vict))
      (mudlog 'notice t "%s has been thrown into jail by ~a at ~d" (name-of vict)
            (name-of ch)
            (number-of (in-room-of ch)))
      (if (is-pc vict)
          (save-player-to-xml vict)
          (extract-creature vict nil)))))

(defun drag-char-to-jail (ch vict jail-room)
  (let ((dir (find-first-step (in-room-of ch)
                              jail-room
                              :std-track)))
    (when (and dir
               (can-go ch dir)
               (can-travel-terrain ch (terrain-of (to-room-of (exit ch dir))))
               (can-go vict dir)
               (can-travel-terrain vict (terrain-of (to-room-of (exit ch dir)))))
      (act ch :target vict
           :subject-emit (format nil "You drag a semi-conscious $N ~a" (aref +to-dirs+ dir))
           :target-emit "You dimly feel yourself being dragged down the street"
           :not-target-emit (format nil "$n drags a semi-conscious $N ~a" (aref +to-dirs+ dir)))
      (dolist (tch (people-of (in-room-of ch)))
        (when (eql (func-of (shared-of tch)) 'special-cityguard)
          (setf (func-data-of tch)
                (to-room-of (exit ch dir)))))
      (char-from-room ch nil)
      (char-to-room ch (real-room (to-room-of (exit ch dir))))
      (char-from-room vict nil)
      (char-to-room vict (real-room (to-room-of (exit vict dir))))
      (act ch :target vict
           :not-target-emit (format nil "$n drags $N in from ~a" (aref +from-dirs+ dir)))
      (wait-state ch (rl-sec 1)))))

(defun cityguard-parse-param (text)
  (let (jail-num hq-num deathspawn-list)
    (with-input-from-string (str text)
      (loop
         for line = (read-line str nil)
         as lineno from 0
         while line
         unless (string= (string-trim " " line) "")
         do
           (with-words line (param-key &rest value)
             (string-case param-key
               ("jailroom"
                (setf jail-num (parse-integer value :junk-allowed t)))
               ("headquarters"
                (setf hq-num (parse-integer value :junk-allowed t)))
               ("deathspawn"
                (push (parse-integer value :junk-allowed t) deathspawn-list))
               (t
                (signal 'specparam-error :msg "an invalid specparam line" :lineno lineno))))))
    (values jail-num hq-num deathspawn-list)))

(defun cityguard-idle-action (self)
  (let ((lawful (not (zone-flagged (zone-of (in-room-of self))
                                   +zone-nolaw+)))
        (action 0)
        (target nil))
    (dolist (tch (people-of (in-room-of self)))
      (unless (is-dead tch)
        (cond
          ((and (< action 5)
                (fighting-cityguard? tch)
                (can-attack self tch))
           (setf action 5
                 target tch))
          ((and (< action 4)
                (or (and lawful (criminalp tch))
                    (char-is-arrested? tch))
                (can-see-creature self tch)
                (not (pref-flagged tch +pref-nohassle+))
                (can-attack self tch))
           (setf action 4
                 target tch))
          ((and (< action 3)
                (or (and lawful (criminalp tch))
                    (char-is-arrested? tch))
                (<= (position-of tch)
                    +pos-sleeping+)
                (or (> (level-of tch) 20)
                    (> (remort-gen-of tch) 0)))
           (setf action 3
                 target tch))
          ((and (< action 3)
                (or (and lawful (criminalp tch))
                    (char-is-arrested? tch))
                (<= (position-of tch)
                    +pos-sleeping+)
                (or (> (level-of tch) 20)
                    (> (remort-gen-of tch) 0)))
           (setf action 3
                 target tch))
          ((and (< action 2)
                (can-see-creature self tch)
                (not (pref-flagged tch +pref-nohassle+))
                (fighting-of tch))
           (setf action 2
                 target tch))
          ((and (< action 1)
                (is-pc tch)
                (can-see-creature self tch)
                (not (pref-flagged tch +pref-nohassle+))
                (>= (reputation-of tch)
                    (floor +criminal-reputation+ 2)))
           (setf action 1
                 target tch)))))
    (values action target)))

(defun cityguard-perform-action (self action target jail-num)
  (ecase action
    (0
     ;; do general emote
     (setf target (get-char-random-vis self (in-room-of self)))
     (cond
       ((null target)
        nil)
       ((and (is-good self)
             (is-thief target))
        (if (is-evil target)
            (act self :target target :all-emit "$n look$% at $N suspiciously.")
            (act self :target target :all-emit "$n look$% at $N skeptically.")))
       ((and (is-npc target)
             (eql (func-of (shared-of target)) 'special-cityguard))
        (act self :target target :all-emit "$n nod$% at $N."))
       ((or
         (immortalp target)
         (and (or (is-cleric target)
                  (is-knight target))
              (eql (is-evil self)
                   (is-evil target))
              (not (is-neutral target))))
        (act self :target target :all-emit "$n bow$% before $N."))
       ((not (eql (is-evil self)
                  (is-evil target)))
        (case (random-range 0 2)
          (0
           (act self :target target :all-emit "$n watch$% $N carefully."))
          (1
           (act self :target target :all-emit "$n thoroughly examine$% $N."))
          (2
           (act self :all-emit "$n mutters something under $s breath."))))))
    (1
     ;; emote half-criminal
     (cond
       ((randomly-true 9)
        nil)
       ((randomly-true 4)
        (act self :target target :all-emit "$n growl$% at $N."))
       ((randomly-true 3)
        (act self :target target :all-emit "$n cracks$% $s knuckles."))
       ((get-eq self +wear-wield+)
        (let ((wield-hit-type (+ (obj-val-of (get-eq self +wear-wield+) 3)
                                 +type-hit+)))
          (when (member wield-hit-type (list +type-slash+ +type-pierce+ +type-stab+))
            (act self :target target :item (get-eq self +wear-wield+)
                 :all-emit "$n sharpens $p while watching $N."))))))
    (2
     ;; stop fight
     (case (random-range 0 2)
       (0
        (perform-say self "bellow" "Knock it off!"))
       (1
        (perform-say self "bellow" "Stop disturbing the peace of this city!"))
       (2
        (perform-say self "bellow" "Here now!")))
     (let ((vict (random-elt (fighting-of target))))
       (when vict
         (breakup-fight self target vict))))
    (3
     ;; drag criminal to jail
     (when (and jail-num
                (zerop (quest-id-of target))
                (< (position-of target)
                   +pos-fighting+))
       (cond
         ((= (number-of (in-room-of self)) jail-num)
          (throw-char-in-jail self target)
          (char-arrest-pardoned target)
          (forget self target))
         (t
          (drag-char-to-jail self target (real-room jail-num))))))
    (4
     ;; attack criminal
     (if (char-is-arrested? target)
         (perform-say self "scream" "HEY!!!  I know who you are!!!!")
         (perform-say self "scream" "HEY!!!  You're one of those CRIMINALS!!!!!!"))
     (char-under-arrest target)
     (attack self target (get-next-weapon self +type-undefined+) +type-undefined+))
    (5
     ;; assist cityguard
     (when (randomly-true 10)
       (if (randomly-true)
           (perform-say self "yell" "To arms!  To arms!!")
           (perform-say self "yell" "BAAAANNNZZZZZAAAAAIIIIII!!!")))
     (attack self target (get-next-weapon self +type-undefined+) +type-undefined+))))

(define-special cityguard (trigger self ch command vars) (+spec-mob+)
  (declare (ignore command vars))
  (when (func-param-of (shared-of self))
    (handler-case
        (multiple-value-bind (jail-num hq-num deathspawn-list)
            (cityguard-parse-param (func-param-of (shared-of self)))
          (case trigger
            (tick
             (when (awakep self)
               (cond
                 ((fighting-of self)
                  (let ((tch (random-elt (fighting-of self))))
                    ;; Save the newbies from themselves
                    (when (and (< (level-of tch)
                                  20)
                               (zerop (remort-gen-of tch)))
                      (perform-say self "declare" "Here now!")
                      (remove-combat self tch)
                      (remove-combat tch self)
                      (return-from special-cityguard t))
                    (char-under-arrest tch)
                    (when (randomly-true 4)
                      (call-for-help self tch)
                      t)))
                 ((func-data-of self)
                  ;; We're marching to where someone shouted for help
                  (cond
                    ((= (func-data-of self) (number-of (in-room-of self)))
                     (setf (func-data-of self) nil))
                    (t
                     (let* ((dir (find-first-step (in-room-of self)
                                                  (real-room (func-data-of self))
                                                  :std-track))
                            (next-room (real-room (to-room-of (exit ch dir)))))
                       (when (and (>= dir 0)
                                  (can-enter-room self next-room)
                                  (not (room-flagged next-room +room-death+)))
                         (smart-mobile-move self dir)
                         t)))))
                 (t
                  ;; The action value is in order of importance
                  ;; action = 0 : do emote
                  ;; action = 1 : emote half-criminal
                  ;; action = 2 : stop fight
                  ;; action = 3 : drag creature to jail
                  ;; action = 4 : attack criminal
                  ;; action = 5 : assist guard
                  (multiple-value-bind (action target)
                      (cityguard-idle-action self)
                    (cityguard-perform-action self action target jail-num))))))
            (death
             (let ((room (and hq-num (real-room hq-num))))
               (when room
                 (dolist (spawn-vnum deathspawn-list)
                   (let ((new-guard (read-mobile spawn-vnum)))
                     (setf (func-data-of new-guard)
                           (number-of (in-room-of self)))
                     (char-to-room new-guard room t))))))))
      (specparam-error (e)
        (unless (or (null ch) (is-npc ch))
          (cond
            ((immortalp ch)
             (perform-tell self ch (format nil "I have ~a in line ~d of my specparam."
                                           (msg-of e) (lineno-of e))))
            (t
             (mudlog 'error t "ERR: Mobile ~d has ~a in line ~d of specparam"
                     (vnum-of self)
                     (msg-of e)
                     (lineno-of e))
             (perform-tell self ch "Sorry.  I'm broken, but a god has already been notified."))))))))
