(in-package #:tempus)

(defun perform-hit (ch target)
  (cond
    ((null target)
     (send-to-char ch "They don't seem to be here.~%")
     (wait-state ch 4))
    ((eql ch target)
     (act ch
          :subject-emit "You hit yourself...  OUCH!"
          :place-emit "$n hits $mself.  That's gotta hurt!"))
    ((and (aff-flagged ch +aff-charm+)
          (eql (master-of ch) target))
     (act ch :target target
          :subject-emit "$N is such a good friend, you simply can't hit $M."))
    ((not (can-attack ch target))
     nil)
    ((find target (fighting-of ch))
     (act ch :target target :subject-emit  "You concentrate your attacks on $N!")
     (setf (fighting-of ch)
           (cons target (delete target (fighting-of ch)))))
    (t
     (setf (move-of ch) (max 0 (- (move-of ch) 5)))
     (attack ch target +type-undefined+)
     (wait-state ch +pulse-violence+))))

(defun calc-fleeing-exp-penalty (ch tch)
  (unless tch
    (return-from calc-fleeing-exp-penalty 0))
  (let ((loss (* (level-of tch)
                 (1+ (floor (level-of ch)
                            (- (1+ +lvl-grimp+)
                               (level-of ch)))))))
    (when (is-remort ch)
      (setf loss (- loss (* loss (floor (remort-gen-of ch)
                                        (+ 2 (remort-gen-of ch)))))))

    (when (is-thief ch)
      (setf loss (floor loss 2)))
    loss))

(defun attempt-fleeing (ch)
  (let* ((dir (random-range 0 (1- +num-of-dirs+)))
         (exit-info (exit ch dir))
         (other-room (when exit-info (real-room (to-room-of exit-info)))))
    (when (and (can-go ch dir)
               (not (room-flagged other-room +room-noflee+))
               (not (room-flagged other-room +room-death+))
               (not (room-flagged other-room +room-godroom+))
               (or (is-pc ch) (not (room-flagged other-room +room-nomob+))))
      (act ch :place-emit "$n panics, and attempts to flee!")
      (when (or (room-is-open-air (in-room-of ch))
                (room-is-open-air other-room))
          (if (aff-flagged ch +aff-inflight+)
              (setf (position-of ch) +pos-flying+)
              (return-from attempt-fleeing nil)))
      (let* ((fighting (random-elt (fighting-of ch)))
             (loss (calc-fleeing-exp-penalty ch fighting)))
        (remove-all-combat ch)
        (when (= (do-simple-move ch dir :flee t) 1)
          (act ch :place-emit "$n tries to flee, but can't!")
          (return-from attempt-fleeing nil))

        (send-to-char ch "You flee head over heels.~%")
        (when fighting
          (gain-exp ch (- loss))
          (gain-exp fighting (floor loss 32)))
        t))))

(defun perform-flee (ch)
  (cond
    ((aff2-flagged ch +aff2-petrified+)
     (send-to-char ch "You are solid stone!~%"))
    ((and (aff2-flagged ch +aff2-berserk+)
          (fighting-of ch))
     (send-to-char ch "You are too enraged to flee!~%"))
    ((< (position-of ch)
        +pos-fighting+)
     (send-to-char ch "You can't flee until you get on your feet!~%"))
    (t
     (loop
        for time from 0 upto 6
        as success = (attempt-fleeing ch)
        until success
        finally (unless success
                  (send-to-char ch "PANIC!  You couldn't escape!~%"))))))

(defun perform-assist (ch tch)
  (let ((opponent (random-elt (remove-if-not (lambda (x)
                                               (is-visible-to x ch))
                                             (fighting-of tch)))))
    (cond
      ((fighting-of ch)
       (send-to-char ch "You're already fighting!  How can you assist someone else?~%"))
      ((eql ch tch)
       (send-to-char ch "You can't help yourself more than this!~%"))
      ((null (fighting-of tch))
       (act ch :target tch :subject-emit "But nobody is fighting $M!"))
      ((null opponent)
       (act ch :target tch :subject-emit "You can't see who is fighting $M!"))
      ((can-attack ch opponent)
       (act ch :target tch
            :subject-emit "You join the fight!"
            :target-emit "$N assists you!"
            :not-target-emit "$n assists $N.")
       (perform-hit ch opponent)))))

(defcommand (ch "assist") (:standing)
  (send-to-char ch "Whom do you wish to assist?~%"))

(defcommand (ch "assist" target) (:standing)
  (let ((tchs (resolve-alias ch target (people-of (in-room-of ch)))))
    (cond
      ((null tchs)
       (send-to-char ch "No-one by that name here.~%"))
      ((cdr tchs)
       (send-to-char ch "You can only assist one person at a time!~%"))
      (t
       (perform-assist ch (first tchs))))))

(defcommand (ch "flee") ()
  (perform-flee ch))

(defcommand (ch "attack") (:standing)
  (send-to-char ch "Attack who?~%"))

(defcommand (ch "attack" target) (:standing)
  (perform-hit ch (first (resolve-alias ch target (people-of (in-room-of ch))))))

(defcommand (ch "fight") (:standing)
  (send-to-char ch "Fight who?~%"))

(defcommand (ch "fight" target) (:standing)
  (perform-hit ch (first (resolve-alias ch target (people-of (in-room-of ch))))))

(defcommand (ch "hit") (:standing)
  (send-to-char ch "Hit who?~%"))

(defcommand (ch "hit" target) (:standing)
  (perform-hit ch (first (resolve-alias ch target (people-of (in-room-of ch))))))

(defcommand (ch "kill") (:standing)
  (send-to-char ch "Kill who?~%"))

(defcommand (ch "kill" target) (:standing)
  (perform-hit ch (first (resolve-alias ch target (people-of (in-room-of ch))))))

(defcommand (ch "murder") (:standing)
  (send-to-char ch "Murder who?~%"))

(defcommand (ch "murder" target) (:standing)
  (perform-hit ch (first (resolve-alias ch target (people-of (in-room-of ch))))))

(defcommand (ch "slay") (:standing)
  (send-to-char ch "Slay who?~%"))

(defcommand (ch "slay" target) (:standing)
  (let* ((targets (resolve-alias ch target (people-of (in-room-of ch))))
         (tch (first targets)))
    (cond
      ((null targets)
       (send-to-char ch "They aren't here.~%"))
      ((not (immortalp ch))
       (perform-hit ch (first (resolve-alias ch target (people-of (in-room-of ch))))))
      ((find ch targets)
       (send-to-char ch "Your mother would be so sad... :(~%"))
      (t
       (act ch :target tch
            :subject-emit "You chop $M to pieces!  Ah!  The blood!"
            :target-emit "$N chops you to pieces!"
            :not-target-emit "$N brutally slays $N!")
       (mudlog 'alert t "~a killed ~a with a wiz-slay at ~a"
               (name-of ch)
               (name-of tch)
               (name-of (in-room-of ch)))
       (raw-kill tch ch +type-slash+)))))