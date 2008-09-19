(in-package #:tempus)

(defvar +money-log-limit+ 1000000)

(defun perform-split (ch amount kind)
  nil)

(defun explode-all-sigils (ch)
  nil)

(defun consolidate-char-money (ch)
  (let ((credits 0)
        (gold 0))
    (setf (carrying-of ch)
          (delete-if
           (lambda (obj)
             (when (is-obj-kind obj +item-money+)
               (if (zerop (aref (value-of obj) 1))
                   (incf gold (aref (value-of obj) 0))
                   (incf credits (aref (value-of obj) 0)))
               t))
             (carrying-of ch)))
    (when (plusp gold)
      (incf (gold-of ch) gold)
      (when (> gold +money-log-limit+)
        (slog "MONEY: ~a picked up ~d gold in room #~d (~a)"
              (name-of ch) gold (number-of (in-room-of ch))
              (name-of (in-room-of ch))))
      (if (= gold 1)
          (send-to-char ch "There was only a single gold coin.~%")
          (send-to-char ch "There were ~d coins.~%" gold))

      (when (and (aff-flagged ch +aff-group+)
                 (pref-flagged ch +pref-autosplit+))
        (perform-split ch gold :gold)))
    (when (plusp credits)
      (incf (cash-of ch) credits)
      (when (> credits +money-log-limit+)
        (slog "MONEY: ~a picked up ~d credits in room #~d (~a)"
              (name-of ch) credits (number-of (in-room-of ch))
              (name-of (in-room-of ch))))
      (if (= credits 1)
          (send-to-char ch "There was only a single credit.~%")
          (send-to-char ch "There were ~d credits.~%" credits))

      (when (and (aff-flagged ch +aff-group+)
                 (pref-flagged ch +pref-autosplit+))
        (perform-split ch credits :credits)))))

(defun activate-char-quad (ch)
  nil)

(defun can-carry-items (ch)
  (+ 1
     (dex-of ch)
     (if (immortal-level-p ch) 100000 0)
     (if (aff2-flagged ch +aff2-telekinesis+) (floor (level-of ch) 4) 0)))

(defun can-carry-weight (ch)
  (let ((raw-weight (getf (aref +str-app+ (strength-apply-index ch)) :carry-w)))
    (max 10
         (+ (if (aff2-flagged ch +aff2-telekinesis+)
                (* raw-weight 2)
                raw-weight)
            (if (immortal-level-p ch) 100000 0)))))

(defun can-take-obj (ch obj check-weight print)
  (cond
    ((>= (carry-items-of ch) (can-carry-items ch))
     (when print
       (act ch :item obj
            :subject-emit "$p: you can't carry that many items."))
     nil)
    ((and check-weight
          (> (+ (carry-weight-of ch) (weight-of obj)) (can-carry-weight ch))
          (not (immortalp ch)))
     (when print
       (act ch :item obj
            :subject-emit "$p: you can't carry that much weight."))
     nil)
    ((and (not (can-wear obj +item-wear-take+))
          (not (immortalp ch)))
     (when print
       (act ch :item obj
            :subject-emit "$p: you can't take that!"))
     nil)
    (t
     t)))

(defun get-from-room (ch arg)
  (let ((objs (get-matching-objects ch arg (contents-of (in-room-of ch))))
        (mode (find-all-dots arg))
        (money-found nil)
        (quad-found nil)
        (sigil-found nil))
    (cond
      (objs
       (loop
          for obj-sublist on objs
          as obj = (first obj-sublist)
          as next-obj = (second obj-sublist)
          as counter from 1
          do
          (cond
            ((and (is-corpse obj)
                 (/= (corpse-idnum obj) (idnum-of ch))
                 (not (immortalp ch))
                 (eql (pk-style-of (zone-of (in-room-of ch))) +zone-neutral-pk+)
                 (not (is-npc ch))
                 (plusp (corpse-idnum obj)))
             (send-to-char ch "You can't take corpses in NPK zones!~%")
             (setf counter 0))
            ((can-take-obj ch obj t t)
             (obj-from-room obj)
             (obj-to-char obj ch)

             (cond
               ((is-obj-kind obj +item-money+)
                (setf money-found t))
               ((eql (vnum-of obj) +quad-vnum+)
                (setf quad-found t))
               ((and (not (zerop (sigil-idnum-of obj)))
                     (/= (sigil-idnum-of obj) (idnum-of ch)))
                (setf sigil-found t)))
             (when (or (null next-obj)
                       (string/= (name-of next-obj) (name-of obj)))
               (cond
                 ((= counter 1)
                  (act ch :item obj :all-emit "$n get$% $p."))
                 ((plusp counter)
                  (act ch :item obj
                       :all-emit (format nil "$n get$% $p. (x~d)" counter))))
               (setf counter 0))))))
      ((eql mode :find-indiv)
       (send-to-char ch "You don't see ~a ~a here.~%"
                     (a-or-an arg) arg))
      ((eql mode :find-all)
       (send-to-char ch "You didn't find anything to take.~%"))
      (t
       (send-to-char ch "You didn't find anything to take that looks like a '~a'.~%"
                     arg)))

    (when money-found
      (consolidate-char-money ch))
    (when quad-found
      (activate-char-quad ch))
    (when sigil-found
      (explode-all-sigils ch))))

(defun get-from-container (ch thing container)
  (let ((objs (get-matching-objects ch thing (contains-of container)))
        (mode (find-all-dots thing))
        (money-found nil)
        (quad-found nil)
        (sigil-found nil))
    (cond
      ((and (logtest (aref (value-of container) 1) +cont-closed+)
            (not (zerop (aref (value-of container) 3)))
            (not (immortalp ch)))
       (act ch :item container
            :subject-emit "$p is closed."))
      ((and (is-corpse container)
            (/= (corpse-idnum container) (idnum-of ch))
            (not (immortalp ch))
            (eql (pk-style-of (zone-of (in-room-of ch))) +zone-neutral-pk+)
            (not (is-npc ch))
            (plusp (corpse-idnum container)))
       (send-to-char ch "You may not loot corpses in NPK zones.~%"))
      (objs
       (loop
          for obj-sublist on objs
          as obj = (first obj-sublist)
          as next-obj = (second obj-sublist)
          as counter from 1
          do
          (cond
            ((can-take-obj ch obj t t)
             (obj-from-obj obj)
             (obj-to-char obj ch)
             (cond
               ((is-obj-kind obj +item-money+)
                (setf money-found t))
               ((eql (vnum-of obj) +quad-vnum+)
                (setf quad-found t))
               ((and (not (zerop (sigil-idnum-of obj)))
                     (/= (sigil-idnum-of obj) (idnum-of ch)))
                (setf sigil-found t)))
             (when (or (null next-obj)
                       (string/= (name-of next-obj) (name-of obj)))
               (cond
                 ((= counter 1)
                  (act ch :item obj :target-item container
                       :all-emit "$n get$% $p from $P."))
                 ((plusp counter)
                  (act ch :item obj
                       :target-item container
                       :all-emit (format nil "$n get$% $p from $P. (x~d)" counter))))
               (setf counter 0))))))
      ((eql mode :find-indiv)
       (act ch :item container
            :subject-emit (format nil "There doesn't seem to be ~a ~a in $p.~%"
                                  (a-or-an thing) thing)))
      ((eql mode :find-all)
       (act ch :item container
            :subject-emit "You didn't find anything to in $p.~%"))
      (t
       (act ch :item container
            (format nil "You didn't find anything in $p to take that looks like a '~a'.~%"
                    thing))))

    (when money-found
      (consolidate-char-money ch))
    (when quad-found
      (activate-char-quad ch))
    (when sigil-found
      (explode-all-sigils ch))))

(defun perform-drop (ch obj mode sname dest-room displayp)
  (when (is-obj-stat obj +item-nodrop+)
    (cond
      ((immortalp ch)
       (act ch :item obj
            :subject-emit (format nil "You can't ~a $p, it must be CURSED!" sname)))
      (t
       (act ch :item obj
             :subject-emit "You peel $p off your hand..."))))

  (when displayp
    (act ch :item obj :all-emit (format nil "$n ~a$% $p." sname)))

  (obj-from-char obj)

  (case mode
    (:drop
     (obj-to-room obj (in-room-of ch))
     (when (and (room-is-open-air (in-room-of ch))
                (exit ch +down+)
                (to-room-of (exit ch +down+))
                (not (logtest (exit-info-of (exit ch +down+)) +ex-closed+)))
       (act ch :item obj
            :all-emit "$p falls downward through the air, out of sight!")
       (obj-from-room obj)
       (obj-to-room obj (to-room-of (exit ch +down+)))
       (act nil :item obj
            :place-emit "$p falls from the sky and lands by your feet.")))
    (:donate
     (obj-to-room obj dest-room)
     (act nil :item obj
          :place-emit "$p suddenly appears in a puff of smoke!"))
    (:junk
     (extract-obj obj))))

(defcommand (ch "get") (:resting)
  (send-to-char ch "Get what?~%"))

(defcommand (ch "get" thing) (:resting)
    (cond
      ((>= (carry-items-of ch) (can-carry-items ch))
       (send-to-char ch "Your arms are already full!~%"))
      (t
       (get-from-room ch thing))))

(defcommand (ch "get" thing "from" container) (:resting)
  (cond
    ((>= (carry-items-of ch) (can-carry-items ch))
     (send-to-char ch "Your arms are already full!~%"))
    (t
     (let ((containers (append (get-matching-objects ch container
                                                     (carrying-of ch))
                               (get-matching-objects ch
                                              container
                                              (contents-of (in-room-of ch))))))
       (if containers
           (dolist (container containers)
             (get-from-container ch thing container))
           (send-to-char ch "You don't see any '~a'.~%" container))))))


(defcommand (ch "drop") (:resting)
  (send-to-char ch "What do you want to drop?"))

(defcommand (ch "drop" thing) (:resting)
  "Drop one or more objects"
  (let* ((dot-mode (find-all-dots thing))
         (objs (if (eql dot-mode :find-all)
                  (carrying-of ch)
                  (get-matching-objects ch thing (carrying-of ch)))))
    (cond
      (objs
       (loop
          for obj-sublist on objs
          as obj = (first obj-sublist)
          as next-obj = (second obj-sublist)
          as counter from 1
          do
            (perform-drop ch obj :drop "drop" (in-room-of ch) nil)
            (when (or (null next-obj)
                        (string/= (name-of next-obj) (name-of obj)))
                (cond
                  ((= counter 1)
                   (act ch :item obj :all-emit "$n drop$% $p."))
                  ((plusp counter)
                   (act ch :item obj
                        :all-emit (format nil "$n drop$% $p. (x~d)" counter))))
                (setf counter 0))))
      ((eql dot-mode :find-all)
       (send-to-char ch "You don't seem to be carrying anything.~%"))
      (t
       (send-to-char ch "You don't seem to have any ~as.~%" thing)))))
