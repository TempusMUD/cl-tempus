(in-package #:tempus)

(defvar +money-log-limit+ 1000000)

(defun explode-sigil (ch obj)
  (cond
    ((or (room-flagged (in-room-of ch) +room-peaceful+)
         (eql (pk-style-of (zone-of (in-room-of ch))) :nopk))
     (act ch :item obj
          :subject-emit "$p feels rather warm to the touch and shudders violently."))
    (t
     (let* ((raw-dam (random-range (sigil-level-of obj) (* (sigil-level-of obj) 4)))
            (dam (if (mag-savingthrow ch (sigil-level-of obj) +saving-spell+)
                     (floor raw-dam 2)
                     raw-dam)))
       (act ch :item obj :all-emit "$p explodes when $n pick$% it up!!")
       (let ((killer (or (get-char-in-world-by-idnum (sigil-idnum-of obj))
                         (load-player-from-xml (sigil-idnum-of obj)))))
         (damage-creature killer ch dam nil +spell-warding-sigil+ +wear-hands+))

       (setf (sigil-idnum-of obj) 0)
       (setf (sigil-level-of obj) 0)))))

(defun explode-all-sigils (ch)
  (loop for obj in (copy-list (carrying-of ch))
       until (or (destroyedp obj) (deadp ch))
       when (and (plusp (sigil-idnum-of obj))
                 (/= (sigil-idnum-of obj) (idnum-of ch)))
       do (explode-sigil ch obj)))

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
        (perform-split ch gold 'gold)))
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
  (let ((quad (find +quad-vnum+ (carrying-of ch) :key 'vnum-of)))
    (when quad
      (call-magic ch ch nil nil +spell-quad-damage+ +lvl-grimp+ :spell)
      (extract-obj quad)
      (slog "~a got the Quad Damage at ~d"
            (name-of ch)
            (number-of (in-room-of ch)))

      ;; Notify the world of this momentous event
      (unless (or (room-flagged (in-room-of ch) +room-soundproof+)
                  (zone-flagged (zone-of (in-room-of ch)) +zone-soundproof+)
                  (zone-flagged (zone-of (in-room-of ch)) +zone-isolated+))
        (dolist (zone *zone-table*)
          (unless (or (zone-flagged zone +zone-soundproof+)
                      (zone-flagged zone +zone-isolated+)
                      (eql zone (zone-of (in-room-of ch)))
                      (/= (plane-of zone) (plane-of (zone-of (in-room-of ch)))))
            (dolist (room (world-of zone))
              (when (and (people-of room)
                         (not (room-flagged room +room-soundproof+)))
                (send-to-room room "You hear a screaming roar in the distance.~%"))))))
      (dolist (room (world-of (zone-of (in-room-of ch))))
        (when (and (people-of room)
                   (not (room-flagged room +room-soundproof+)))
          (send-to-room room "You hear a screaming roar nearby!~%"))))))


(defun perform-put (ch obj container)
  (cond
    ((is-obj-kind container +item-pipe+)
     (assert (is-obj-kind obj +item-tobacco+) nil "Invalid attempt to pack pipe with non-tobacco")
     (cond
       ((and (plusp (cur-drags container))
             (/= (smoke-type container) (smoke-type obj)))
        (act ch :item obj :target container
             :subject-emit "You need to clear out $P before packing it with $p."))
       ((not (plusp (- (max-drags obj) (cur-drags obj))))
        (act ch :item container
             :subject-emit "Sorry, $p is fully packed."))
       (t
        (setf (aref (value-of container) 0) (min (max-drags container)
                                                  (+ (cur-drags container)
                                                     (cur-drags obj))))
        (setf (aref (value-of container) 2) (aref (value-of obj) 0)))))
    ((is-obj-kind container +item-vehicle+)
     (let ((to-room (real-room (room-number container))))
       (cond
         ((null to-room)
          (act ch :item container
               :subject-emit "Sorry, $p doesn't seem to have an interior right now."))
         (t
          (obj-from-char obj)
          (obj-to-room obj to-room)))))
    (t
     (cond
       ((> (+ (weight-of container) (weight-of obj))
           (aref (value-of container) 0))
        (act ch :item obj :target-item container
             :subject-emit "$p won't fit in $P."))
       ((and (is-obj-stat obj +item-nodrop+)
             (not (immortalp ch)))
        (act ch :item obj
             :subject-emit "$p must be cursed!  You can't seem to let go of it..."))
       (t
        (obj-from-char obj)
        (obj-to-obj obj container))))))


(defun can-carry-items (ch)
  (+ 1
     (dex-of ch)
     (if (immortal-level-p ch) 100000 0)
     (if (aff2-flagged ch +aff2-telekinesis+) (floor (level-of ch) 4) 0)))

(defun can-carry-weight (ch)
  (let ((raw-weight (getf (aref +str-app+ (str-of ch)) :carry-w)))
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

(defun get-from-room (ch objs arg)
  (let ((mode (find-all-dots arg))
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
               (act ch :item obj
                    :all-emit (format nil "$n get$% $p.~[~;~:;~:* (x~d)~]" counter))
               (setf counter 0))))))
      ((eql mode :find-indiv)
       (multiple-value-bind (ignore name)
           (get-number arg)
         (declare (ignore ignore))
         (send-to-char ch "You don't see ~a ~a here.~%"
                       (a-or-an name) name)))
      ((eql mode :find-all)
       (send-to-char ch "You didn't find anything to take.~%"))
      (t
       (let ((name (subseq arg 4)))
         (send-to-char ch "You didn't find anything that looks like ~a ~a.~%"
                       (a-or-an name)
                       name))))

    (when money-found
      (consolidate-char-money ch))
    (when quad-found
      (activate-char-quad ch))
    (when sigil-found
      (explode-all-sigils ch))))

(defun perform-get-from-container (ch objs container name mode)
  (let ((money-found nil)
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
               (act ch :item obj
                    :target-item container
                    :all-emit (format nil "$n get$% $p from $P.~[~;~:;~:* (x~d)~]" counter))
               (setf counter 0))))))
      ((eql mode :find-indiv)
       (act ch :item container
            :subject-emit (format nil "There doesn't seem to be ~a ~a in $p."
                                  (a-or-an name) name)))
      ((eql mode :find-all)
       (act ch :item container
            :subject-emit "You didn't find anything to take in $p."))
      (t
       (act ch :item container
            :subject-emit (format nil "You didn't find anything in $p that looks like ~a ~a."
                                  (a-or-an name)
                                  name))))

    (when money-found
      (consolidate-char-money ch))
    (when quad-found
      (activate-char-quad ch))
    (when sigil-found
      (explode-all-sigils ch))))

(defun get-from-container (ch thing container)
  (let* ((objs (resolve-alias ch thing (contains-of container)))
         (mode (find-all-dots thing))
         (name (cond
                 ((eql mode :find-all)
                  (subseq thing 4))
                 ((eql mode :find-indiv)
                  (nth-value 2 (get-number thing)))
                 (t
                  thing))))
    (perform-get-from-container ch objs container name mode)))

(defun perform-autoloot (ch corpse)
  ;; Only loot the corpse if the character can see it, and if the
  ;; corpse isn't a player corpse in a NPK zone
  (when (and corpse
             (can-see-object ch corpse)
             (eql (in-room-of ch) (in-room-of corpse))
             (not (and (eql (pk-style-of (zone-of (in-room-of ch))) +zone-neutral-pk+)
                       (not (is-npc ch))
                       (not (immortalp ch))
                       (is-corpse corpse)
                       (/= (corpse-idnum corpse) (idnum-of ch))
                       (plusp (corpse-idnum corpse)))))
    (let ((money-found nil))
      (loop
         for obj-sublist on (contents-of corpse)
         as obj = (first obj-sublist)
         as next-obj = (second obj-sublist)
         as counter from 1
         do
           (when (and (can-take-obj ch obj t t)
                      (is-obj-kind obj +item-money+))
             (obj-from-obj obj)
             (obj-to-char obj ch)
             (setf money-found t))
           (when (or (null next-obj)
                     (string/= (name-of next-obj) (name-of obj)))
             (act ch :item obj
                  :target-item corpse
                  :all-emit (format nil "$n get$% $p from $P.~[~;~:;~:* (x~d)~]" counter))
             (setf counter 0)))
      (when money-found
        (consolidate-char-money ch)))))

(defun check-object-nodrop (ch obj mode)
  "Checks to see if OBJ can be dropped by CH.  May emit messages to CH.  Returns T if the object may be dropped, otherwise returns NIL."
  (cond
    ((not (is-obj-stat obj +item-nodrop+))
     t)
    ((immortalp ch)
     (act ch :item obj :subject-emit "You peel $p off your hand...")
     t)
    (t
     (act ch :item obj
          :subject-emit (format nil "You can't ~(~a~) $p, it must be CURSED!" mode))
     nil)))

(defun undisposablep (ch cmdstr obj displayp)
  (cond
    ((and (is-corpse obj)
          (plusp (corpse-idnum obj))
          (contains-of obj)
          (not (immortalp ch)))
     (when displayp
       (send-to-char "You can't ~a a player's corpse while it still has objects in it.~%" cmdstr))
     t)
    ((and (is-obj-kind obj +item-container+)
          (not (is-corpse obj))
          (contains-of obj))
     (when displayp
       (send-to-char "You can't ~a a container with items in it!~%" cmdstr))
     t)
    ((immortalp ch)
     nil)
    ((and (shared-of obj)
          (not (minusp (vnum-of obj)))
          (string/= (name-of obj) (name-of (real-object-proto (vnum-of obj)))))
     (when displayp
       (send-to-char ch "You can't ~a a renamed object!~%" cmdstr))
     t)
    (t
     nil)))

(defun perform-single-drop (ch obj mode)
  (when (check-object-nodrop ch obj mode)
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
       (let ((dest-room (real-room
                         (random-elt '(3032
                                       30032
                                       5510
                                       20470
                                       63102
                                       22942
                                       22807)))))
         (obj-to-room obj dest-room)
         (when (people-of dest-room)
           (act (first (people-of dest-room)) :item obj
                :place-emit "$p suddenly appears in a puff of smoke!"))))
      (:junk
       (extract-obj obj)))
    t))

(defun perform-drop (ch objects mode)
  (loop
     for obj-sublist on (copy-list objects)
     as obj = (first obj-sublist)
     as next-obj = (second obj-sublist)
     as counter from 1
     do
       (if (perform-single-drop ch obj mode)
           (when (or (null next-obj)
                     (string/= (name-of next-obj) (name-of obj)))
             (act ch :item obj
                  :all-emit (format nil "$n ~(~a~)$% $p.~[~;~:;~:* (x~d)~]" mode counter))
             (setf counter 0))
           (setf counter 0))))

(defparameter +money-log-limit+ 50000000)

(defun perform-drop-money (ch amount currency)
  (let ((on-hand (if (eql currency 'gold)
                     (gold-of ch)
                     (cash-of ch))))
  (cond
    ((minusp amount)
     (send-to-char ch "Heh heh heh... We are jolly funny today, eh?~%"))
    ((< on-hand amount)
     (send-to-char ch "You don't have that many coins!~%"))
    (t
     (let ((obj (make-money-object amount currency)))
       (obj-to-room obj (in-room-of ch))
       (act ch :item obj :all-emit "$n drop$% $p."))
     (if (eql currency 'gold)
         (decf (gold-of ch) amount)
         (decf (cash-of ch) amount))
     (when (>= amount +money-log-limit+)
       (slog "MONEY: ~a has dropped ~a ~a in room #~d (~a)"
             (name-of ch)
             amount
             (if (eql currency 'gold) "coins" "credits")
             (number-of (in-room-of ch))
             (name-of (in-room-of ch))))))))

(defun find-eq-pos (obj)
  (cdr
   (find-if (lambda (tuple)
              (can-wear obj (car tuple)))
            +wear-eq-positions+)))

(defun perform-wear (ch obj pos)
  (cond
    ((is-animal ch)
     (send-to-char ch "Animals don't wear things.~%"))
    ((not (can-wear obj (aref +wear-bitvectors+ pos)))
     (act ch :item obj
          :subject-emit "You can't wear $p there."))
    ((or (and (is-implant obj) (/= pos +wear-hold+))
         (is-obj-kind obj +item-tattoo+))
     (act ch :item obj
          :subject-emit "You cannot wear $p."))
    ((and (or (= pos +wear-finger-r+)
              (= pos +wear-neck-1+)
              (= pos +wear-wrist-r+)
              (= pos +wear-ear-l+))
          (aref (equipment-of ch) pos))
     (perform-wear ch obj (1+ pos)))
    ((aref (equipment-of ch) pos)
     (send-to-char ch "~a~%" (aref +already-wearing+ pos)))
    ((and (= pos +wear-belt+)
          (null (aref (equipment-of ch) +wear-waist+)))
     (act ch :item obj
          :subject-emit "You need to be WEARING a belt to put $p on it."))
    ((and (= pos +wear-hands+)
          (is-obj-stat2 obj +item2-two-handed+)
          (aref (equipment-of ch) +wear-wield+))
     (act ch :item obj
          :subject-emit "You can't be using your hands for anything else to use $p."))
    ((and (= pos +wear-shield+)
          (> (weight-of obj) (* 1.5 (getf (aref +str-app+
                                                (str-of ch))
                                          :wield-w))))
     (send-to-char ch "It's too damn heavy.~%"))
    ((and (not (approvedp obj))
          (not (immortal-level-p ch)))
     (act ch :item obj
          :subject-emit "$p has not been approved for mortal use."))
    ((and (is-obj-stat2 obj +item2-broken+)
          (not (immortalp ch)))
     (act ch :item obj
          :subject-emit "$p is broken and unusable until it is repaired."))
    ((and (or (= pos +wear-wield+) (= pos +wear-wield-2+))
          (not (is-obj-kind obj +item-weapon+))
          (not (is-obj-kind obj +item-energy-gun+))
          (not (is-obj-kind obj +item-gun+)))
     (send-to-char ch "You cannot wield non-weapons.~%"))
    ((and (not (is-npc ch))
          (not (zerop (owner-id-of (shared-of obj))))
          (/= (owner-id-of (shared-of obj)) (idnum-of ch)))
     (act ch :item obj :subject-emit "You may not use $p."))
    ((and (is-obj-stat2 obj +item2-no-mort+)
          (not (is-remort ch))
          (not (immortalp ch)))
     (act ch :item obj
          :subject-emit "You feel a strange sensation as you attempt to use $p."))
    ((and (is-obj-stat2 obj +item2-req-mort+)
          (is-remort ch)
          (not (immortalp ch)))
     (act ch :item obj
          :subject-emit "You feel a strange sensation as you attempt to use $p."))
    ((and (is-obj-stat2 obj +item2-singular+)
          (find (vnum-of obj) (remove nil (equipment-of ch)) :key 'vnum-of))
     (act ch :item obj
          :subject-emit "You cannot use more than one of $p."))
    ((and (is-obj-stat2 obj +item2-godeq+)
          (not (immortal-level-p ch)))
     (act ch :item obj
          :subject-emit "$p swiftly leaps off your body into your hands."))
    (t
     (act ch :item obj
          :subject-emit (aref +wear-messages+ pos 1)
          :place-emit (aref +wear-messages+ pos 0))
     (when (and (action-desc-of obj)
                (string/= (action-desc-of obj) "")
                (or (is-obj-kind obj +item-worn+)
                    (is-obj-kind obj +item-armor+)
                    (is-obj-kind obj +item-weapon+)))
       (act ch :item obj
            :subject-emit (action-desc-of obj)))
     (obj-from-char obj)
     (equip-char ch obj pos :worn)
     (check-eq-align ch))))

(defun perform-remove (ch pos)
  (let ((obj (aref (equipment-of ch) pos)))
    (assert obj nil "Bad pos passed to perform-remove")
    (cond
      ((>= (carry-items-of ch) (can-carry-items ch))
       (act ch :item obj :subject-emit "$p: you can't carry that many items!"))
      ((and (= (position-of ch) +pos-flying+)
            (is-obj-kind obj +item-wings+)
            (not (aff-flagged ch +aff-inflight+)))
       (act ch :item obj :subject-emit "$p: you probably shouldn't remove those while flying!"))
      ((is-obj-kind obj +item-tattoo+)
       (act ch :item obj :subject-emit "$p: you must have this removed by a professional."))
      ((and (is-obj-stat2 obj +item2-noremove+)
            (not (immortalp ch)))
       (act ch :item obj :subject-emit "$p: you cannot convince yourself to stop using it."))
      (t
       (if (and (= (worn-on-of obj) +wear-wield+)
                (aref (equipment-of ch) +wear-wield-2+))
           (act ch
                :item obj
                :target-item (aref (equipment-of ch) +wear-wield-2+)
                :all-emit "$n stop$% using $p and wield$% $P.")
           (act ch :item obj
                :all-emit "$n stop$% using $p."))
       (obj-to-char (unequip-char ch (worn-on-of obj) :worn nil) ch)))))

(defun perform-give (ch vict obj)
  (cond
    ((not (check-object-nodrop ch obj "let go"))
     nil)
    ((and (not (immortalp ch))
          (<= (position-of vict) +pos-sleeping+))
     (act ch :target vict
          :subject-emit "$E is currently unconscious."))
    ((>= (carry-items-of vict) (can-carry-items vict))
     (act ch :target vict
          :subject-emit "$E seems to have $S hands full."))
    ((>= (+ (weight-of obj) (carry-weight-of vict)) (can-carry-weight vict))
     (act ch :target vict
          :subject-emit "$E can't carry that much weight."))
    (t
     (obj-from-char obj)
     (obj-to-char obj vict)
     (when (and (is-obj-kind obj +item-money+)
                (> (aref (value-of obj) 0) +money-log-limit+))
       (slog "MONEY: ~a has given obj #~d (~a) worth ~d ~a to ~a in room #~d (~a)"
             (name-of ch) (vnum-of obj) (name-of obj)
             (aref (value-of obj) 0)
             (if (zerop (aref (value-of obj) 1)) "gold" "credits")
             (name-of vict)
             (number-of (in-room-of vict))
             (title-of (in-room-of vict))))
     t)))

(defun perform-give-money (ch target amount kind)
  (let ((money-desc (if (eql kind 'gold) "coin" "credit")))
    (ecase kind
      (gold
       (decf (gold-of ch) amount)
       (incf (gold-of target) amount))
      (cash
       (decf (cash-of ch) amount)
       (incf (cash-of target) amount)))

    (if (= amount 1)
        (act ch :target target
             :all-emit (format nil "$n give$% a single ~a to $N." money-desc))
        (act ch :target target
             :subject-emit (format nil "You give ~d ~as to $N."
                                   amount
                                   money-desc)
             :target-emit (format nil "$n gives ~d ~as to you."
                                  amount
                                  money-desc)
             :not-target-emit (format nil "$n gives some ~as to $N." money-desc)))
    (save-player-to-xml ch)
    (save-player-to-xml target)))

(defun perform-plant (ch vict obj)
  (cond
    ((not (check-object-nodrop ch obj "let go"))
     nil)
    ((>= (carry-items-of vict) (can-carry-items vict))
     (act ch :target vict
          :subject-emit "$E seems to have $S hands full."))
    ((>= (+ (weight-of obj) (carry-weight-of vict)) (can-carry-weight vict))
     (act ch :target vict
          :subject-emit "$E can't carry that much weight."))
    (t
     (obj-from-char obj)
     (obj-to-char obj vict)
     (when (and (is-obj-kind obj +item-money+)
                (> (aref (value-of obj) 0) +money-log-limit+))
       (slog "MONEY: ~a has planted obj #~d (~a) worth ~d ~a onto ~a in room #~d (~a)"
             (name-of ch) (vnum-of obj) (name-of obj)
             (aref (value-of obj) 0)
             (if (zerop (aref (value-of obj) 1)) "gold" "credits")
             (name-of vict)
             (number-of (in-room-of vict))
             (title-of (in-room-of vict))))

     (act ch :target vict :item obj
          :subject-emit "You plant $p on $N.")
     (if (or (< (+ (check-skill ch +skill-plant+) (dex-of ch))
                (+ (random-range 0 83) (wis-of vict)))
             (aff2-flagged vict +aff2-true-seeing+))
         (act ch :target vict :item obj
              :target-emit "$n puts $p in your pocket.")
         (gain-skill-proficiency ch +skill-plant+)))))

(defun name-from-drinkcon (obj drink)
  (setf (aliases-of obj)
        (cl-ppcre:regex-replace-all
         (cl-ppcre:create-scanner
          `(:sequence
            (:greedy-repetition 0 nil :whitespace-char-class)
            ,(aref +drink-names+ drink)
            (:greedy-repetition 0 nil :whitespace-char-class)))
         (aliases-of obj) "")))

(defun name-to-drinkcon (obj drink)
  (setf (aliases-of obj) (concatenate 'string
                                      (aliases-of obj)
                                      " "
                                      (aref +drink-names+ drink))))

(defun poison-char (ch level amount)
  (send-to-char ch "Oops, it tasted rather strange!~%")
  (act ch :place-emit
       (if (zerop (random-range 0 1))
           "$n chokes and utters some strange sounds."
           "$n sputters and coughs."))
  (let ((af (make-instance 'affected-type
                           :location +apply-str+
                           :modifier -2
                           :duration (* 3 amount)
                           :bitvector 0
                           :kind +spell-poison+
                           :level 30
                           :owner 0)))
    (case level
      (2
       (if (has-poison-3 ch)
           (setf (duration-of af) amount)
           (setf (bitvector-of af) +aff3-poison-2+))
       (setf (aff-index-of af) 3))
      (3
       (setf (bitvector-of af) +aff3-poison-3+)
       (setf (aff-index-of af) 3))
      (4
       (setf (kind-of af) +spell-sickness+)
       (setf (bitvector-of af) +aff3-sickness+)
       (setf (aff-index-of af) 3))
      (t
       (if (or (has-poison-3 ch) (has-poison-2 ch))
           (setf (duration-of af) amount)
           (setf (bitvector-of af) +aff-poison+))
       (setf (aff-index-of af) 1)))
    (affect-join ch af t t t nil)))

(defun perform-drink (ch obj)
  (cond
    ((is-obj-kind obj +item-potion+)
     (send-to-char ch "You must QUAFF that!~%"))
    ((not (or (is-obj-kind obj +item-drinkcon+)
              (is-obj-kind obj +item-fountain+)))
     (send-to-char ch "You can't drink from that!~%"))
    ((and (in-room-of obj)
          (is-obj-kind obj +item-drinkcon+))
     (send-to-char ch "You have to be carrying that to drink from it.~%"))
    ((or (= (get-condition ch +drunk+) 24)
         (> (get-condition ch +drunk+) (* (con-of ch) 2)))
     ;; The pig is drunk
     (act ch
          :subject-emit "You can't seem to get it close enough to your mouth."
          :place-emit "$n tries to drink but misses $s mouth!"))
    ((and (> (get-condition ch +full+) 20)
          (plusp (get-condition ch +thirst+)))
     (send-to-char ch "Your stomach can't contain anymore!~%"))
    ((zerop (aref (value-of obj) 1))
     (send-to-char ch "It's empty.~%"))
    (t
     (when (or (null (link-of ch))
               (zerop (repeat-cmd-count-of ch)))
       (act ch :item obj
            :place-emit (format nil "$n drink$% ~a from $p."
                                (aref +drinks+ (aref (value-of obj) 2)))))
     (if (string= "" (action-desc-of obj))
         (send-to-char ch "You drink the ~a.~%"
                       (aref +drinks+ (aref (value-of obj) 2)))
         (act ch :item obj :subject-emit (action-desc-of obj)))
     (let ((amount (if (> (aref (value-of obj) 1) -1)
                       (min (aref (value-of obj) 1) (random-range 1 3))
                       (random-range 1 3))))
       ;; Change weight of container
       (when (and (/= (aref (value-of obj) 1) -1)
                  (/= (vnum-of obj) -1))
         (setf (weight-of obj) (+ (weight-of (real-object-proto (vnum-of obj)))
                                  (floor (aref (value-of obj) 1) 10))))
       (let ((drunk (floor (* (aref +drink-affects+
                                    (aref (value-of obj) 2) +drunk+)
                              amount)
                           (if (is-monk ch) 4 16)))
             (full (floor (* (aref +drink-affects+
                                   (aref (value-of obj) 2) +full+)
                             amount)))
             (thirst (floor (* (aref +drink-affects+
                                     (aref (value-of obj) 2) +thirst+)
                               amount))))
         (when (pref-flagged ch +pref-debug+)
           (send-to-char ch "&c[DRINK] amount: ~d   drunk: ~d   full: ~d   thirst: ~d&n~%"
                         amount drunk full thirst))
         (gain-condition ch +drunk+ drunk)
         (gain-condition ch +full+ full)
         (gain-condition ch +thirst+ thirst))

       (cond
         ((or (> (get-condition ch +drunk+) (* (con-of ch) 2))
              (= (get-condition ch +drunk+) 24))
          (send-to-char ch "You are on the verge of passing out!~%"))
         ((> (get-condition ch +drunk+) (/ (* (con-of ch) 3) 4))
          (send-to-char ch "You are feeling no pain.~%"))
         ((> (get-condition ch +drunk+) (con-of ch))
          (send-to-char ch "You feel pretty damn drunk.~%"))
         ((> (get-condition ch +drunk+) (/ (con-of ch) 2))
          (send-to-char ch "You feel pretty good.~%")))

       (when (> (get-condition ch +thirst+) 20)
         (send-to-char ch "Your thirst has been quenched.~%"))

       (when (> (get-condition ch +full+) 20)
         (send-to-char ch "Your belly is satiated.~%"))

       ;; TODO: do something useful with radioactive drinks

       (when (plusp (aref (value-of obj) 3))
         (poison-char ch (aref (value-of obj) 3) amount))

       (unless (= (aref (value-of obj) 1) -1)
         (decf (aref (value-of obj) 1) amount)
         (when (zerop (aref (value-of obj) 1))
           (setf (aref (value-of obj) 2) 0)
           (setf (aref (value-of obj) 3) 0)
           (name-from-drinkcon obj (aref (value-of obj) 2))))))))

(defun perform-eating (ch obj)
  (cond
    ((and (not (is-obj-kind obj +item-food+))
          (not (immortalp ch)))
     (send-to-char ch "You can't eat THAT!~%"))
    ((> (get-condition ch +full+) 20)   ; stomach full
     (send-to-char ch "Your belly is stuffed to capacity!~%"))
    (t
     (act ch :item obj :all-emit "$n eat$% $p.")
     (let ((amount (aref (value-of obj) 0)))
       (gain-condition ch +full+ amount)
       ;; perform foody magics
       (when (and (is-obj-kind obj +item-food+)
                  (plusp (aref (value-of obj) 1))
                  (plusp (aref (value-of obj) 2)))
         (mag-objectmagic ch obj nil))
       (when (and (is-obj-kind obj +item-food+)
                  (> (get-condition ch +full+) 20))
         (send-to-char ch "You are satiated.~%"))
       (when (and (plusp (aref (value-of obj) 3))
                  (not (immortalp ch)))
         (poison-char ch (aref (value-of obj) 3) amount))
       (extract-obj obj)))))

(defun transfer-liquid (from-obj to-obj)
  (when (zerop (aref (value-of to-obj) 1))
    (name-to-drinkcon to-obj (aref (value-of from-obj) 2)))
  (setf (aref (value-of to-obj) 2) (aref (value-of from-obj) 2))
  (let ((amount (min (aref (value-of from-obj) 1)
                     (- (aref (value-of to-obj) 0) (aref (value-of to-obj) 1)))))
    (unless (= -1 (aref (value-of from-obj) 0))
      (decf (aref (value-of from-obj) 1) amount))
    (incf (aref (value-of to-obj) 1) amount))
  ;; Handle emptied container
  (when (zerop (aref (value-of from-obj) 1))
    (name-from-drinkcon from-obj (aref (value-of from-obj) 2))
    (setf (aref (value-of from-obj) 1) 0)
    (setf (aref (value-of from-obj) 2) 0)
    (setf (aref (value-of from-obj) 3) 0))
  ;; Transfer poison
  (setf (aref (value-of to-obj) 3)
        (if (not (and (zerop (aref (value-of from-obj) 3))
                      (zerop (aref (value-of to-obj) 3))))
            1 0))
  ;; Transfer weight
  (unless (= -1 (vnum-of from-obj))
    (let ((proto (real-object-proto (vnum-of from-obj))))
      (setf (weight-of from-obj) (+ (weight-of proto)
                                    (floor (aref (value-of from-obj) 1) 10)))))
  (unless (= -1 (vnum-of to-obj))
    (let ((proto (real-object-proto (vnum-of to-obj))))
      (setf (weight-of to-obj) (+ (weight-of proto)
                                    (floor (aref (value-of to-obj) 1) 10))))))

(defun perform-pour (ch from-obj to-obj)
  (cond
      ((not (or (is-obj-kind from-obj +item-drinkcon+)
                (is-obj-kind from-obj +item-fountain+)))
       (act ch :item from-obj :subject-emit "$p doesn't contain any liquids."))
      ((not (or (is-obj-kind to-obj +item-drinkcon+)
                (is-obj-kind to-obj +item-fountain+)))
       (act ch :item to-obj :subject-emit "$p can't contain liquids."))
      ((eql from-obj to-obj)
       (send-to-char ch "A most unproductive effort.~%"))
      ((or (minusp (aref (value-of to-obj) 0))
           (>= (aref (value-of to-obj) 1) (aref (value-of to-obj) 0)))
       (act ch :item to-obj :subject-emit "$p can't hold any more."))
      ((and (plusp (aref (value-of to-obj) 1))
            (/= (aref (value-of from-obj) 2) (aref (value-of to-obj) 2)))
       (send-to-char ch "There is already another liquid in it!~%"))
      (t
       (act ch :item to-obj
            :all-emit (format nil "$n pour$% ~a into $p."
                              (aref +drink-names+
                                    (aref (value-of from-obj) 2))))
       (transfer-liquid from-obj to-obj))))

(defun perform-pour-out (ch obj)
  (cond
    ((not (or (is-obj-kind obj +item-drinkcon+)
              (is-obj-kind obj +item-fountain+)))
     (act ch :item obj :subject-emit "$p doesn't contain any liquids."))
    ((zerop (aref (value-of obj) 1))
     (act ch :item obj :subject-emit "$p is empty."))
    ((= -1 (aref (value-of obj) 0))
     (act ch :item obj :subject-emit "$p will never run out."))
    (t
     (act ch :item obj
          :all-emit (format nil "$n pour$% ~a out of $p."
                            (aref +drinks+
                                  (aref (value-of obj) 2))))
     (name-from-drinkcon obj (aref (value-of obj) 2))
     (setf (aref (value-of obj) 1) 0)
     (setf (aref (value-of obj) 2) 0)
     (setf (aref (value-of obj) 3) 0)
     (unless (= -1 (vnum-of obj))
       (let ((proto (real-object-proto (vnum-of obj))))
         (setf (weight-of obj) (weight-of proto)))))))

(defun char-hands-free (ch)
  (flet ((count-eq-position (pos)
           (let ((obj (get-eq ch pos)))
             (cond
               ((null obj) 0)
               ((is-obj-stat2 obj +item2-two-handed+) 2)
               (t 1)))))
    (let ((hands-free (- 2
                         (count-eq-position +wear-wield+)
                         (count-eq-position +wear-wield-2+)
                         (count-eq-position +wear-shield+)
                         (count-eq-position +wear-hold+))))
      (assert (not (minusp hands-free)))
      hands-free)))

(defun perform-hold-light (ch obj)
  (assert (is-obj-kind obj +item-light+) nil "PERFORM-HOLD-LIGHT called on non-light")
  (if (zerop (aref (value-of obj) 2))
      (act ch :item obj :subject-emit "$p is no longer usable as a light.")
      (perform-wear ch obj +wear-light+)))

(defun perform-hold (ch obj)
  (if (and (not (can-wear obj +item-wear-hold+))
           (not (is-obj-kind obj +item-wand+))
           (not (is-obj-kind obj +item-staff+))
           (not (is-obj-kind obj +item-scroll+))
           (not (is-obj-kind obj +item-potion+))
           (not (is-obj-kind obj +item-syringe+)))
      (send-to-char ch "You can't hold that.~%")
      (perform-wear ch obj +wear-hold+)))

(defun perform-attach (ch obj to-obj)
  (cond
    ((or (and (is-obj-kind obj +item-scuba-mask+)
              (is-obj-kind to-obj +item-scuba-tank+))
         (and (is-obj-kind obj +item-scuba-tank+)
              (is-obj-kind to-obj +item-scuba-mask+)))
       (cond
         ((or (eql (aux-obj-of obj) to-obj)
              (eql (aux-obj-of to-obj) obj))
          (act ch :item obj :target-item to-obj
               :subject-emit "$p is already attached to $P."))
         (t
          (setf (aux-obj-of obj) to-obj)
          (setf (aux-obj-of to-obj) obj)
          (act ch :item obj :target-item to-obj
               :subject-emit "You attach $p to $P."
               :place-emit "$n attaches $p to $P."))))
    ((not (and (is-obj-kind obj +item-fuse+)
               (is-obj-kind to-obj +item-bomb+)))
     (act ch :item obj :target-item to-obj
          :subject-emit "You cannot attach $p to $P."))
    ((and (contains-of to-obj)
          (is-obj-kind (first (contains-of to-obj)) +item-fuse+))
     (act ch :item obj :target-item to-obj
          :subject-emit "$p is already attached to $P."))
    ((contains-of to-obj)
     (act ch :item obj :target-item to-obj
          :subject-emit "$P is jammed with $p."))
    ((plusp (fuse-state obj))
     (act ch :item obj :subject-emit "Better not do that -- $p is active!"))
    ((null (carried-by-of obj))
     (act ch :item obj
          :subject-emit "You can't attach $p while you are using it."))
    (t
     (obj-from-char obj)
     (obj-to-obj obj to-obj)
     (act ch :item obj :target-item to-obj
          :subject-emit "You attach $p to $P."
          :place-emit "$n attaches $p to $P."))))

(defun perform-detach (ch obj from-obj)
  (cond
    ((or (null (aux-obj-of obj))
         (not (eql from-obj (aux-obj-of obj))))
     (act ch :item obj :target-item from-obj
          :subject-emit "$p is not attached to $P."))
    ((not (or (is-obj-kind obj +item-scuba-mask+)
              (is-obj-kind obj +item-scuba-tank+)))
     (send-to-char ch "...~%"))
    (t
     (setf (aux-obj-of obj) nil)
     (setf (aux-obj-of from-obj) nil)
     (act ch :item obj :target-item from-obj
          :subject-emit "You detach $p from $P."
          :place-emit "$n detaches $p from $P."))))

(defun perform-conceal (ch obj)
  (cond
    ((and (in-room-of obj) (not (can-take-obj ch obj t nil)))
     (send-to-char ch "You'd have a hard time concealing that.~%"))
    (t
     (act ch :item obj
          :subject-emit "You conceal $p."
          :place-emit (if (in-room-of obj)
                          "$n conceals $p."
                          "$n conceals something."))
     (wait-state ch (rl-sec 1))
     (when (> (+ (check-skill ch +skill-conceal+) (level-of ch))
              (random-range 60 120))
       (setf (extra2-flags-of obj) (logior (extra2-flags-of obj) +item2-hidden+))
       (gain-skill-proficiency ch +skill-conceal+)))))

(defun perform-sacrifice (ch obj)
  (cond
    ((and (not (can-wear obj +item-wear-take+))
          (immortalp ch))
     (send-to-char ch "You can't sacrifice that.~%"))
    ((undisposablep ch "sacrifice" obj t)
     nil)
    (t
     (act ch :item obj :all-emit "$n sacrifice$% $p.")
     (let ((mana (cond
                   ((not (is-corpse obj))
                    (max 0 (floor (cost-of obj) 100000)))
                   ((is-corpse obj)
                    (let ((orig-char (load-corpse-owner obj)))
                      (if orig-char
                          (random-range 1 (get-level-bonus orig-char))
                          0))))))
       (when (plusp mana)
         (send-to-char ch "You sense your deity's favor upon you.~%")
         (setf (mana-of ch) (pin (+ (mana-of ch) mana) 0 (max-mana-of ch)))))
     (extract-obj obj))))

(defun perform-empty (ch container)
  (cond
    ((is-obj-kind container +item-drinkcon+)
     (perform-pour-out ch container))
    ((not (is-obj-kind container +item-container+))
     (send-to-char ch "You can't empty that.~%"))
    ((and (not (is-corpse container))
          (logtest (aref (value-of container) 1) +cont-closeable+)
          (logtest (aref (value-of container) 1) +cont-closed+))
     (send-to-char ch "It seems to be closed.~%"))
    ((and (is-corpse container)
          (plusp (corpse-idnum container))
          (/= (idnum-of ch) (corpse-idnum container))
          (not (immortalp ch)))
     (send-to-char ch "You can't empty a player's corpse.~%"))
    ((null (contains-of container))
     (act ch :item container :subject-emit "$p is already empty."))
    (t
     (dolist (obj (copy-list (contains-of container)))
       (when (and (not (is-obj-stat obj +item-nodrop+))
                  (can-wear obj +item-wear-take+))
         (obj-from-obj obj)
         (obj-to-room obj (in-room-of ch))))
     (act ch :item container
          :subject-emit "You carefully empty the contents of $p."
          :place-emit "$n empties the contents of $p.")
     (when (contains-of container)
       (send-to-char ch "There seems to be something still stuck in it.~%")))))

(defun perform-empty-into (ch from-obj to-obj)
  (cond
    ((eql from-obj to-obj)
     (send-to-char ch "Ha ha.  Very funny.~%"))
    ((and (is-obj-kind from-obj +item-drinkcon+)
          (is-obj-kind to-obj +item-drinkcon+))
     (perform-pour ch from-obj to-obj))
    ((and (not (is-obj-kind from-obj +item-drinkcon+))
          (is-obj-kind to-obj +item-drinkcon+))
     (send-to-char ch "You can't pour non-liquids into a drink container.~%"))
    ((and (is-obj-kind from-obj +item-drinkcon+)
          (not (is-obj-kind to-obj +item-drinkcon+)))
     (send-to-char ch "The liquid would leak out!~%"))
    ((not (is-obj-kind from-obj +item-container+))
     (send-to-char ch "You can't empty that.~%"))
    ((not (is-obj-kind to-obj +item-container+))
     (send-to-char ch "You can't empty into that.~%"))
    ((is-corpse to-obj)
     (send-to-char ch "You can't empty things into a corpse.~%"))
    ((and (not (is-corpse from-obj))
          (logtest (aref (value-of from-obj) 1) +cont-closeable+)
          (logtest (aref (value-of from-obj) 1) +cont-closed+))
     (act ch :item from-obj :subject-emit "$p seems to be closed."))
    ((and (logtest (aref (value-of from-obj) 1) +cont-closeable+)
          (logtest (aref (value-of from-obj) 1) +cont-closed+))
     (act ch :item to-obj :subject-emit "$p seems to be closed."))
    ((and (is-corpse from-obj)
          (plusp (corpse-idnum from-obj))
          (/= (idnum-of ch) (corpse-idnum from-obj))
          (not (immortalp ch)))
     (send-to-char ch "You can't empty a player's corpse.~%"))
    ((null (contains-of from-obj))
     (act ch :item from-obj :subject-emit "$p is already empty."))
    (t
     (dolist (obj (copy-list (contains-of from-obj)))
       (when (and (not (is-obj-stat obj +item-nodrop+))
                  (can-wear obj +item-wear-take+))
         (obj-from-obj obj)
         (obj-to-obj obj to-obj)))
     (act ch :item from-obj :target-item to-obj
          :subject-emit "You carefully empty the contents of $p into $P."
          :place-emit "$n empties the contents of $p into $P.")
     (when (contains-of from-obj)
       (send-to-char ch "There seems to be something still stuck in it.~%")))))

(defcommand (ch "get") (:resting)
  (send-to-char ch "Get what?~%"))

(defcommand (ch "get" thing) (:resting)
    (cond
      ((>= (carry-items-of ch) (can-carry-items ch))
       (send-to-char ch "Your arms are already full!~%"))
      (t
       (get-from-room ch (resolve-alias ch thing (contents-of (in-room-of ch))) thing))))

(defcommand (ch "get" thing "from" container) (:resting)
  (cond
    ((>= (carry-items-of ch) (can-carry-items ch))
     (send-to-char ch "Your arms are already full!~%"))
    (t
     (let ((containers (append (resolve-alias ch container
                                                     (carrying-of ch))
                               (resolve-alias ch
                                              container
                                              (contents-of (in-room-of ch))))))
       (if containers
           (dolist (container containers)
             (get-from-container ch thing container))
           (send-to-char ch "You don't see any '~a'.~%" container))))))

(defcommand (ch "put") (:resting)
  (send-to-char ch "Put what in what?~%"))

(defcommand (ch "put" thing) (:resting)
  (send-to-char ch "What do you want to put ~a in?~%"
                (if (eql (find-all-dots thing) :find-indiv) "it" "them")))

(defcommand (ch "put" thing "into" container-str) (:resting)
  (let* ((locations (append (carrying-of ch)
                            (coerce (remove nil (equipment-of ch)) 'list)
                            (contents-of (in-room-of ch))))
         (containers (resolve-alias ch container-str locations))
         (container (first containers))
         (objs (resolve-alias ch thing (carrying-of ch))))
    (cond
      ((null containers)
       (send-to-char ch "You don't see ~a ~a here.~%"
                     (a-or-an container-str) container-str))
      ((> (length containers) 1)
       (send-to-char ch "You can only put things into one container at a time.~%"))
      ((and (is-obj-kind container +item-container+)
            (is-obj-kind container +item-vehicle+)
            (is-obj-kind container +item-pipe+))
       (act ch :item container
            :subject-emit "$p is not a container.~%"))
      ((and (not (is-obj-kind container +item-pipe+))
            (logtest (aref (value-of container) 1) +cont-closed+))
       (send-to-char ch "You'd better open it first!~%"))
      ((null objs)
       (send-to-char ch "You aren't carrying ~a ~a.~%"
                     (a-or-an thing) thing))
      (t
       (loop
          for obj-sublist on objs
          as obj = (first obj-sublist)
          as next-obj = (second obj-sublist)
          as counter from 1
          do
          (cond
            ((eql obj container)
             (act ch :item obj
                  :subject-emit "You attempt to fold $p into itself, but fail."))
            ((and (is-obj-kind obj +item-bomb+)
                  (contains-of obj)
                  (is-obj-kind (contains-of obj) +item-fuse+)
                  (plusp (fuse-state (contains-of obj))))
             (act ch :item obj :target-item container
                  :subject-emit "It would really be best if you didn't put $p into $P."))
            ((and (is-obj-kind container +item-pipe+)
                  (not (is-obj-kind obj +item-tobacco+)))
             (act ch :item obj :target-item container
                  :subject-emit "You can't pack $P with $p!"))
            (t
             (perform-put ch obj container)

             (when (or (null next-obj)
                       (string/= (name-of next-obj) (name-of obj)))
               (act ch :item obj
                    :target-item container
                    :all-emit (format nil "$n put$% $p into $P.~[~;~:;~:* (x~d)~]" counter))
               (setf counter 0)))))
       (when (is-obj-kind container +item-pipe+)
         (loop while (contains-of container) do
              (extract-obj (contains-of container))))))))


(defcommand (ch "drop") (:resting)
  (send-to-char ch "What do you want to drop?"))

(defcommand (ch "drop" thing) (:resting)
  "Drop one or more objects"
  (let* ((dot-mode (find-all-dots thing))
         (objs (if (eql dot-mode :find-all)
                  (carrying-of ch)
                  (resolve-alias ch thing (carrying-of ch)))))
    (cond
      (objs
       (perform-drop ch objs :drop))
      ((eql dot-mode :find-all)
       (send-to-char ch "You don't seem to be carrying anything.~%"))
      (t
       (send-to-char ch "You don't seem to have any ~as.~%" thing)))))

(defcommand (ch "drop" amount-str "gold") (:resting)
  "Drop an amount of gold"
  (let ((amount (parse-integer amount-str :junk-allowed t)))
    (cond
      ((null amount)
       (send-to-char ch "That's not a proper amount of gold."))
      (t
       (perform-drop-money ch amount 'gold)))))

(defcommand (ch "drop" amount-str "coin") (:resting)
  "Drop an amount of gold"
  (let ((amount (parse-integer amount-str :junk-allowed t)))
    (cond
      ((null amount)
       (send-to-char ch "That's not a proper amount of gold."))
      (t
       (perform-drop-money ch amount 'gold)))))

(defcommand (ch "drop" amount-str "coins") (:resting)
  "Drop an amount of gold"
  (let ((amount (parse-integer amount-str :junk-allowed t)))
    (cond
      ((null amount)
       (send-to-char ch "That's not a proper amount of gold."))
      (t
       (perform-drop-money ch amount 'gold)))))

(defcommand (ch "drop" amount-str "credits") (:resting)
  "Drop an amount of gold"
  (let ((amount (parse-integer amount-str :junk-allowed t)))
    (cond
      ((null amount)
       (send-to-char ch "That's not a proper amount of credits."))
      (t
       (perform-drop-money ch amount 'cash)))))

(defcommand (ch "wear") (:resting)
  (send-to-char ch "What do you want to wear?~%"))

(defcommand (ch "wear" thing) (:resting)
  (let* ((objs (resolve-alias ch thing (carrying-of ch)))
         (wear-objs (delete nil
                            (mapcar (lambda (obj)
                                      (let ((pos (find-eq-pos obj)))
                                        (when pos
                                          (cons obj pos))))
                                    objs))))
    (cond
      ((null objs)
       (case (find-all-dots thing)
         (:find-all
          (send-to-char ch "You aren't carrying anything.~%"))
         (:find-alldot
          (send-to-char ch "You don't seem to have any '~a'.~%"
                        thing))
         (:find-indiv
          (send-to-char ch "You don't seem to have ~a ~a.~%"
                        (a-or-an thing) thing))))
      ((null wear-objs)
       (case (find-all-dots thing)
         (:find-all
          (send-to-char ch "You don't seem to have anything wearable.~%"))
         (:find-alldot
          (send-to-char ch "You don't seem to have any wearable '~a'.~%"
                        thing))
         (:find-indiv
          (act ch :item (first objs)
               :subject-emit "You can't wear $p."))))
      (t
       (dolist (tuple wear-objs)
         (perform-wear ch (car tuple) (cdr tuple)))))))

(defcommand (ch "wear" thing "on" pos-str) (:resting)
    (let ((pos (position pos-str +wear-keywords+ :test #'string-abbrev))
          (objs (resolve-alias ch thing (carrying-of ch))))
      (cond
        ((null objs)
         (send-to-char "You don't seem to have any '~a'.~%"
                       thing))
        ((> (length objs) 1)
         (send-to-char ch "You can't wear more than one item on a position.~%"))
        ((null pos)
         (send-to-char ch "'~a'?  What part of your body is THAT?~%" pos-str))
        (t
         (perform-wear ch (car objs) pos)))))

(defcommand (ch "wear" thing "about" "body") (:resting)
    (let ((objs (resolve-alias ch thing (carrying-of ch))))
      (cond
        ((null objs)
         (send-to-char "You don't seem to have any '~a'.~%"
                       thing))
        ((> (length objs) 1)
         (send-to-char ch "You can't wear more than one item on a position.~%"))
        (t
         (perform-wear ch (car objs) +wear-about+)))))

(defcommand (ch "wear" thing "up" "ass") (:resting)
    (let ((objs (resolve-alias ch thing (carrying-of ch))))
      (cond
        ((null objs)
         (send-to-char "You don't seem to have any '~a'.~%"
                       thing))
        ((> (length objs) 1)
         (send-to-char ch "You can't wear more than one item on a position.~%"))
        (t
         (perform-wear ch (car objs) +wear-ass+)))))

(defcommand (ch "remove") (:resting)
  (send-to-char ch "Remove what?~%"))

(defcommand (ch "remove" thing) (:resting)
  (let* ((objs (resolve-alias ch thing
                                     (coerce (remove nil (equipment-of ch)) 'list))))
    (cond
      ((and (aff3-flagged ch +aff3-attraction-field+)
            (not (pref-flagged ch +pref-nohassle+)))
       (send-to-char ch "You cannot remove anything while generating an attraction field!"))
      ((null objs)
       (case (find-all-dots thing)
         (:find-all
          (send-to-char ch "You're not using anything.~%"))
         (:find-alldot
          (send-to-char ch "You don't seem to be using any ~as.~%"
                        thing))
         (:find-indiv
          (send-to-char ch "You don't seem to have ~a ~a.~%"
                        (a-or-an thing) thing))))
      (t
       (dolist (obj objs)
         (perform-remove ch (worn-on-of obj)))))))

(defcommand (ch "remove" thing "from" pos-str) (:resting)
  (let* ((pos (position pos-str +wear-keywords+ :test #'string-abbrev))
         (obj (when pos (aref (equipment-of ch) pos))))
    (cond
      ((null pos)
       (send-to-char ch "'~a'?  What part of your body is THAT?~%" pos-str))
      ((null obj)
       (send-to-char ch "You aren't wearing anything there.~%"))
      ((not (is-alias-of thing (aliases-of obj)))
       (send-to-char ch "You aren't wearing ~a ~a there.~%"
                     (a-or-an thing) thing))
      (t
       (perform-remove ch pos)))))

(defcommand (ch "give") (:resting)
  (send-to-char ch "Give what to who?~%"))

(defcommand (ch "give" thing) (:resting)
  (declare (ignore thing))
  (send-to-char ch "Who do you want to give it to?~%"))

(defcommand (ch "give" thing "to" target) (:resting)
  (let* ((objs (resolve-alias ch thing (carrying-of ch)))
         (victs (resolve-alias ch target (people-of (in-room-of ch))))
         (vict (first victs))
         (mode (find-all-dots thing)))
    (cond
      ((null vict)
       (send-to-char ch "No-one by that name here.~%"))
      ((rest victs)
       (send-to-char ch "You can't give to more than one person.~%"))
      ((eql ch vict)
       (send-to-char ch "What's the point of that?~%"))
      (objs
       (loop
          for obj-sublist on objs
          as obj = (first obj-sublist)
          as next-obj = (second obj-sublist)
          as counter from 1
          do
          (cond
            ((perform-give ch (first victs) obj)
             (when (or (null next-obj)
                       (string/= (name-of next-obj) (name-of obj)))
               (act ch :target vict :item obj
                    :all-emit (format nil "$n give$% $p to $N.~[~;~:;~:* (x~d)~]" counter))
               (setf counter 0)))
            (t
             (setf counter 0)))))
      ((eql mode :find-indiv)
       (send-to-char ch "You aren't carrying ~a ~a.~%" (a-or-an thing) thing))
      ((eql mode :find-all)
       (send-to-char ch "You aren't carrying anything.~%"))
      (t
       (send-to-char ch "You aren't carrying any ~as.~%" (a-or-an thing) thing)))))

(defcommand (ch "give" amount-str "coins" "to" target) (:resting)
  (let ((amount (parse-integer amount-str :junk-allowed t))
        (targets (resolve-alias ch target (people-of (in-room-of ch)))))
    (cond
      ((notevery #'digit-char-p amount-str)
       (send-to-char ch "That's a bogus number of coins.~%"))
      ((> amount (gold-of ch))
       (send-to-char ch "You don't have that much money!~%"))
      ((null targets)
       (send-to-char ch "No-one by that name here.~%"))
      ((rest targets)
       (send-to-char ch "You can't give money to multiple people at once.~%"))
      (t
       (perform-give-money ch (first targets) amount 'gold)))))


(defcommand (ch "give" amount-str "credits" "to" target) (:resting)
  (let ((amount (parse-integer amount-str :junk-allowed t))
        (targets (resolve-alias ch target (people-of (in-room-of ch)))))
    (cond
      ((notevery #'digit-char-p amount-str)
       (send-to-char ch "That's a bogus number of credits.~%"))
      ((> amount (gold-of ch))
       (send-to-char ch "You don't have that much money!~%"))
      ((null targets)
       (send-to-char ch "No-one by that name here.~%"))
      ((rest targets)
       (send-to-char ch "You can't give money to multiple people at once.~%"))
      (t
       (perform-give-money ch (first targets) amount 'cash)))))

(defcommand (ch "plant" thing) (:resting)
  (declare (ignore thing))
  (send-to-char ch "Plant what on whom?~%"))

(defcommand (ch "plant" thing "on" target) (:resting)
  (let* ((objs (resolve-alias ch thing (carrying-of ch)))
         (victs (resolve-alias ch target (people-of (in-room-of ch))))
         (vict (first victs))
         (mode (find-all-dots thing)))
    (cond
      ((null vict)
       (send-to-char ch "No-one by that name here.~%"))
      ((rest objs)
       (send-to-char ch "You can't plant more than one item at a time.~%"))
      ((rest victs)
       (send-to-char ch "You can't give to more than one person.~%"))
      ((eql ch vict)
       (send-to-char ch "What's the point of that?~%"))
      (objs
       (perform-plant ch (first victs) (first objs)))
      ((eql mode :find-indiv)
       (send-to-char ch "You aren't carrying ~a ~a.~%" (a-or-an thing) thing))
      ((eql mode :find-all)
       (send-to-char ch "You aren't carrying anything.~%"))
      (t
       (send-to-char ch "You aren't carrying any ~as.~%" (a-or-an thing) thing)))))

(defcommand (ch "drink" thing) (:resting)
  (let* ((objs (resolve-alias ch thing (append
                                               (carrying-of ch)
                                               (contents-of (in-room-of ch)))))
         (obj (first objs)))
    (cond
      ((null obj)
       (send-to-char ch "You can't find it!~%"))
      ((rest objs)
       (send-to-char ch "You can only drink from one thing at a time!~%"))
      (t
       (perform-drink ch obj)))))

(defcommand (ch "eat") (:resting)
  (send-to-char ch "Eat what?~%"))

(defcommand (ch "eat" thing) (:resting)
  (let* ((objs (resolve-alias ch thing (append
                                               (carrying-of ch)
                                               (contents-of (in-room-of ch)))))
         (obj (first objs)))
    (cond
      ((null obj)
       (send-to-char ch "You don't seem to have ~a ~a.~%"
                     (a-or-an thing) thing))
      ((rest objs)
       (send-to-char ch "You can only eat one thing at a time!~%"))
      (t
       (perform-eating ch obj)))))

(defcommand (ch "pour") (:resting)
  (send-to-char ch "What do you want to pour?~%"))

(defcommand (ch "pour" "out") (:resting)
  (send-to-char ch "What do you want to pour out?~%"))

(defcommand (ch "pour" "out" thing) (:resting)
  (let* ((objs (resolve-alias ch thing (carrying-of ch))))
    (cond
      ((null objs)
       (send-to-char ch "You can't find ~a ~a.~%" (a-or-an thing) thing))
      ((rest objs)
       (send-to-char ch "You can't pour out than one container at a time!~%"))
      (t
       (perform-pour-out ch (first objs))))))

(defcommand (ch "pour" from-thing "into" to-thing) (:resting)
  (let* ((from-objs (resolve-alias ch from-thing (carrying-of ch)))
         (from-obj (first from-objs))
         (to-objs (resolve-alias ch to-thing (append
                                                     (carrying-of ch)
                                                     (contents-of (in-room-of ch)))))
         (to-obj (first to-objs)))
    (cond
      ((null from-obj)
       (send-to-char ch "You can't find ~a ~a.~%"
                     (a-or-an from-thing)
                     from-thing))
      ((null to-obj)
       (send-to-char ch "You can't find ~a ~a.~%"
                     (a-or-an to-thing)
                     to-thing))
      ((rest from-objs)
       (send-to-char ch "You can't pour from more than one container at a time!~%"))
      ((rest to-objs)
       (send-to-char ch "You can't pour into more than one container at a time!~%"))
      (t
       (perform-pour ch from-obj to-obj)))))

(defcommand (ch "wield") (:resting)
  (send-to-char ch "Wield what?~%"))

(defcommand (ch "wield" thing) (:resting)
  (let* ((objs (resolve-alias ch thing (carrying-of ch)))
         (obj (first objs))
         (hands-free (char-hands-free ch)))
    (cond
      ((null objs)
       (send-to-char ch "You don't seem to have ~a ~a.~%"
                     (a-or-an thing)
                     thing))
      ((is-animal ch)
       (send-to-char ch "Animals don't wield weapons.~%"))
      ((not (can-wear obj +item-wear-wield+))
       (send-to-char ch "You can't wield that.~%"))
      ((> (weight-of obj) (getf (aref +str-app+
                                                (str-of ch))
                                          :wield-w))
       (send-to-char ch "It's too damn heavy.~%"))
      ((and (is-cleric ch)
            (not (is-evil ch))
            (member (+ (aref (value-of obj) 3) +type-hit+)
                    (list +type-slash+
                          +type-pierce+
                          +type-stab+
                          +type-rip+
                          +type-chop+
                          +type-claw+)))
       (send-to-char ch "You can't wield that as a cleric.~%"))
      ((zerop hands-free)
       (send-to-char ch "You don't have a hand free to wield it with.~%"))
      ((and (/= hands-free 2)
            (is-obj-stat2 obj +item2-two-handed+))
       (act ch :item obj
            :subject-emit "You need both hands free to wield $p."))
      ((and (get-eq ch +wear-hands+)
            (is-obj-stat2 (get-eq ch +wear-hands+) +item2-two-handed+))
       (act ch :item (get-eq ch +wear-hands+)
            :subject-emit "You can't wield anything while wearing $p on your hands."))
      ((null (get-eq ch +wear-wield+))
       ;; normal wield
       (perform-wear ch obj +wear-wield+))
      ((and (is-merc ch)
            (is-any-gun (get-eq ch +wear-wield+))
            (is-any-gun obj))
       ;; mercs can dual wield any gun
       (perform-wear ch obj +wear-wield-2+))
      ((if (<= (weight-of (get-eq ch +wear-wield+)) 6)
           (> (weight-of obj) (weight-of (get-eq ch +wear-wield+)))
           (> (weight-of obj) (floor (weight-of (get-eq ch +wear-wield+)) 2)))
       ;; dual wield weight restrictions
       (send-to-char ch "Your secondary weapon must weigh less than half of your primary weapon,~%if your primary weighs more than six pounds.~%"))
      (t
       ;; dual wield
       (perform-wear ch obj +wear-wield-2+)))))

(defcommand (ch "hold") (:resting)
  (send-to-char ch "Hold what?"))

(defcommand (ch "hold" thing) (:resting)
  (let* ((objs (resolve-alias ch thing (carrying-of ch)))
         (obj (first objs)))
    (cond
      ((null objs)
       (send-to-char ch "You don't seem to have ~a ~a.~%"
                     (a-or-an thing)
                     thing))
      ((cdr objs)
       (send-to-char ch "You can only hold one thing at a time!~%"))
      ((is-obj-kind obj +item-light+)
       (perform-hold-light ch obj))
      (t
       (perform-hold ch obj)))))

(defcommand (ch "attach") (:resting)
  (send-to-char ch "Attach what to what?~%"))

(defcommand (ch "attach" thing) (:resting)
  (declare (ignore thing))
  (send-to-char ch "Attach what to what?~%"))

(defcommand (ch "attach" thing "to" to-thing) (:resting)
  (let ((objs (resolve-alias ch thing (carrying-of ch)))
        (to-objs (resolve-alias ch to-thing (append
                                                          (carrying-of ch)
                                                          (contents-of (in-room-of ch))))))
    (cond
      ((null objs)
       (send-to-char ch "You don't seem to have ~a ~a.~%"
                     (a-or-an thing)
                     thing))
      ((null to-objs)
       (send-to-char ch "You don't see any '~a'.~%" to-thing))
      ((cdr objs)
       (send-to-char ch "You can only attach one thing at a time.~%"))
      ((cdr to-objs)
       (send-to-char ch "You can only attach to one thing at a time.~%"))
      (t
       (perform-attach ch (first objs) (first to-objs))))))

(defcommand (ch "detach") (:resting)
  (send-to-char ch "Detach what from what?~%"))

(defcommand (ch "detach" thing) (:resting)
  (declare (ignore thing))
  (send-to-char ch "Detach what from what?~%"))

(defcommand (ch "detach" thing "from" from-thing) (:resting)
  (let ((objs (resolve-alias ch thing (carrying-of ch)))
        (from-objs (resolve-alias ch from-thing (append
                                                          (carrying-of ch)
                                                          (contents-of (in-room-of ch))))))
    (cond
      ((null objs)
       (send-to-char ch "You don't seem to have ~a ~a.~%"
                     (a-or-an thing)
                     thing))
      ((null from-objs)
       (send-to-char ch "You don't see any '~a'.~%" from-thing))
      ((cdr objs)
       (send-to-char ch "You can only detach one thing at a time.~%"))
      ((cdr from-objs)
       (send-to-char ch "You can only detach to one thing at a time.~%"))
      (t
       (perform-detach ch (first objs) (first from-objs))))))

(defcommand (ch "conceal") (:resting)
  (send-to-char ch "Conceal what?~%"))

(defcommand (ch "conceal" thing) (:resting)
  (let* ((locations (append (carrying-of ch)
                            (coerce (remove nil (equipment-of ch)) 'list)
                            (contents-of (in-room-of ch))))
         (objs (resolve-alias ch thing locations))
         (obj (first objs)))
    (cond
      ((null objs)
       (send-to-char ch "You don't seem to have ~a ~a.~%"
                     (a-or-an thing)
                     thing))
      ((rest objs)
       (send-to-char ch "You can conceal only one thing at a time!~%"))
      (t
       (perform-conceal ch obj)))))

(defcommand (ch "sacrifice") (:resting)
  (send-to-char ch "Sacrifice what object?~%"))

(defcommand (ch "sacrifice" thing) (:resting)
  (let* ((objs (resolve-alias ch thing (contents-of (in-room-of ch))))
         (obj (first objs)))
    (cond
      ((null objs)
       (send-to-char ch "You don't seem to have ~a ~a.~%"
                     (a-or-an thing)
                     thing))
      ((rest objs)
       (send-to-char ch "You can sacrifice only one thing at a time!~%"))
      (t
       (perform-sacrifice ch obj)))))

(defcommand (ch "junk" thing) (:resting)
  (let* ((dot-mode (find-all-dots thing))
         (objs (resolve-alias ch thing (carrying-of ch))))
    (cond
      ((and objs (eql dot-mode :find-all))
       (send-to-char ch "Go to the dump if you want to junk EVERYTHING!~%"))
      (objs
       (perform-drop ch objs :junk))
      ((eql dot-mode :find-all)
       (send-to-char ch "You don't seem to be carrying anything.~%"))
      (t
       (send-to-char ch "You don't seem to have any ~as.~%" thing)))))

(defcommand (ch "donate" thing) (:resting)
  (let* ((dot-mode (find-all-dots thing))
         (objs (resolve-alias ch thing (carrying-of ch))))
    (cond
      ((and objs (eql dot-mode :find-all))
       (send-to-char ch "Go to the donation room if you want to donate EVERYTHING!~%"))
      (objs
       (perform-drop ch objs :donate))
      ((eql dot-mode :find-all)
       (send-to-char ch "You don't seem to be carrying anything.~%"))
      (t
       (send-to-char ch "You don't seem to have any ~as.~%" thing)))))

(defcommand (ch "empty" thing) (:resting)
  (let* ((objs (resolve-alias ch thing (carrying-of ch)))
         (obj (first objs)))
    (cond
      ((null objs)
       (send-to-char ch "You don't seem to have ~a ~a.~%"
                     (a-or-an thing)
                     thing))
      ((rest objs)
       (send-to-char ch "You can only empty one thing at a time!~%"))
      (t
       (perform-empty ch obj)))))

(defcommand (ch "empty" from-thing "into" to-thing) (:resting)
  (let* ((from-objs (resolve-alias ch from-thing (carrying-of ch)))
         (from-obj (first from-objs))
         (to-objs (resolve-alias ch to-thing (append
                                                     (carrying-of ch)
                                                     (contents-of (in-room-of ch)))))
         (to-obj (first to-objs)))
    (cond
      ((null from-obj)
       (send-to-char ch "You can't find ~a ~a.~%"
                     (a-or-an from-thing)
                     from-thing))
      ((null to-obj)
       (send-to-char ch "You can't find ~a ~a.~%"
                     (a-or-an to-thing)
                     to-thing))
      ((rest from-objs)
       (send-to-char ch "You can't empty from more than one container at a time!~%"))
      ((rest to-objs)
       (send-to-char ch "You can't empty into more than one container at a time!~%"))
      (t
       (perform-empty-into ch from-obj to-obj)))))
