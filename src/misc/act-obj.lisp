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

(defun check-object-nodrop (ch obj verb)
  "Checks to see if OBJ can be dropped by CH.  May emit messages to CH.  Returns T if the object may be dropped, otherwise returns NIL."
  (cond
    ((not (is-obj-stat obj +item-nodrop+))
     t)
    ((immortalp ch)
     (act ch :item obj :subject-emit "You peel $p off your hand...")
     t)
    (t
     (act ch :item obj
          :subject-emit (format nil "You can't ~a $p, it must be CURSED!" verb))
     nil)))

(defun perform-drop (ch obj mode sname dest-room displayp)
  (when (check-object-nodrop ch obj sname)
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
       (extract-obj obj)))
    t))

(defun find-eq-pos (ch obj)
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
                                                (strength-apply-index ch))
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
             (title-of (in-room-of vict)))))))


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

(defcommand (ch "put") (:resting)
  (send-to-char ch "Put what in what?~%"))

(defcommand (ch "put" thing) (:resting)
  (send-to-char ch "What do you want to put ~a in?~%"
                (if (eql (find-all-dots thing) :find-indiv) "it" "them")))

(defcommand (ch "put" thing "into" container-str) (:resting)
  (let* ((locations (append (carrying-of ch)
                            (coerce (remove nil (equipment-of ch)) 'list)
                            (contents-of (in-room-of ch))))
         (containers (get-matching-objects ch container-str locations))
         (container (first containers))
         (objs (get-matching-objects ch thing (carrying-of ch))))
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
               (cond
                 ((= counter 1)
                  (act ch :item obj :target-item container
                       :all-emit "$n put$% $p into $P."))
                 ((plusp counter)
                  (act ch :item obj
                       :target-item container
                       :all-emit (format nil "$n put$% $p into $P. (x~d)" counter))))
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
                  (get-matching-objects ch thing (carrying-of ch)))))
    (cond
      (objs
       (loop
          for obj-sublist on objs
          as obj = (first obj-sublist)
          as next-obj = (second obj-sublist)
          as counter from 1
          do
            (if (perform-drop ch obj :drop "drop" (in-room-of ch) nil)
                (when (or (null next-obj)
                          (string/= (name-of next-obj) (name-of obj)))
                  (cond
                    ((= counter 1)
                     (act ch :item obj :all-emit "$n drop$% $p."))
                    ((plusp counter)
                     (act ch :item obj
                          :all-emit (format nil "$n drop$% $p. (x~d)" counter))))
                  (setf counter 0))
                (setf counter 0))))
      ((eql dot-mode :find-all)
       (send-to-char ch "You don't seem to be carrying anything.~%"))
      (t
       (send-to-char ch "You don't seem to have any ~as.~%" thing)))))

(defcommand (ch "wear") (:resting)
  (send-to-char ch "What do you want to wear?~%"))

(defcommand (ch "wear" thing) (:resting)
  (let* ((objs (get-matching-objects ch thing (carrying-of ch)))
         (wear-objs (delete nil
                            (mapcar (lambda (obj)
                                      (let ((pos (find-eq-pos ch obj)))
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
          (objs (get-matching-objects ch thing (carrying-of ch))))
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

(defcommand (ch "remove") (:resting)
  (send-to-char ch "Remove what?~%"))

(defcommand (ch "remove" thing) (:resting)
  (let* ((objs (get-matching-objects ch thing
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
      ((not (is-name thing (aliases-of obj)))
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
  (let* ((objs (get-matching-objects ch thing (carrying-of ch)))
         (victs (get-matching-objects ch target (people-of (in-room-of ch))))
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
          (perform-give ch (first victs) obj)
          (when (or (null next-obj)
                    (string/= (name-of next-obj) (name-of obj)))
            (cond
              ((= counter 1)
               (act ch :target vict :item obj
                    :all-emit "$n give$% $p to $N."))
              ((plusp counter)
               (act ch :target vict :item obj
                    :all-emit (format nil "$n give$% $p to $N. (x~d)" counter))))
            (setf counter 0))))
      ((eql mode :find-indiv)
       (send-to-char ch "You aren't carrying ~a ~a.~%" (a-or-an thing) thing))
      ((eql mode :find-all)
       (send-to-char ch "You aren't carrying anything.~%"))
      (t
       (send-to-char ch "You aren't carrying any ~as.~%" (a-or-an thing) thing)))))
