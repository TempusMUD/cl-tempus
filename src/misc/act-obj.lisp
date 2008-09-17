(in-package #:tempus)

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
          (> (+ (carry-weight-of ch) (weight-of obj)) (can-carry-weight ch)))
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

(defun perform-get-from-room (ch obj display counter)
  (let ((retval nil))
    (cond
      ((can-take-obj ch obj t t)
       (obj-from-room obj)
       (obj-to-char obj ch)
       (setf retval t))
      ((> counter 1)
       (setf display t)
       (decf counter))
      (t
       (return-from perform-get-from-room nil)))

    (cond
      ((or (not display) (not (plusp counter)))
       nil)
      ((= counter 1)
       (act ch :item obj :all-emit "$n get$% $p."))
      (t
       (act ch :item obj :all-emit (format nil "$n get$% $p. (x~d)" counter))))

    retval))

(defun get-from-room (ch arg)
  (let ((dotmode (find-all-dots arg))
        (money-found nil)
        (quad-found nil)
        (sigil-found nil))
    (case dotmode
      (:find-indiv
       ;; 'get <item>'
       (let ((obj (get-obj-in-list-vis ch arg (contents-of (in-room-of ch)))))
         (cond
           ((null obj)
            (send-to-char ch "You don't see ~a ~a here.~%"
                          (a-or-an arg) arg))
           ((and (is-corpse obj)
                 (/= (corpse-idnum obj) (idnum-of ch))
                 (not (immortalp ch))
                 (eql (pk-style-of (zone-of (in-room-of ch))) +zone-neutral-pk+)
                 (not (is-npc ch))
                 (plusp (corpse-idnum obj)))
            (send-to-char ch "You can't take corpses in NPK zones!~%"))
           ((not (perform-get-from-room ch obj t 1))
            nil)
           ((is-obj-kind obj +item-money+)
            (setf money-found t))
           ((eql (vnum-of obj) +quad-vnum+)
            (setf quad-found t))
           ((and (not (zerop (sigil-idnum-of obj)))
                 (/= (sigil-idnum-of obj) (idnum-of ch)))
            (setf sigil-found t)))))
      (:find-alldot
       nil))))


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

(defcommand (ch "get" thing) (:resting)
    (cond
      ((>= (carry-items-of ch) (can-carry-items ch))
       (send-to-char ch "Your arms are already full!~%"))
      ((string= thing "")
       (send-to-char ch "Get what?~%"))
      (t
       (get-from-room ch thing))))

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
