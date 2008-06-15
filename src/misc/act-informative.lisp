(in-package #:tempus)

(defparameter +dirs+ #("n" "e" "s" "w" "u" "d" "f" "p"))
(defparameter +num-of-dirs+ (length +dirs+))

(defun do-auto-exits (ch room)
  (send-to-char ch "&c[ Exits: ~:[None obvious~;~:*~{~a~^ ~}~] ]"
                (loop for door across (dir-option-of room)
                     for dir across +dirs+
                   unless (or (null door)
                              (zerop (to-room-of door))
                              (logtest (exit-info-of door)
                                       (logior +ex-hidden+ +ex-secret+))) 
                   collect (if (logtest (exit-info-of door)
                                        (logior +ex-closed+))
                               (format nil "|~a|" dir)
                               (format nil "~a" dir))))
  (when (immortalp ch)
    (send-to-char ch " [ Hidden doors: ~:[None~;~:*~{~a~^ ~}~] ]"
                  (loop for door across (dir-option-of room)
                     for dir across +dirs+
                     unless (or (null door)
                                (zerop (to-room-of door))
                                (not (logtest (exit-info-of door)
                                              (logior +ex-hidden+ +ex-secret+))))
                     collect (if (logtest (exit-info-of door)
                                          (logior +ex-closed+))
                                 (format nil "|~a|" dir)
                                 (format nil "~a" dir))))))

(defun show-room-obj (object ch stream)
  (cond
    ((line-desc-of object)
     (princ (line-desc-of object) stream))
    ((immortalp ch)
     (format stream "~a exists here."
             (string-upcase (name-of object) :end 1)))))

(defun show-obj-bits (object ch stream)
  nil)

(defun show-obj-extra (object ch stream)
  nil)

(defun show-obj-to-char (stream object ch mode count)
    (cond
      ((eql mode :room)
       (show-room-obj object ch stream))
      ((and (or (eql mode :inv)
                (eql mode :content))
            (name-of object))
       (princ (name-of object) stream))
      ((eql mode :extra)
       (show-obj-extra object ch stream)))

    (unless (eql mode :nobits)
      (show-obj-bits object ch stream))

    (when (> count 1)
      (format stream " [~d]" count))
    (format stream "~%")

    (when (and (= (kind-of object) +item-vehicle+)
               (eql mode :bits)
               (car-openable object))
      (format stream "The door of ~a is ~a."
              (describe object ch)
              (if (car-closed object) "closed" "open"))))

(defun list-obj-to-char (stream obj-list ch mode show)
  (let ((corpse (and obj-list
                     (in-obj-of (first obj-list))
                     (is-corpse (first obj-list))))
        (found nil))
    (loop with o = obj-list
       for i = (car obj-list) then (car o)
       while i do
       (cond
         ((or (not (can-see-object ch i))
              (is-soilage i)
              (and (is-obj-stat2 i +item2-hidden+)
                   (not (pref-flagged ch +pref-holylight+))
                   (> (random-range 50 120) (hidden-obj-prob ch i))))
          nil)
         ((and corpse
               (is-implant i)
               (not (can-wear i +item-wear-take+))
               (not (pref-flagged ch +pref-holylight+))
               (< (+ (check-skill ch +skill-cyberscan+)
                     (if (aff3-flagged ch +aff3-sonic-imagery+) 50 0))
                  (random-range 80 150)))
          nil)
         ((or (and (proto-of (shared-of i))
                   (string/= (name-of i) (name-of (proto-of (shared-of i)))))
              (is-obj-stat2 i +item2-broken+))
          (setf o (cdr o))
          (setf found t)
          (show-obj-to-char stream i ch mode 1))
         (t
          (setf found t)
          (setf o (cdr o))
          (show-obj-to-char stream i ch mode
                            (1+ (loop while (and o (same-obj (car o) i))
                                   do (setf o (cdr o))
                                   count (can-see-object ch o)))))))
    (when (and (not found) show)
      (format stream " Nothing.~%"))))

(defun desc-one-char (stream ch i is-group)
  (let ((positions #(" is lying here, dead."
                     " is lying here, badly wounded."
                     " is lying here, incapacitated."
                     " is lying here, stunned."
                     " is sleeping here."
                     " is resting here."
                     "!FIGHTING!"
                     " is standing here."
                     " is hovering here."
                     "!MOUNTED!"
                     " is swimming here.")))
    (unless (or (aff2-flagged i +aff2-mounted+)
                (and (not (is-npc ch))
                     (mob2-flagged i +mob2-unapproved+)
                     (not (pref-flagged ch +pref-holylight+))
                     (not (tester-p ch))))
      (let ((name (cond
                    ((is-npc i)
                     (short-descr-of i))
                    ((affected-by-spell i +skill-disguise+)
                     (string-upcase (get-disguised-name ch i) :end 1))
                    (t
                     (concatenate 'string
                                  (string-upcase (name-of i) :end 1)
                                  (title-of i))))))
        (format stream "~a" (if is-group "&Y" "&y"))
        (cond
          ((and (is-npc i)
                (= (position-of i) (default-pos-of (shared-of i))))
           (if (long-descr-of i)
               (format stream "~a" (long-descr-of i))
               (format stream "~a exists here." name)))
          ((= (position-of i) +pos-fighting+)
           (cond
             ((null (fighting-of i))
              (format stream "~a is here, fighting thin air!" name))
             ((eql (fighting-of i) ch)
              (format stream "~a is here, fighting YOU!" name))
             ((eql (in-room-of (fighting-of i)) (in-room-of i))
              (format stream "~a is here, fighting ~a!"
                      name (describe-char ch (fighting-of i))))
             (t
              (format stream "~a is here, fighting someone who already left!" name))))
          ((= (position-of i) +pos-mounted+)
           (cond
             ((null (mounted-of i))
              (format stream "~a is here, mounted on thin air!" name))
             ((eql (mounted-of i) ch)
              (format stream "~a is here, mounted on YOU.  Heh heh..." name))
             ((eql (in-room-of (mounted-of i)) (in-room-of i))
              (format stream "~a is here, mounted on ~a."
                      name (describe-char ch (mounted-of i))))
             (t
              (format stream "~a is here, mounted on someone who already left!" name))))
          ((and (aff2-flagged i +aff2-meditate+)
                (= (position-of i) +pos-sitting+))
           (format stream "~a is meditating here." name))
          ((aff-flagged i +aff-hide+)
           (format stream "~a is hiding here." name))
          ((and (aff3-flagged i +aff3-stasis+)
                (= (position-of i) +pos-sleeping+))
           (format stream "~a is lying here in a static state." name))
          ((and (member (terrain-of (in-room-of i)) (list +sect-water-noswim+
                                                          +sect-water-swim+
                                                          +sect-fire-river+))
                (or (not (aff-flagged i +aff-waterwalk+))
                    (< (position-of i) +pos-standing+)))
           (format stream "~a is swimming here." name))
          ((and (room-is-underwater (in-room-of i))
                (> (position-of i) +pos-resting+))
           (format stream "~a is swimming here." name))
          ((and (= (terrain-of (in-room-of i)) +sect-pitch-pit+)
                (< (position-of i) +pos-flying+))
           (format stream "~a struggles in the pitch." name))
          ((= (terrain-of (in-room-of i)) +sect-pitch-sub+)
           (format stream "~a struggles blindly in the pitch." name))
          (t
           (format stream "~a~a" name (aref positions (position-of i)))))
        
        ;; Show alignment flags
        (cond
          ((pref-flagged ch +pref-holylight+)
           (format stream " ~a(~da)"
                   (cond ((is-evil i) "&R")
                         ((is-good i) "&B")
                         (t           "&W"))
                   (alignment-of i)))
          ((or (aff-flagged ch +aff-detect-align+)
               (and (is-cleric ch) (aff2-flagged ch +aff2-true-seeing+)))
           (cond
             ((is-evil i)
              (format stream "&R(Red Aura)"))
             ((is-good i)
              (format stream "&B(Blue Aura)")))))
        
        (when (and (aff3-flagged ch +aff3-detect-poison+)
                   (or (has-poison-1 i)
                       (has-poison-2 i)
                       (has-poison-3 i)))
          (format stream " &g(poisoned)"))

        (when (mob-flagged i +mob-utility+)
          (format stream " &c<util>"))
        (when (mob2-flagged i +mob2-unapproved+)
          (format stream " &r(!appr)"))
        (when (and (is-npc i)
                   (or (immortalp ch) (testerp ch))
                   (pref-flagged ch +pref-disp-vnums+))
          (format stream " &g<&n~d&n>" (vnum-of (shared-of i))))

        (unless (is-npc i)
          (unless (null (link-of i))
            (format stream " &m(linkless)&y"))
          (when (plr-flagged i +plr-writing+)
            (format stream " &g(writing)&y"))
          (when (plr-flagged i +plr-olc+)
            (format stream " &g(creating)&y"))
          (when (plr-flagged i +plr-afk+)
            (if (afk-reason-of i)
                (format stream " &g(afk: ~a)&y" (afk-reason-of i))
                (format stream " &g(afk)&y"))))

        (format stream "~%")))))
      

(defun char-list-desc (people ch)
  "Returns two values, a string containing the description of all the seen creatures in PEOPLE, and the number of unseen creatures.  This function may send another creature notification that they've been seen."
  (let ((unseen 0))
    (values
     (with-output-to-string (stream)
       (dolist (i people)
         (cond
           ((eql ch i)
            ;; You don't see yourself in creature lists
            nil)
           ((and (not (eql (in-room-of ch) (in-room-of i)))
                 (aff-flagged i (logior +aff-hide+ +aff-sneak+))
                 (not (pref-flagged ch +pref-holylight+)))
            ;; You don't see hiding or sneaking people in other rooms
            nil)
           ((and (room-is-dark (in-room-of ch))
                 (not (has-dark-sight ch))
                 (aff-flagged i +aff-infravision+))
            ;; You might see creatures with infravision
            (case (random-range 0 2)
              (0
               (format stream 
                       "You see a pair of glowing red eyes looking your way.~%"))
              (1
               (format stream 
                       "A pair of eyes glow red in the darkness~%"))
              (2
               (incf unseen))))
         ((and (not (immortalp ch))
               (is-npc i)
               (null (long-descr-of i)))
          ;; You don't see mobs with no ldesc
          nil)
         ((not (can-see-creature ch i))
          ;; You don't see creatures that you can't see (duh)
          (unless (or (immortalp i) (mob-flagged i +mob-utility+))
            ;; ... and you don't see utility mobs unless you're immortal
            (incf unseen))
          nil)
         ((and (aff-flagged i +aff-hide+)
               (not (aff3-flagged ch +aff3-sonic-imagery+))
               (not (pref-flagged ch +pref-holylight+)))
          ;; You might not see hiding creatures
          (let ((hide-prob (random-range 0 (get-level-bonus i +skill-hide+)))
                (hide-roll (+ (random-range 0 (get-level-bonus ch nil))
                              (if (affected-by-spell ch +zen-awareness+)
                                  (floor (get-level-bonus ch +zen-awareness+) 4)
                                  0))))
            (cond
              ((> hide-prob hide-roll)
               (incf unseen))
              (t
               (when (can-see-creature i ch)
                 (act i :target ch :subject-emit "$N seems to have seen you.~%"))
               (desc-one-char stream ch i (in-same-group-p ch i))))))
         (t
          ;; You can see everyone else
          (desc-one-char stream ch i (in-same-group-p ch i))))))
    unseen)))

(defun list-char-to-char (stream people ch)
  (unless people
    (return-from list-char-to-char))

  (multiple-value-bind (str unseen)
      (char-list-desc people ch)
    (when (and (plusp unseen)
               (or (aff-flagged ch +aff-sense-life+)
                   (affected-by-spell ch +skill-hyperscan+)))
      (cond
        ((= unseen 1)
         (format stream "&mYou sense an unseen presence.&n~%"))
        ((< unseen 4)
         (format stream "&mYou sense a few unseen presences.&n~%"))
        ((< unseen 7)
         (format stream "&mYou sense many unseen presences.&n~%"))
        (t
         (format stream "&mYou sense a crowd of unseen presences.&n~%"))))
    (format stream "~a" str)))

(defun look-at-room (ch room ignore-brief)
  (unless (link-of ch)
    (return-from look-at-room))

  (when (and (room-is-dark ch) (not (has-dark-sight ch)))
    (send-to-char ch "It is pitch black...~%")
    (return-from look-at-room))

  (if (pref-flagged ch +pref-roomflags+)
      (progn
        (send-to-char ch "&c[~5d] ~a [ ~a ] [ ~a ]"
                      (number-of room)
                      (name-of room)
                      (if (zerop (flags-of room))
                          "NONE"
                          (printbits (flags-of room) +room-bits+))
                      (aref +sector-types+ (terrain-of room)))
        (when (max-occupancy-of room)
          (send-to-char ch " [ Max: ~d ]" (max-occupancy-of room)))

        #+nil        (let ((house (find-house-by-room (number-of room))))
                       (when house
                         (send-to-char ch " [ House: ~d ]" (id-of house)))))
      (send-to-char ch "&c~a" (name-of room)))

  (send-to-char ch "&n~%")

  (when (or (not (pref-flagged ch +pref-brief+))
            ignore-brief
            (room-flagged room +room-death+))
    (if (and (room-flagged room +room-smoke-filled+)
             (not (pref-flagged ch +pref-holylight+))
             #+nil (not (aff3-flagged ch +aff3-sonic-imagery+)))
        (send-to-char ch "The smoke swirls around you...~%")
        (when (description-of room)
          (send-to-char ch "~a" (description-of room)))))

  (case (pk-style-of (zone-of room))
    (0
     (send-to-char ch "&c[ &g!PK&c ] "))
    (1
     (send-to-char ch "&c[ &YNPK&c ] "))
    (2
     (send-to-char ch "&c[ &RCPK&c ] ")))

  (unless (or (immortalp ch)
              (not (room-flagged room +room-smoke-filled+))
              #+nil (aff3-flagged ch +aff3-sonic-imagery+))
    (send-to-char ch "~%")
    (return-from look-at-room))

  ;; autoexits
  (when (pref-flagged ch +pref-autoexit+)
      (do-auto-exits ch room))
  (send-to-char ch "~%")

  ;; now list characters & objects
  (let ((blood-shown nil)
        (ice-shown nil))
    (dolist (o (contents-of room))
      (when (and (= (vnum-of (shared-of o)) +blood-vnum+)
                 (not blood-shown)
                 (send-to-char ch "&r~a.&n~%"
                               (cond
                                 ((< (timer-of o) 10)
                                  "Some spots of blood have been splattered around")
                                 ((< (timer-of o) 20)
                                  "Small pools of blood are here")
                                 ((< (timer-of o) 30)
                                  "Large pools of blood are here")
                                 ((< (timer-of o) 40)
                                  "Blood is pooled and splattered over everything")
                                 (t
                                  "Dark red blood covers everything in sight.")))
                 (setf blood-shown t)))
      (when (and (= (vnum-of (shared-of o)) +ice-vnum+)
                 (not ice-shown))
        (send-to-char ch "&r~a.&n~%"
                      (cond
                        ((< (timer-of o) 10)
                         "A few patches of ice are scattered around")
                        ((< (timer-of o) 20)
                         "A thin coating of ice covers everything")
                        ((< (timer-of o) 30)
                         "A thick coating of ice covers everything")
                        (t
                         "Everything is covered with a thick coating of ice")))
        (setf ice-shown t))))
  (send-to-char ch "&g~a&n"
                (with-output-to-string (s)
                  (list-obj-to-char s (contents-of room) ch :room nil)))
  (send-to-char ch "&y~a&n"
                (with-output-to-string (s)
                  (list-char-to-char s (people-of room) ch))))
    
