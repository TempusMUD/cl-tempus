(in-package #:tempus)

(defun show-room-obj (object ch stream count)
  (let ((non-blank-ldesc (string/= (line-desc-of object) "")))
    (when (or non-blank-ldesc (immortalp ch))
      (princ "&g" stream)
      (if non-blank-ldesc
          (princ (line-desc-of object) stream)
          (format stream "~a exists here."
                  (string-upcase (name-of object) :end 1)))
      (show-obj-bits object ch stream)
      (when (> count 1)
        (format stream "&g [~d]&n" count))
      (format stream "~%"))))

(defun show-obj-bits (object ch stream)
  (when (is-obj-stat2 object +item2-broken+)
    (format stream " &n<broken>"))
  (when (and (in-obj-of object)
             (is-corpse (in-obj-of object))
             (is-implant object))
    (format stream " (implanted)"))
  (when (or (carried-by-of object) (worn-by-of object))
    (when (is-obj-stat2 object +item2-reinforced+)
      (format stream " &y[&nreinforced&y]&n"))
    (when (is-obj-stat2 object +item2-enhanced+)
      (format stream " &m|enhanced|&n")))
  (when (and (is-obj-kind object +item-device+)
             (plusp (engine-state object)))
    (format stream " (active)"))

  (when (or (and (or (is-obj-kind object +item-cigarette+)
                     (is-obj-kind object +item-pipe+))
                 (plusp (aref (value-of object) 3)))
            (and (is-obj-kind object +item-bomb+)
                 (contains-of object)
                 (is-obj-kind (first (contains-of object)) +item-fuse+)
                 (plusp (fuse-state (first (contains-of object))))))
    (format stream " (lit)"))
  (when (is-obj-stat object +item-invisible+)
    (format stream " &c(invisible)&n"))
  (when (is-obj-stat object +item-transparent+)
    (format stream " &c(transparent)&n"))
  (when (is-obj-stat2 object +item2-hidden+)
    (format stream " &r(hidden)&n"))
  (when (or (and (aff3-flagged ch +aff3-detect-poison+)
                 (or (is-obj-kind object +item-food+)
                     (is-obj-kind object +item-drinkcon+)
                     (is-obj-kind object +item-fountain+))
                 (plusp (aref (value-of object) 3)))
            (affected-by-spell object +spell-envenom+))
    (format stream " &g(poisoned)&n"))
  (when (or (aff-flagged ch +aff-detect-align+)
            (and (is-cleric ch)
                 (aff2-flagged ch +aff2-true-seeing+)))
    (when (is-obj-stat object +item-bless+)
      (format stream " &B(holy aura)&n"))
    (when (is-obj-stat object +item-damned+)
      (format stream " &B(unholy aura)&n")))
  (when (and (or (aff-flagged ch +aff-detect-magic+)
                 (aff2-flagged ch +aff2-true-seeing+))
             (is-obj-stat object +item-magic+))
    (format stream " &Y(yellow aura)&n"))
  (when (and (or (aff-flagged ch +aff-detect-magic+)
                 (aff2-flagged ch +aff2-true-seeing+)
                 (pref-flagged ch +pref-holylight+))
             (plusp (sigil-idnum-of object)))
    (format stream "&y(&msigil&y)&n"))
  (when (is-obj-stat object +item-glow+)
    (format stream " &g(glowing)&n"))
  (when (is-obj-stat object +item-hum+)
    (format stream " &r(humming)&n"))
  (when (is-obj-stat2 object +item2-ablaze+)
    (format stream " &R*burning*&n"))
  (when (is-obj-stat3 object +item3-hunted+)
    (format stream " &r(hunted)&n"))
  (when (obj-soiled object +soil-blood+)
    (format stream " &r(bloody)&n"))
  (when (obj-soiled object +soil-water+)
    (format stream " &c(wet)&n"))
  (when (obj-soiled object +soil-mud+)
    (format stream " &y(muddy)&n"))
  (when (obj-soiled object +soil-acid+)
    (format stream " &g(acid covered)&n"))
  (when (plusp (owner-id-of (shared-of object)))
    (format stream " &Y(protected)&n"))
  (when (tmp-affects-of object)
    (when (affected-by-spell object +spell-item-repulsion-field+)
      (format stream " &c(repulsive)&n"))
    (when (affected-by-spell object +spell-item-attraction-field+)
      (format stream " &c(attractive)&n"))
    (when (affected-by-spell object +spell-elemental-brand+)
      (format stream " &r(branded)&n")))
  (when (pref-flagged ch +pref-disp-vnums+)
    (format stream " &y<&n~a&y>&n"
            (vnum-of (shared-of object)))))

(defun show-obj-extra (object ch stream)
  nil)

(defun show-obj-to-char (stream object ch mode count)
    (cond
      ((eql mode :room)
       (show-room-obj object ch stream count)
       (return-from show-obj-to-char))
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

    (when (and (is-obj-kind object +item-vehicle+)
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

(defun desc-char-trailers (stream ch i)
  (format stream "&n")
  (when (affected-by-spell i +spell-quad-damage+)
    (format stream "...~a is glowing with a bright blue light!~%"
                  (he-or-she i)))
  (when (aff2-flagged i +aff2-ablaze+)
    (format stream "...~a body is blazing like a bonfire!~%"
                  (his-or-her i)))
  (when (aff-flagged i +aff-blind+)
    (format stream "...~a is groping around blindly!~%"
                  (he-or-she i)))
  (when (aff-flagged i +aff-sanctuary+)
    (format stream
                  (if (is-evil i)
                      "...~a is surrounded by darkness!~%"
                      "...~a glows with a bright light!~%")
                  (he-or-she i)))
  (when (aff-flagged i +aff-confusion+)
    (format stream "...~a is looking around in confusion!~%"
                  (he-or-she i)))
  (when (aff3-flagged i +aff3-symbol-of-pain+)
    (format stream "...a symbol of pain burns bright on ~a forehead!~%"
                  (his-or-her i)))
  (when (aff-flagged i +aff-blur+)
    (format stream "...~a form appears to be blurry and shifting.~%"
                  (his-or-her i)))
  (when (aff2-flagged i +aff2-fire-shield+)
    (format stream "...a blazing sheet of fire floats before ~a body!~%"
                  (his-or-her i)))
  (when (aff2-flagged i +aff2-blade-barrier+)
    (format stream "...~a is surrounded by whirling blades!~%"
                  (he-or-she i)))
  (when (aff2-flagged i +aff2-energy-field+)
    (format stream "...~a is covered by a crackling field of energy!~%"
                  (he-or-she i)))
  (cond
    ((is-soulless i)
     (format stream "...a deep red pentagram has been burned into ~a forehead!~%"
             (his-or-her i)))
    ((aff3-flagged i +aff3-tainted+)
     (format stream "...the mark of the tainted has been burned into ~a forehead!~%"
             (his-or-her i))))

  (when (aff3-flagged i +aff3-prismatic-sphere+)
    (format stream "...~a is surrounded by a prismatic sphere of light!~%"
                  (he-or-she i)))
  (when (or (affected-by-spell i +spell-skunk-stench+)
            (affected-by-spell i +spell-trog-stench+))
    (format stream "...~a is followed by a malodorous stench...~%"
                  (he-or-she i)))
  (when (aff2-flagged i +aff2-petrified+)
    (format stream "...~a is petrified into solid stone.~%"
                  (he-or-she i)))
  (when (affected-by-spell i +spell-entangle+)
    (if (or (= (terrain-of (in-room-of i)) +sect-city+)
            (= (terrain-of (in-room-of i)) +sect-cracked-road+))
        (format stream "...~a is hopelessly tangled in the weeds and sparse vegetation.~%"
                (he-or-she i))
        (format stream "...~a is hopelessly tangled in the undergrowth.~%"
                (he-or-she i))))
  (when (and (aff2-flagged i +aff2-displacement+)
             (aff2-flagged ch +aff2-true-seeing+))
    (format stream "...the image of ~a body is strangely displaced.~%"
            (his-or-her i)))
  (when (aff-flagged i +aff-invisible+)
        (format stream "...~a is invisible to the unaided eye.~%"
                (he-or-she i)))
  (when (aff2-flagged i +aff2-transparent+)
        (format stream "...~a is completely transparent.~%"
                (he-or-she i)))
  (when (and (affected-by-spell i +skill-kata+)
             (>= (get-level-bonus i +skill-kata+) 50))
        (format stream "...~a hands are glowing eerily.~%"
                (his-or-her i)))
  (when (affected-by-spell i +spell-gauss-shield+)
    (format stream "...~a is protected by a swirling shield of energy.~%"
            (he-or-she i)))
  (when (affected-by-spell i +spell-thorn-skin+)
    (format stream "...thorns protrude painfully from ~a skin.~%"
            (his-or-her i)))
  (when (affected-by-spell i +song-wounding-whispers+)
    (format stream "...~a is surrounded by whirling slivers of sound.~%"
            (he-or-she i)))
  (when (affected-by-spell i +song-mirror-image-melody+)
    (format stream "...~a is surrounded by mirror images.~%"
            (he-or-she i)))
  (when (affected-by-spell i +spell-dimensional-shift+)
    (format stream "...~a is shifted into a parallel dimension.~%"
            (he-or-she i))))


(defun diag-char-to-char (stream ch i)
  (let ((percent (if (plusp (max-hitp-of i))
                     (floor (* 100 (hitp-of i)) (max-hitp-of i))
                     -1)))
    (format stream "&y~a ~a.&n~%"
                  (desc ch i)
                  (cond
                    ((>= percent 100) "is in excellent condition")
                    ((>= percent 90) "has a few scratches")
                    ((>= percent 75) "has some small wounds and bruises")
                    ((>= percent 50) "has quite a few wounds")
                    ((>= percent 30) "has some big nasty wounds and scratches")
                    ((>= percent 15) "looks pretty hurt")
                    ((>= percent 5) "is in awful condition")
                    ((>= percent 0) "is on the brink of death")
                    (t "is bleeding awfully from big wounds")))))

(defun look-at-char (ch i mode)
  (let* ((af (affected-by-spell i +skill-disguise+))
         (mob (when af (real-mobile-proto (modifier-of af))))
         (desc (if mob (fdesc-of mob) (fdesc-of i))))
    (unless (eql mode :glance)
      (if desc
         (send-to-char ch "~a" desc)
         (send-to-char ch "You see nothing special about ~a.~%" (him-or-her i)))
      (when (and (null mob) (typep i 'player))
          (send-to-char ch "~a appears to be a ~d cm tall, ~d pound ~(~a ~a~).~%"
                        (name-of i)
                        (height-of i)
                        (weight-of i)
                        (sex-of i)
                        (aref +player-races+ (race-of i))))))

  (send-to-char ch "~a"
                (with-output-to-string (s) (diag-char-to-char s ch i)))

  (when (eql mode :look)
    (send-to-char ch "~a"
                  (with-output-to-string (s) (desc-char-trailers s ch i))))

  ;; Describe soilage
  (when (and (not (eql mode :glance)) (typep i 'player))
    (loop
       for idx upto +num-wears+
       as pos = (aref +eq-pos-order+ idx)
       when (and (null (aref (equipment-of i) pos))
                 (not (illegal-soilpos pos))
                 (not (zerop (char-soilage i pos))))
       do (let ((soilage-descs
                        (loop for bit upto +top-soil+
                             when (char-soiled i pos bit)
                             collect (aref +soilage-bits+ bit))))
            (send-to-char ch "~a ~a ~a~v[~;~{~a~}~;~{~a and ~a~}~:;~{~#[~; and~] ~a~^,~}~].~%"
                          (his-or-her i)
                          (aref +wear-descriptions+ pos)
                          (if (= pos +wear-feet+)
                              "are"
                              (is-are (aref +wear-descriptions+ pos)))
                          (length soilage-descs)
                          soilage-descs)))))

(defun desc-one-char (stream ch i is-group)
  (let ((positions #(" is lying here, dead."
                     " is lying here, badly wounded."
                     " is lying here, incapacitated."
                     " is lying here, stunned."
                     " is sleeping here."
                     " is resting here."
                     " is sitting here."
                     "!FIGHTING!"
                     " is standing here."
                     " is hovering here."
                     "!MOUNTED!"
                     " is swimming here.")))
    (unless (or (aff2-flagged i +aff2-mounted+)
                (and (not (is-npc ch))
                     (mob2-flagged i +mob2-unapproved+)
                     (not (pref-flagged ch +pref-holylight+))
                     (not (testerp ch))))
      (let ((name (cond
                    ((is-npc i)
                     (name-of i))
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
           (if (string= (ldesc-of i) "")
               (format stream "~a exists here." name)
               (format stream "~a" (ldesc-of i))))
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
          (format stream " &g<&n~d&g>" (vnum-of (shared-of i))))

        (unless (is-npc i)
          (unless (link-of i)
            (format stream " &m(linkless)&y"))
          (when (plr-flagged i +plr-writing+)
            (format stream " &g(writing)&y"))
          (when (plr-flagged i +plr-olc+)
            (format stream " &g(creating)&y"))
          (when (plr-flagged i +plr-afk+)
            (if (afk-reason-of i)
                (format stream " &g(afk: ~a)&y" (afk-reason-of i))
                (format stream " &g(afk)&y"))))

        (format stream "~%")

        (unless (pref-flagged ch +pref-notrailers+)
          (desc-char-trailers stream ch i))))))


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
               (null (ldesc-of i)))
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

(defun do-auto-exits (ch room)
  (let ((+dir-letters+ #("n" "e" "s" "w" "u" "d" "f" "p")))
    (send-to-char ch "&c[ Exits: ~:[None obvious~;~:*~{~a~^ ~}~] ]"
                  (loop for door across (dir-option-of room)
                     for dir across +dir-letters+
                     unless (or (null door)
                                (zerop (to-room-of door))
                                (null (real-room (to-room-of door)))
                                (logtest (exit-info-of door)
                                         (logior +ex-hidden+ +ex-secret+)))
                     collect (if (logtest (exit-info-of door) +ex-closed+)
                                 (format nil "|~a|" dir)
                                 (format nil "~a" dir))))
    (when (immortalp ch)
      (send-to-char ch " [ Hidden doors: ~:[None~;~:*~{~a~^ ~}~] ]"
                    (loop for door across (dir-option-of room)
                       for dir across +dir-letters+
                       unless (or (null door)
                                  (zerop (to-room-of door))
                                  (not (logtest (exit-info-of door)
                                                (logior +ex-hidden+ +ex-secret+))))
                       collect (if (logtest (exit-info-of door)
                                            (logior +ex-closed+))
                                   (format nil "|~a|" dir)
                                   (format nil "~a" dir)))))))

(defun show-blood-and-ice (ch room)
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
        (setf ice-shown t)))))

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
             (not (aff3-flagged ch +aff3-sonic-imagery+)))
        (send-to-char ch "The smoke swirls around you...~%")
        (when (description-of room)
          (send-to-char ch "~a" (description-of room)))))

  (send-to-char ch
                (case (pk-style-of (zone-of room))
                  (0 "&c[ &g!PK&c ] ")
                  (1 "&c[ &YNPK&c ] ")
                  (2 "&c[ &RCPK&c ] ")))

  (unless (or (immortalp ch)
              (not (room-flagged room +room-smoke-filled+))
              (aff3-flagged ch +aff3-sonic-imagery+))
    (send-to-char ch "~%")
    (return-from look-at-room))

  ;; autoexits
  (when (pref-flagged ch +pref-autoexit+)
      (do-auto-exits ch room))
  (send-to-char ch "~%")

  ;; now list characters & objects
  (show-blood-and-ice ch room)
  (send-to-char ch "&g~a&n"
                (with-output-to-string (s)
                  (list-obj-to-char s (contents-of room) ch :room nil)))
  (send-to-char ch "&y~a&n"
                (with-output-to-string (s)
                  (list-char-to-char s (people-of room) ch))))

(defun list-equipment (stream ch mode show-empty-p)
    (when show-empty-p
      (format stream "You are using:~%"))
    (loop
       with found = nil
       with pos-array = (case mode
                         (:equipment (equipment-of ch))
                         (:tattoos (tattoos-of ch)))
       with pos-descs = (case mode
                         (:equipment +eq-pos-descs+)
                         (:tattoos +tattoo-pos-descs+))
       for idx from 0 upto +num-wears+
       as pos = (aref +eq-pos-order+ idx)
       as obj = (aref pos-array pos)
       do
         (cond
           ((null obj)
            (when (and show-empty-p (/= pos +wear-ass+))
              (format stream "~aNothing!~%" (aref pos-descs pos))))
           ((can-see-object ch obj)
            (unless (or found show-empty-p)
              (format stream "You are using:~%"))
            (setf found t)
            (format stream "&g~a&n" (aref pos-descs pos))
            (show-obj-to-char stream obj ch :inv 0))
           (t
            (unless (or found show-empty-p)
              (format stream "You are using:~%"))
            (setf found t)
            (format stream "~aSomething" (aref pos-descs pos))))
       finally
         (unless (or found show-empty-p)
           (format stream "You're totally naked!~%"))))

(defcommand (ch "equipment") (:sleeping)
  (send-to-char ch "~a"
                (with-output-to-string (s)
                  (list-equipment s ch :equipment nil))))

(defcommand (ch "equipment" "all") (:sleeping)
  (send-to-char ch "~a"
                (with-output-to-string (s)
                  (list-equipment s ch :equipment t))))

(defun parse-who-args (args)
  (let ((options nil))
    (dolist (term (cl-ppcre:split #/\s+/ args))
      (string-case term
        ("zone"
         (push :zone options))
        ("plane"
         (push :plane options))
        ("time"
         (push :time options))
        ("kills"
         (push :kills options))
        ("noflags"
         (push :noflags options))))
    options))

(defun who-arg-matches (ch player options)
  (and
   (or (immortalp ch) (not (pref-flagged player +pref-nowho+)))
   (<= (invis-level-of player) (level-of ch))
   (or (not (member :zone options)) (eql (zone-of (in-room-of ch))
                                         (zone-of (in-room-of player))))
   (or (not (member :plane options)) (eql (plane-of (zone-of (in-room-of ch)))
                                          (plane-of (zone-of (in-room-of player)))))
   (or (not (member :time options)) (eql (time-frame-of (zone-of (in-room-of ch)))
                                         (time-frame-of (zone-of (in-room-of player)))))
   (or (not (member :kills options)) (plusp (pkills-of player)))))

(defun char-class-color (ch class)
  (case class
    (#.+class-mage+
     "&m")
    (#.+class-cleric+
     (cond
       ((is-good ch) "&B")
       ((is-evil ch) "&R")
       (t            "&y")))
    (#.+class-knight+
     (cond
       ((is-good ch) "&B")
       ((is-evil ch) "&r")
       (t            "&y")))
    (#.+class-ranger+  "&g")
    (#.+class-barb+  "&c")
    (#.+class-thief+  "&N")
    (#.+class-cyborg+  "&c")
    (#.+class-psionic+  "&m")
    (#.+class-physic+  "&n")
    (#.+class-bard+  "&Y")
    (#.+class-monk+  "&g")
    (#.+class-mercenary+  "&y")
    (t              "&n")))

(defun char-class-name (class)
  (aref +char-class-abbrevs+ class))

(defun parse-pc-char-class (class-name)
  (position class-name +class-names+ :test #'string-abbrev))

(defun get-reputation-rank (ch)
  (cond
    ((zerop (reputation-of ch))
     0)
    ((>= (reputation-of ch) 1000)
     11)
    (t
     (1+ (floor (reputation-of ch) 100)))))

(defun reputation-desc (ch)
  (aref +reputation-msg+ (get-reputation-rank ch)))

(defun send-who-flags (ch player)
  (when (pref-flagged player +pref-nowho+)
    (send-to-char ch " &r(nowho)"))
  (let ((clan (real-clan (clan-of player))))
    (cond
      ((null clan)
       nil)
      ((not (pref-flagged player +pref-clan-hide+))
       (send-to-char ch " &c~a" (badge-of clan)))
      ((immortalp ch)
       (send-to-char ch " &c)~a(" (name-of clan)))))
  (when (and (plusp (invis-level-of player)) (immortalp ch))
    (send-to-char ch " &b(&mi~d&b)" (invis-level-of player)))
  (cond
    ((plr-flagged player +plr-mailing+)
     (send-to-char ch " &g(mailing)"))
    ((plr-flagged player +plr-writing+)
     (send-to-char ch " &g(writing)"))
    ((plr-flagged player +plr-olc+)
     (send-to-char ch " &g(creating)")))
  (when (pref-flagged player +pref-deaf+)
    (send-to-char ch " &b(deaf)"))
  (when (pref-flagged player +pref-notell+)
    (send-to-char ch " &b(notell)"))
  (when (plusp (quest-id-of player))
    (send-to-char ch " &Y(quest)"))
  (when (plr-flagged player +plr-afk+)
    (send-to-char ch " &g(afk)")))

(defun send-who-line (ch player options)
  (cond
    ((immortal-level-p player)
     (send-to-char ch "&Y[&G~7<~;~:@(~a~)~;~>&Y]&g " (badge-of player)))
    ((testerp player)
     (send-to-char ch "&Y[&GTESTING&Y]&g "))
    ((immortalp ch)
     (send-to-char ch "&g[~:[&r~;&n~]~2d&c(&n~2d&c) ~a~a&g] &n"
                   (pref-flagged player +pref-anonymous+)
                   (level-of player)
                   (remort-gen-of player)
                   (char-class-color player (char-class-of player))
                   (char-class-name (char-class-of player))))
    (t
     (send-to-char ch "&g[~:[&c--~*~;&n~2d~] ~a~a&g] &n"
                   (pref-flagged player +pref-anonymous+)
                   (level-of player)
                   (char-class-color player (char-class-of player))
                   (char-class-name (char-class-of player)))))

  (send-to-char ch "~a~@[~a~]&n"
                (name-of player)
                (title-of player))

  (unless (member :noflags options)
    (send-who-flags ch player))
  (when (member :kills options)
    (send-to-char ch " &R*~d KILLS* -~a-"
                  (pkills-of player)
                  (reputation-desc player)))
  (send-to-char ch "~%"))

(defun perform-who (ch options)
  (with-pagination ((link-of ch))
    ;; Print header
    (send-to-char ch "&W**************      &GVisible Players of TEMPUS      &W**************&n~%")

    ;; Filter out the players to be displayed
    (let* ((players (remove-if-not (lambda (actor)
                                     (and actor (in-room-of actor)))
                                   (mapcar #'actor-of
                                           (remove-if-not (lambda (cxn)
                                                            (typep cxn 'tempus-cxn))
                                                          *cxns*))))
           (displayed (sort (remove-if-not (lambda (player)
                                             (who-arg-matches ch player options))
                                           players)
                            #'> :key 'level-of)))

      ;; Display the proper players
      (dolist (player displayed)
        (send-who-line ch player options))

      ;; Send counts of the various kinds of players
      (let* ((immortals-displayed (length (remove-if-not #'immortal-level-p displayed)))
             (immortals-playing (length (remove-if-not #'immortal-level-p players)))
             (testers-displayed (length (remove-if-not #'testerp displayed)))
             (testers-playing (length (remove-if-not #'testerp players)))
             (players-displayed (- (length displayed) immortals-displayed testers-displayed))
             (players-playing (- (length players) immortals-playing testers-playing)))
        (if (immortalp ch)
            (send-to-char ch "&n~d of ~d immortal~:p, ~d tester~:p, and ~d player~:p displayed.~%"
                          immortals-displayed
                          immortals-playing
                          testers-displayed
                          players-displayed)
            (send-to-char ch "&n~d of ~d immortal~:p and ~d of ~d player~:p displayed.~%"

                          immortals-displayed
                          immortals-playing
                          players-displayed
                          players-playing))))))

(defun show-mud-date-to-char (ch)
  (unless (in-room-of ch)
    (error "No room of ch in show-mud-date-to-char"))

  (multiple-value-bind (hour day month year)
      (local-time-of (zone-of (in-room-of ch)))
    (declare (ignore hour))
    (incf day)
    (let ((suf (cond
                 ((= day 1) "st")
                 ((= day 2) "nd")
                 ((= day 3) "rd")
                 ((< day 20) "th")
                 ((= (mod day 10) 1) "st")
                 ((= (mod day 10) 2) "nd")
                 ((= (mod day 10) 3) "rd")
                 (t "th"))))
      (send-to-char ch "It is the ~d~a Day of the ~a, Year ~d~%"
                    day suf (aref +month-name+ month) year))))

(defun send-commands-to-ch (ch preamble pred)
  (let ((cmds (sort
               (remove-duplicates
                (mapcar #'first
                        (mapcar #'command-info-pattern
                                (remove-if-not pred *commands*)))
                :test #'string=)
               #'string<)))
    (with-pagination ((link-of ch))
      (send-to-char ch "~a~%~a" preamble (print-columns-to-string 5 15 cmds)))))

(defcommand (ch "commands") ()
  (send-commands-to-ch ch
                       "Commands:"
                       (lambda (cmd)
                         (not (or (member :mood (command-info-flags cmd))
                                  (member :social (command-info-flags cmd)))))))

(defcommand (ch "socials") ()
  (send-commands-to-ch ch
                       "Socials:"
                       (lambda (cmd)
                         (member :social (command-info-flags cmd)))))

(defcommand (ch "moods") ()
  (send-commands-to-ch ch
                       "Moods:"
                       (lambda (cmd)
                         (member :mood (command-info-flags cmd)))))

(defcommand (ch "look") (:resting)
  (look-at-room ch (in-room-of ch) t))

(defcommand (ch "look" thing) (:resting)
  (let ((vict (resolve-alias ch thing)))
    (cond
      (vict
       (look-at-char ch vict :look)
       (when (can-see-creature vict ch)
         (act ch :target vict
              :target-emit "$n looks at you."
              :not-target-emit "$n looks at $N.")))
      (t
       (send-to-char ch "There's no '~a' here.~%" thing)))))

(defcommand (ch "look" "at" thing) (:resting)
  (let ((vict (resolve-alias ch thing)))
    (if vict
        (look-at-char ch vict :look)
        (send-to-char ch "There's no '~a' here.~%" thing))))

(defcommand (ch "examine" thing) (:resting)
  (let ((vict (resolve-alias ch thing)))
    (if vict
        (look-at-char ch vict :examine)
        (send-to-char ch "There's no '~a' here.~%" thing))))

(defcommand (ch "glance" thing) (:resting)
  (let ((vict (resolve-alias ch thing)))
    (if vict
        (look-at-char ch vict :glance)
        (send-to-char ch "There's no '~a' here.~%" thing))))

(defcommand (ch "inventory") (:important)
  (send-to-char ch "~a"
                (with-output-to-string (str)
                  (format str "You are carrying:~%")
                  (list-obj-to-char str (carrying-of ch) ch :inv t))))

(defcommand (ch "who") ()
  (perform-who ch nil))

(defcommand (ch "who" flags) ()
  (perform-who ch (parse-who-args flags)))

(defcommand (ch "gold") ()
  (cond
    ((zerop (gold-of ch))
     (send-to-char ch "You're broke!~%"))
    ((= 1 (gold-of ch))
     (send-to-char ch "You have one miserable little gold coin.~%"))
    (t
     (send-to-char ch "You have ~a gold coins.~%" (gold-of ch)))))

(defcommand (ch "cash") ()
  (cond
    ((zerop (cash-of ch))
     (send-to-char ch "You're broke!~%"))
    ((= 1 (cash-of ch))
     (send-to-char ch "You have one miserable little credit.~%"))
    (t
     (send-to-char ch "You have ~a credits.~%" (cash-of ch)))))
