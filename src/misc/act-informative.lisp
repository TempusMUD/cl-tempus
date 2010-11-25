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
        (format stream " [~d]" count))
      (format stream "&n~%"))))

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

(defun show-obj-extra (object stream)
  (cond
    ((= (kind-of object) +item-note+)
     (format stream "~a~%" (or (action-desc-of object)
                               "It's blank.")))
    ((= (kind-of object) +item-drinkcon+)
     (format stream "It looks like a drink container.~%"))
    ((= (kind-of object) +item-fountain+)
     (format stream "It looks like a source of drink.~%"))
    ((= (kind-of object) +item-food+)
     (format stream "It looks edible.~%"))
    ((= (kind-of object) +item-holy-symb+)
     (format stream "It looks like the symbol of some deity.~%"))
    ((or (= (kind-of object) +item-cigarette+)
         (= (kind-of object) +item-pipe+))
     (if (zerop (aref (value-of object) 3))
         (format stream "It appears to be unlit.~%")
         (format stream "It appears to be lit and smoking.~%")))
    ((and (= (kind-of object) +item-container+)
          (not (zerop (aref (value-of object) 3))))
         (format stream "It looks like a corpse.~%"))
    ((= (kind-of object) +item-container+)
     (format stream "It looks like a container.~%")
     (if (and (car-closed object)
              (car-openable object))
         (format stream "It appears to be closed.~%")
         (progn
           (format stream "It appears to be open.~%")
           (if (contains-of object)
               (format stream "There appears to be something inside.~%")
               (format stream "It appears to be empty.~%")))))
    ((= (kind-of object) +item-syringe+)
     (if (zerop (aref (value-of object) 0))
         (format stream "It is full.~%")
         (format stream "It is empty.~%")))
    ((and (> (material-of object) +mat-none+)
          (< (material-of object) +top-material+))
     (format stream "It appears to be composed of ~a.~%"
             (aref +material-names+ (material-of object))))
    (t
     (format stream "You see nothing special.~%"))))


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
       (show-obj-extra object stream)))

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
                     (is-corpse (in-obj-of (first obj-list)))))
        (found nil))
    (loop with o = obj-list
       for i = (car obj-list) then (car o)
       while i do
       (cond
         ((or (not (is-visible-to i ch))
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
                                   when o
                                   count (is-visible-to (car o) ch)
                                   do (setf o (cdr o))))))))
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
             (>= (get-skill-bonus i +skill-kata+) 50))
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
             ((eql (first (fighting-of i)) ch)
              (format stream "~a is here, fighting YOU!" name))
             ((eql (in-room-of (first (fighting-of i))) (in-room-of i))
              (format stream "~a is here, fighting ~a!"
                      name (describe-char ch (first (fighting-of i)))))
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

        (format stream "&n~%")

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
         ((not (is-visible-to i ch))
          ;; You don't see creatures that you can't see (duh)
          (unless (or (immortalp i) (mob-flagged i +mob-utility+))
            ;; ... and you don't see utility mobs unless you're immortal
            (incf unseen))
          nil)
         ((and (aff-flagged i +aff-hide+)
               (not (aff3-flagged ch +aff3-sonic-imagery+))
               (not (pref-flagged ch +pref-holylight+)))
          ;; You might not see hiding creatures
          (let ((hide-prob (random-range 0 (get-skill-bonus i +skill-hide+)))
                (hide-roll (+ (random-range 0 (get-level-bonus ch))
                              (if (affected-by-spell ch +zen-awareness+)
                                  (floor (get-skill-bonus ch +zen-awareness+) 4)
                                  0))))
            (cond
              ((> hide-prob hide-roll)
               (incf unseen))
              (t
               (when (is-visible-to ch i)
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

  (when (and (room-is-dark (in-room-of ch)) (not (has-dark-sight ch)))
    (send-to-char ch "It is pitch black...~%")
    (return-from look-at-room))

  (if (pref-flagged ch +pref-roomflags+)
      (progn
        (send-to-char ch "&c[~5d] ~a [ ~a ] [ ~a ]"
                      (number-of room)
                      (name-of room)
                      (printbits (flags-of room) +room-bits+ "NONE")
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
                  (0 "&c[ &g!PK &c] ")
                  (1 "&c[ &YNPK &c] ")
                  (2 "&c[ &RCPK &c] ")))

  (unless (or (immortalp ch)
              (not (room-flagged room +room-smoke-filled+))
              (aff3-flagged ch +aff3-sonic-imagery+))
    (send-to-char ch "~%")
    (return-from look-at-room))

  ;; autoexits
  (when (pref-flagged ch +pref-autoexit+)
    (do-auto-exits ch room))
  (send-to-char ch "&n~%")

  ;; now list characters & objects
  (show-blood-and-ice ch room)
  (send-to-char ch "~a"
                (with-output-to-string (s)
                  (list-obj-to-char s (contents-of room) ch :room nil)))
  (send-to-char ch "~a"
                (with-output-to-string (s)
                  (list-char-to-char s (people-of room) ch))))

(defun list-worn (stream ch header-msg none-msg worn order pos-descs &key show-empty-p)
  (if (or show-empty-p (find-if #'identity worn))
      (loop
         initially (format stream "~a~%" header-msg)
         as pos in order
         as obj = (aref worn pos)
         do
           (cond
             ((null obj)
              (when (and show-empty-p (/= pos +wear-ass+))
                (format stream "~aNothing!~%" (aref pos-descs pos))))
             ((is-visible-to obj ch)
              (format stream "~a" (aref pos-descs pos))
              (show-obj-to-char stream obj ch :inv 0))
             (t
              (format stream "~aSomething" (aref pos-descs pos)))))
      (format stream "~a~%" none-msg)))

(defun obj-condition-desc (obj)
  (cond
    ((or (= (damage-of obj) -1)
         (= (max-dam-of obj) -1))
     "&gunbreakable&n")
    ((is-obj-stat2 obj +item2-broken+)
     "<broken>")
    ((zerop (max-dam-of obj))
     "frail")
    (t
     (let ((descs '((0 "perfect")
                    (10 "excellent")
                    (30 "&cgood&n")
                    (50 "&cfair&n")
                    (60 "&yworn&n")
                    (70 "&yshabby&n")
                    (90 "&ybad&n")
                    (100 "&rterrible&n"))))
       (second
        (assoc (floor (* (- (max-dam-of obj) (damage-of obj)) 100)
                      (max-dam-of obj))
               descs
               :test #'<=))))))

(defun list-worn-status (stream ch header-msg none-msg worn order)
  (if (find-if #'identity worn)
      (loop
         initially (format stream "~a~%" header-msg)
         as pos in order
         as obj = (aref worn pos)
         when (and obj (is-visible-to obj ch))
         do (format stream "-~a- is in ~a condition~%"
                    (name-of obj)
                    (obj-condition-desc obj)))
      (format stream "~a~%" none-msg)))

(defcommand (ch "equipment") (:sleeping)
  (send-to-char ch "~a"
                (with-output-to-string (s)
                  (list-worn s ch
                             "You are using:"
                             "You're totally naked!"
                             (equipment-of ch)
                             +eq-pos-order+
                             +eq-pos-descs+))))

(defcommand (ch "equipment" "all") (:sleeping)
  (send-to-char ch "~a"
                (with-output-to-string (s)
                  (list-worn s ch
                             "You are using:"
                             "You're totally naked!"
                             (equipment-of ch)
                             +eq-pos-order+
                             +eq-pos-descs+
                             :show-empty-p t))))

(defcommand (ch "equipment" "status") (:sleeping)
  (send-to-char ch "~a"
                (with-output-to-string (s)
                  (list-worn-status s ch
                                    "Equipment status:"
                                    "You're totally naked!"
                                    (equipment-of ch)
                                    +eq-pos-order+))))

(defcommand (ch "implants") (:sleeping)
  (send-to-char ch "~a"
                (with-output-to-string (s)
                  (list-worn s ch
                             "You are implanted with:"
                             "You don't have any implants!"
                             (implants-of ch)
                             +implant-pos-order+
                             +implant-pos-descs+))))

(defcommand (ch "implants" "all") (:sleeping)
  (send-to-char ch "~a"
                (with-output-to-string (s)
                  (list-worn s ch
                             "You are implanted with:"
                             "You don't have any implants!"
                             (implants-of ch)
                             +implant-pos-order+
                             +implant-pos-descs+
                             :show-empty-p t))))

(defcommand (ch "implants" "status") (:sleeping)
  (send-to-char ch "~a"
                (with-output-to-string (s)
                  (list-worn-status s ch
                                    "Implant status:"
                                    "You don't have any implants!"
                                    (implants-of ch)
                                    +implant-pos-order+))))

(defcommand (ch "tattoos") (:sleeping)
  (send-to-char ch "~a"
                (with-output-to-string (s)
                  (list-worn s ch
                             "You have the following tattoos:"
                             "You're a tattoo virgin!"
                             (tattoos-of ch)
                             +tattoo-pos-order+
                             +tattoo-pos-descs+))))

(defcommand (ch "tattoos" "all") (:sleeping)
  (send-to-char ch "~a"
                (with-output-to-string (s)
                  (list-worn s ch
                             "You have the following tattoos:"
                             "You're a tattoo virgin!"
                             (tattoos-of ch)
                             +tattoo-pos-order+
                             +tattoo-pos-descs+
                             :show-empty-p t))))

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
      (loop
         for dch in displayed
         for pch in players
         if (immortal-level-p dch)
         count t into immortals-displayed
         else if (testerp dch)
         count t into testers-displayed
         else
         count t into players-displayed
         end
         if (immortal-level-p pch)
         count t into immortals-playing
         else if (testerp pch)
         count t into testers-playing
         else
         count t into players-playing
         end
         finally
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

(defun describe-attributes (ch)
  (with-output-to-string (str)
    (format str "     &YStrength:&n ")
    (cond
      ((<= (str-of ch) 3)
       (format str "You can barely stand up under your own weight."))
      ((<= (str-of ch) 4)
       (format str "You couldn't beat your way out of a paper bag."))
      ((<= (str-of ch) 5)
       (format str "You are laughed at by ten year olds."))
      ((<= (str-of ch) 6)
       (format str "You are a weakling."))
      ((<= (str-of ch) 7)
       (format str "You can pick up large rocks without breathing too hard."))
      ((<= (str-of ch) 8)
       (format str "You are not very strong."))
      ((<= (str-of ch) 10)
       (format str "You are of average strength."))
      ((<= (str-of ch) 12)
       (format str "You are fairly strong."))
      ((<= (str-of ch) 15)
       (format str "You are a nice specimen."))
      ((<= (str-of ch) 16)
       (format str "You are a damn nice specimen."))
      ((<= (str-of ch) 18)
       (format str "You are very strong."))
      ((<= (str-of ch) 18)
       (format str "You are extremely strong."))
      ((<= (str-of ch) 18)
       (format str "You are exceptionally strong."))
      ((<= (str-of ch) 18)
       (format str "Your strength is awe inspiring."))
      ((<= (str-of ch) 18)
       (format str "Your strength is super-human."))
      ((<= (str-of ch) 18)
       (format str "Your strength is at the human peak!"))
      ((<= (str-of ch) 19)
       (format str "You have the strength of a hill giant!"))
      ((<= (str-of ch) 20)
       (format str "You have the strength of a stone giant!"))
      ((<= (str-of ch) 21)
       (format str "You have the strength of a frost giant!"))
      ((<= (str-of ch) 22)
       (format str "You can toss boulders with ease!"))
      ((<= (str-of ch) 23)
       (format str "You have the strength of a cloud giant!"))
      ((<= (str-of ch) 24)
       (format str "You possess a herculean might!"))
      ((<= (str-of ch) 25)
       (format str "You have the strength of a god!")))
    (format str "~%")

    (format str " &YIntelligence:&n ")
    (cond
      ((<= (int-of ch) 5)
       (format str "You lose arguments with inanimate objects."))
      ((<= (int-of ch) 8)
       (format str "You're about as smart as a rock."))
      ((<= (int-of ch) 10)
       (format str "You are a bit slow-witted."))
      ((<= (int-of ch) 12)
       (format str "Your intelligence is average."))
      ((<= (int-of ch) 13)
       (format str "You are fairly intelligent."))
      ((<= (int-of ch) 14)
       (format str "You are very smart."))
      ((<= (int-of ch) 16)
       (format str "You are exceptionally smart."))
      ((<= (int-of ch) 17)
       (format str "You possess a formidable intellect."))
      ((<= (int-of ch) 18)
       (format str "You are a powerhouse of logic."))
      ((<= (int-of ch) 19)
       (format str "You are an absolute genius!"))
      ((<= (int-of ch) 20)
       (format str "You are a suuuuper-geniuus!"))
      (t
       (format str "You solve nonlinear higher dimensional systems in your sleep.")))
    (format str "~%")

    (format str "       &YWisdom:&n ")
    (cond
      ((<= (wis-of ch) 5)
       (format str "Yoda you are not."))
      ((<= (wis-of ch) 6)
       (format str "You are pretty foolish."))
      ((<= (wis-of ch) 8)
       (format str "You are fairly foolhardy."))
      ((<= (wis-of ch) 10)
       (format str "Your wisdom is average."))
      ((<= (wis-of ch) 12)
       (format str "You are fairly wise."))
      ((<= (wis-of ch) 15)
       (format str "You are very wise."))
      ((<= (wis-of ch) 18)
       (format str "You are exceptionally wise."))
      ((<= (wis-of ch) 19)
       (format str "You have the wisdom of a god!"))
      (t
       (format str "God himself comes to you for advice.")))
    (format str "~%")

    (format str "    &YDexterity:&n ")
    (cond
      ((<= (dex-of ch) 5)
       (format str "I wouldnt walk too fast if I were you."))
      ((<= (dex-of ch) 8)
       (format str "You're pretty clumsy."))
      ((<= (dex-of ch) 10)
       (format str "Your agility is pretty average."))
      ((<= (dex-of ch) 12)
       (format str "You are fairly agile."))
      ((<= (dex-of ch) 15)
       (format str "You are very agile."))
      ((<= (dex-of ch) 18)
       (format str "You are exceptionally agile."))
      (t
       (format str "You have the agility of a god!")))
    (format str "~%")

    (format str " &YConstitution:&n ")
    (cond
      ((<= (con-of ch) 3)
       (format str "You are dead, but haven't realized it yet."))
      ((<= (con-of ch) 5)
       (format str "You're as healthy as a rabid dog."))
      ((<= (con-of ch) 7)
       (format str "A child poked you once, and you have the scars to prove it."))
      ((<= (con-of ch) 8)
       (format str "You're pretty skinny and sick looking."))
      ((<= (con-of ch) 10)
       (format str "Your health is average."))
      ((<= (con-of ch) 12)
       (format str "You are fairly healthy."))
      ((<= (con-of ch) 15)
       (format str "You are very healthy."))
      ((<= (con-of ch) 18)
       (format str "You are exceptionally healthy."))
      ((<= (con-of ch) 19)
       (format str "You are practically immortal!"))
      (t
       (format str "You can eat pathogens for breakfast.")))
    (format str "~%")

    (format str "     &YCharisma:&n ")
    (cond
      ((<= (cha-of ch) 5)
       (format str "U-G-L-Y"))
      ((<= (cha-of ch) 6)
       (format str "Your face could turn a family of elephants to stone."))
      ((<= (cha-of ch) 7)
       (format str "Small children run away from you screaming."))
      ((<= (cha-of ch) 8)
       (format str "You are totally unattractive."))
      ((<= (cha-of ch) 9)
       (format str "You are slightly unattractive."))
      ((<= (cha-of ch) 10)
       (format str "You are not too unpleasant to deal with."))
      ((<= (cha-of ch) 12)
       (format str "You are a pleasant person."))
      ((<= (cha-of ch) 15)
       (format str "You are exceptionally attractive."))
      ((<= (cha-of ch) 16)
       (format str "You have a magnetic personality."))
      ((<= (cha-of ch) 17)
       (format str "Others eat from the palm of your hand."))
      ((<= (cha-of ch) 18)
       (format str "Your image should be chiseled in marble!"))
      ((<= (cha-of ch) 22)
       (format str "Others eat from the palm of your hand.  Literally."))
      (t
       (format str "If the gods made better they'd have kept it for themselves.")))
    (format str "~%")))


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
                         (and (can-do-command ch cmd)
                              (not (or (member :mood (command-info-flags cmd))
                                       (member :social (command-info-flags cmd))))))))

(defcommand (ch "socials") ()
  (send-commands-to-ch ch
                       "Socials:"
                       (lambda (cmd)
                         (and (can-do-command ch cmd)
                              (member :social (command-info-flags cmd))))))

(defcommand (ch "moods") ()
  (send-commands-to-ch ch
                       "Moods:"
                       (lambda (cmd)
                         (and (can-do-command ch cmd)
                              (member :mood (command-info-flags cmd))))))

(defcommand (ch "look") (:resting :important)
  (cond
    ((null (link-of ch))
     (values))
    ((< (position-of ch) +pos-sleeping+)
     (send-to-char ch "You can't see anything but stars!~%"))
    ((not (check-sight-self ch))
     (send-to-char ch "You can't see a damned thing, you're blind!~%"))
    ((and (room-is-dark (in-room-of ch)) (not (has-dark-sight ch)))
     (send-to-char ch "It is pitch black...~%"))
    (t
     (look-at-room ch (in-room-of ch) t))))

(defun pluralp (str)
  (and (not (string= str "portcullis"))
       (or (find str '("teeth" "cattle" "data") :test #'string-equal)
           (char= (char str (1- (length str))) #\s))))

(defun find-extradesc (ch str)
  (let ((result (find str (ex-description-of (in-room-of ch))
                      :test #'string-abbrev
                      :key 'keyword-of)))
    (when result
      (return-from find-extradesc (description-of result))))

  (let ((exit-pos (find str (remove nil (dir-option-of (in-room-of ch)))
                        :test #'string-abbrev
                        :key 'keyword-of)))
    (when exit-pos
      (let* ((exit (aref (exit-info-of (in-room-of ch)) exit-pos))
             (exit-name (first-word (keyword-of exit))))
        (return-from find-extradesc
          (format nil "The ~a ~a ~a.~%"
                  exit-name
                  (if (pluralp exit-name) "are" "is")
                  (if (logtest (exit-info-of exit) +door-closed+)
                      "closed" "open"))))))

  (loop for obj across (equipment-of ch)
       when obj do
       (let ((result (find str (ex-description-of obj)
                           :test #'string-abbrev
                           :key 'keyword-of)))
         (when result
           (return-from find-extradesc (description-of result))))  )

  (loop for obj in (carrying-of ch)
       when obj do
       (let ((result (find str (ex-description-of obj)
                           :test #'string-abbrev
                           :key 'keyword-of)))
         (when result
           (return-from find-extradesc (description-of result))))  )

  (loop for obj in (contents-of (in-room-of ch))
       when obj do
       (let ((result (find str (ex-description-of obj)
                           :test #'string-abbrev
                           :key 'keyword-of)))
         (when result
           (return-from find-extradesc (description-of result))))  )
  nil)

(defun look-at-target (ch arg cmd)
  (cond
    ((null (link-of ch))
     (values))
    ((< (position-of ch) +pos-sleeping+)
     (send-to-char ch "You can't see anything but stars!~%"))
    ((not (check-sight-self ch))
     (send-to-char ch "You can't see a damned thing, you're blind!~%"))
    ((and (room-is-dark (in-room-of ch)) (not (has-dark-sight ch)))
     (send-to-char ch "It is pitch black...~%"))
    (t
     (let ((vict (first (resolve-alias ch arg
                                       (append
                                        (people-of (in-room-of ch))
                                        (coerce (delete nil (equipment-of ch)) 'list)
                                        (carrying-of ch)
                                        (contents-of (in-room-of ch)))))))
       (with-pagination ((link-of ch))
         (cond
           ((typep vict 'creature)
            (when (is-visible-to ch vict)
              (act ch :target vict
                   :all-emit "$n look$% at $N."))
            (look-at-char ch vict cmd))
           (t
            (let ((desc (find-extradesc ch arg)))
              (cond
                (desc
                 (send-to-char ch "~a" desc)
                 (when vict
                   (send-to-char ch "~a~%"
                                 (with-output-to-string (str)
                                   (show-obj-bits vict ch str)))))
                (vict
                 (send-to-char ch "~a"
                               (with-output-to-string (str)
                                 (show-obj-extra vict str))))
                (t
                 (send-to-char ch "There's no '~a' here.~%" arg)))))))))))

(defun look-into-target (ch arg)
  (let* ((objs (resolve-alias ch arg (append (carrying-of ch)
                                             (contents-of (in-room-of ch))
                                             (coerce (equipment-of ch) 'list))))
         (obj (first objs)))
    (cond
      ((null objs)
       (send-to-char ch "There doesn't seem to be ~a ~a here.~%"
                     (a-or-an arg) arg))
      ((rest objs)
       (send-to-char ch "You can only look into one object at a time.~%"))
      ((not (or (is-obj-kind obj +item-drinkcon+)
                (is-obj-kind obj +item-fountain+)
                (is-obj-kind obj +item-container+)
                (is-obj-kind obj +item-pipe+)
                (is-obj-kind obj +item-vehicle+)))
       (send-to-char ch "There's nothing inside that!~%"))
      ((and (logtest (aref (value-of obj) 1) +cont-closed+)
            (not (immortalp ch)))
       (send-to-char ch "It is closed.~%"))
      ((is-obj-kind obj +item-container+)
       (send-to-char ch "~a (~a):~%"
                     (name-of obj)
                     (cond
                       ((carried-by-of obj) "carried")
                       ((worn-by-of obj) "used")
                       ((in-room-of obj) "here")))
       (send-to-char ch "~a"
                     (with-output-to-string (str)
                       (list-obj-to-char str (contains-of obj) ch
                                         :content t)))))))


(defcommand (ch "look" thing) (:resting :important)
  (look-at-target ch thing :look))

(defcommand (ch "look" "at" thing) (:resting :important)
  (look-at-target ch thing :look))

(defcommand (ch "look" "into" thing) (:resting :important)
  (look-into-target ch thing))

(defcommand (ch "examine" thing) (:resting)
  (look-at-target ch thing :examine))

(defcommand (ch "glance" thing) (:resting)
  (look-at-target ch thing :glance))

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

(defcommand (ch "alignment") ()
  (send-to-char ch "Your alignment is &~c~d&n.~%"
                  (cond
                    ((is-good ch) #\c)
                    ((is-evil ch) #\r)
                    (t            #\y))
                  (alignment-of ch)))

(defcommand (ch "clear") ()
  (send-to-char ch "&@"))

(defcommand (ch "cls") ()
  (send-to-char ch "&@"))

(defcommand (ch "version") ()
  (send-to-char ch "TempusMUD, version 1.0~%"))

(defun send-bad-char-affects (ch stream)
  (when (affected-by-spell ch +spell-fire-breathing+)
    (format stream "You are empowered with breath of FIRE!~%"))
  (when (affected-by-spell ch +spell-frost-breathing+)
    (format stream "You are empowered with breath of FROST!~%"))
  (when (aff-flagged ch +aff-blind+)
    (format stream "You have been blinded!~%"))
  (when (or (aff-flagged ch +aff-poison+)
            (aff3-flagged ch +aff3-poison-2+)
            (aff3-flagged ch +aff3-poison-3+))
    (format stream "You are poisoned!~%"))
  (when (aff2-flagged ch +aff2-petrified+)
    (format stream "You have been turned to stone.~%"))
  (when (aff3-flagged ch +aff3-radioactive+)
    (format stream "You are radioactive.~%"))
  (when (affected-by-spell ch +spell-gamma-ray+)
    (format stream "You have been irradiated.~%"))
  (when (aff2-flagged ch +aff2-ablaze+)
    (format stream "You are &RON FIRE!!&n~%"))
  (when (affected-by-spell ch +spell-quad-damage+)
    (format stream "You are dealing out &cquad damage&n.~%"))
  (when (affected-by-spell ch +spell-blackmantle+)
    (format stream "You are covered by blackmantle.~%"))
  (when (affected-by-spell ch +spell-entangle+)
    (format stream "You are entangled in the undergrowth!~%"))
  (let* ((af (affected-by-spell ch +skill-disguise+))
         (mob (and af (real-mobile-proto (modifier-of af)))))
    (when mob
      (format stream "You are disguised as ~a.~%" (name-of mob))))
  ;; radiation sickness
  (when (affected-by-spell ch +type-rad-sickness+)
    (case (random 6)
      ((0 1 2)
       (format stream "You feel nauseous.~%"))
      ((3 4)
       (format stream "You feel sick and your skin is dry.~%"))
      (t
       (format stream "You feel sick and your hair is falling out.~%"))))

  (when (aff2-flagged ch +aff2-slow+)
    (format stream "You feel unnaturally slowed.~%"))
  (when (aff-flagged ch +aff-charm+)
    (format stream "You have been charmed!~%"))
  (when (and (aff3-flagged ch +aff3-mana-leak+)
             (not (aff3-flagged ch +aff3-mana-tap+)))
    (format stream "You are slowly being drained of your spiritual energy.~%"))
  (when (and (aff3-flagged ch +aff3-energy-leak+)
             (not (aff3-flagged ch +aff3-energy-tap+)))
    (format stream "Your body is slowly being drained of physical energy.~%"))

  (when (aff3-flagged ch +aff3-symbol-of-pain+)
    (format stream "Your mind burns with the symbol of pain!~%"))
  (when (affected-by-spell ch +spell-weakness+)
    (format stream "You feel unusually weakened.~%"))
  (when (aff3-flagged ch +aff3-psychic-crush+)
    (format stream "You feel a psychic force crushing your mind!~%"))
  (when (affected-by-spell ch +spell-fear+)
    (format stream "The world is a terribly frightening place!~%"))
  (when (aff3-flagged ch +aff3-acidity+)
    (format stream "Your body is producing self-corroding acids!~%"))
  (when (aff3-flagged ch +aff3-gravity-well+)
    (format stream "Spacetime is bent around you in a powerful gravity well!~%"))
  (when (aff3-flagged ch +aff3-hamstrung+)
    (format stream "&rThe gash on your leg is &RBLEEDING&r all over!!&n~%"))
  (when (is-sick ch)
    (format stream "You are afflicted with a terrible sickness!~%"))
  (when (aff-flagged ch +aff-confusion+)
    (format stream "You are very confused.~%"))
  (when (affected-by-spell ch +spell-motor-spasm+)
    (format stream "Your muscles are spasming uncontrollably!~%"))
  (when (aff2-flagged ch +aff2-vertigo+)
    (format stream "You are lost in a sea of vertigo.~%"))
  (when (aff3-flagged ch +aff3-tainted+)
    (format stream "The very essence of your being has been tainted.~%"))
  (when (affected-by-spell ch +song-insidious-rhythm+)
    (format stream "Your senses have been dulled by insidious melodies.~%"))
  (when (affected-by-spell ch +song-verse-of-vulnerability+)
    (format stream "You feel more vulnerable to attack.~%"))

  (let* ((af (affected-by-spell ch +spell-vampiric-regeneration+))
         (name (and af (retrieve-player-name (modifier-of af)))))
    (when af
      (format stream "You are under the effects of ~@[~a's ~]vampiric regeneration.~%" name)))
  (let* ((af (affected-by-spell ch +spell-locust-regeneration+))
         (name (and af (retrieve-player-name (modifier-of af)))))
    (when af
      (format stream "You are under the effects of ~@[~a's ~]locust regeneration.~%" name))))

(defun send-good-char-affects (ch stream)
  (when (is-soulless ch)
    (format stream "A deep despair clouds your soulless mind.~%"))
  (when (and (aff-flagged ch +aff-sneak+)
             (not (aff3-flagged ch +aff3-infiltrate+)))
    (format stream "You are sneaking.~%"))
  (when(aff3-flagged ch +aff3-infiltrate+)
    (format stream "You are infiltrating.~%"))
  (when (aff-flagged ch +aff-invisible+)
    (format stream "You are invisible.~%"))
  (when (aff2-flagged ch +aff2-transparent+)
    (format stream "You are transparent.~%"))
  (when (aff-flagged ch +aff-detect-invis+)
    (format stream "You are sensitive to the presence of invisible things.~%"))
  (when (aff3-flagged ch +aff3-detect-poison+)
    (format stream "You are sensitive to the presence of poisons.~%"))
  (when (aff2-flagged ch +aff2-true-seeing+)
    (format stream "You are seeing truly.~%"))
  (when (aff-flagged ch +aff-sanctuary+)
    (format stream "You are protected by Sanctuary.~%"))
  (when (affected-by-spell ch +spell-armor+)
    (format stream "You feel protected.~%"))
  (when (affected-by-spell ch +spell-barkskin+)
    (format stream "Your skin is thick and tough like tree bark.~%"))
  (when (affected-by-spell ch +spell-stoneskin+)
    (format stream "Your skin is as hard as granite.~%"))
  (when (aff-flagged ch +aff-infravision+)
    (format stream "Your eyes are glowing red.~%"))
  (when (aff-flagged ch +aff-rejuv+)
    (format stream "You feel like your body will heal with a good rest.~%"))
  (when (aff-flagged ch +aff-regen+)
    (format stream "Your body is regenerating itself rapidly.~%"))
  (when (aff-flagged ch +aff-glowlight+)
    (format stream "You are followed by a ghostly illumination.~%"))
  (when (aff-flagged ch +aff-blur+)
    (format stream "Your image is blurred and shifting.~%"))
  (when (aff2-flagged ch +aff2-displacement+)
    (if (affected-by-spell ch +spell-refraction+)
        (format stream "Your body is irregularly refractive.~%")
        (format stream "Your image is displaced.~%")))
  (when (affected-by-spell ch +spell-electrostatic-field+)
    (format stream "You are surrounded by an electrostatic field.~%"))
  (when (aff2-flagged ch +aff2-fire-shield+)
    (format stream "You are protected by a shield of fire.~%"))
  (when (aff2-flagged ch +aff2-blade-barrier+)
    (format stream "You are protected by whirling blades.~%"))
  (when (aff2-flagged ch +aff2-energy-field+)
    (format stream "You are surrounded by a field of energy.~%"))
  (when (aff3-flagged ch +aff3-prismatic-sphere+)
    (format stream "You are surrounded by a prismatic sphere of light.~%"))
  (when (aff2-flagged ch +aff2-fluorescent+)
    (format stream "The atoms in your vicinity are fluorescent.~%"))
  (when (aff2-flagged ch +aff2-divine-illumination+)
    (cond
      ((is-evil ch)
       (format stream "An unholy light is following you.~%"))
      ((is-good ch)
       (format stream "A holy light is following you.~%"))
      (t
       (format stream "A sickly light is following you.~%"))))
  (when (aff2-flagged ch +aff2-berserk+)
    (format stream "You are BERSERK!~%"))
  (when (aff-flagged ch +aff-protect-good+)
    (format stream "You are protected from good.~%"))
  (when (aff-flagged ch +aff-protect-evil+)
    (format stream "You are protected from evil.~%"))
  (when (aff2-flagged ch +aff2-prot-devils+)
    (format stream "You are protected from devils.~%"))
  (when (aff2-flagged ch +aff2-prot-demons+)
    (format stream "You are protected from demons.~%"))
  (when (aff2-flagged ch +aff2-protect-undead+)
    (format stream "You are protected from the undead.~%"))
  (when (aff2-flagged ch +aff2-prot-lightning+)
    (format stream "You are protected from lightning.~%"))
  (when (aff2-flagged ch +aff2-prot-fire+)
    (format stream "You are protected from fire.~%"))
  (when (affected-by-spell ch +spell-magical-prot+)
    (format stream "You are protected against magic.~%"))
  (when (aff2-flagged ch +aff2-endure-cold+)
    (format stream "You can endure extreme cold.~%"))
  (when (aff-flagged ch +aff-sense-life+)
    (format stream "You are sensitive to the presence of living creatures~%"))
  (when (affected-by-spell ch +skill-empower+)
    (format stream "You are empowered.~%"))
  (when (aff2-flagged ch +aff2-telekinesis+)
    (format stream "You are feeling telekinetic.~%"))
  (when (aff2-flagged ch +aff2-haste+)
    (format stream "You are moving very fast.~%"))
  (when (affected-by-spell ch +skill-kata+)
    (format stream "You feel focused from your kata.~%"))
  (when (aff2-flagged ch +aff2-oblivity+)
    (format stream "You are oblivious to pain.~%"))
  (when (affected-by-spell ch +zen-motion+)
    (format stream "The zen of motion is one with your body.~%"))
  (when (affected-by-spell ch +zen-translocation+)
    (format stream "You are as one with the zen of translocation.~%"))
  (when (affected-by-spell ch +zen-celerity+)
    (format stream "You are under the effects of the zen of celerity.~%"))
  (when (and (aff3-flagged ch +aff3-mana-tap+)
             (not (aff3-flagged ch +aff3-mana-leak+)))
    (format stream "You have a direct tap to the spiritual energies of the universe.~%"))
  (when (and (aff3-flagged ch +aff3-energy-tap+)
             (not (aff3-flagged ch +aff3-energy-leak+)))
    (format stream "Your body is absorbing physical energy from the universe.~%"))
  (when (aff3-flagged ch +aff3-sonic-imagery+)
    (format stream "You are perceiving sonic images.~%"))
  (when (aff3-flagged ch +aff3-prot-heat+)
    (format stream "You are protected from heat.~%"))
  (when (affected-by-spell ch +spell-righteous-penetration+)
    (format stream "You feel overwhelmingly righteous!~%"))
  (when (affected-by-spell ch +spell-pray+)
    (format stream "You feel guided by divine forces.~%"))
  (when (affected-by-spell ch +spell-bless+)
    (format stream "You feel blessed.~%"))
  (when (affected-by-spell ch +spell-death-knell+)
    (format stream "You feel giddy from the absorption of a life.~%"))
  (when (affected-by-spell ch +spell-malefic-violation+)
    (format stream "You feel overwhelmingly wicked!~%"))
  (when (affected-by-spell ch +spell-mana-shield+)
    (format stream "You are protected by a mana shield.~%"))
  (when (affected-by-spell ch +spell-shield-of-righteousness+)
    (format stream "You are surrounded by a shield of righteousness.~%"))
  (when (affected-by-spell ch +spell-anti-magic-shell+)
    (format stream "You are enveloped in an anti-magic shell.~%"))
  (when (affected-by-spell ch +spell-sanctification+)
    (format stream "You have been sanctified!~%"))
  (when (affected-by-spell ch +spell-divine-intervention+)
    (format stream "You are shielded by divine intervention.~%"))
  (when (affected-by-spell ch +spell-sphere-of-desecration+)
    (format stream "You are surrounded by a black sphere of desecration.~%"))

  ;; pisonic affections
  (when (affected-by-spell ch +spell-power+)
    (format stream "Your physical strength is augmented.~%"))
  (when (affected-by-spell ch +spell-intellect+)
    (format stream "Your mental faculties are augmented.~%"))
  (when (aff-flagged ch +aff-nopain+)
    (format stream "Your mind is ignoring pain.~%"))
  (when (aff-flagged ch +aff-retina+)
    (format stream "Your retina is especially sensitive.~%"))
  (cond
    ((affected-by-spell ch +skill-adrenal-maximizer+)
     (format stream "Shukutei Adrenal Maximizations are active.~%"))
    ((aff-flagged ch +aff-adrenaline+)
     (format stream "Your adrenaline is pumping.~%")))
  (when (aff-flagged ch +aff-confidence+)
    (format stream "You feel very confident.~%"))
  (when (affected-by-spell ch +spell-dermal-hardening+)
    (format stream "Your dermal surfaces are hardened.~%"))
  (when (affected-by-spell ch +spell-lattice-hardening+)
    (format stream "Your molecular lattice has been strengthened.~%"))
  (when (aff3-flagged ch +aff3-nobreathe+)
    (format stream "You are not breathing.~%"))
  (when (aff3-flagged ch +aff3-psishield+)
    (format stream "You are protected by a psionic shield.~%"))
  (cond
    ((affected-by-spell ch +spell-metabolism+)
     (format stream "Your metabolism is racing.~%"))
    ((affected-by-spell ch +spell-relaxation+)
     (format stream "You feel very relaxed.~%")))
  (when (affected-by-spell ch +spell-endurance+)
    (format stream "Your endurance is increased.~%"))
  (when (affected-by-spell ch +spell-capacitance-boost+)
    (format stream "Your energy capacitance is boosted.~%"))
  (when (affected-by-spell ch +spell-psychic-resistance+)
    (format stream "Your mind is resistant to external energies.~%"))
  (when (affected-by-spell ch +spell-psychic-feedback+)
    (format stream "You are providing psychic feedback to your attackers.~%"))

  ;; physic affects
  (when (aff3-flagged ch +aff3-attraction-field+)
    (format stream "You are emitting an attraction field.~%"))
  (when (affected-by-spell ch +spell-repulsion-field+)
    (format stream "You are emitting a repulsion field.~%"))
  (when (affected-by-spell ch +spell-vacuum-shroud+)
    (format stream "You are existing in a total vacuum.~%"))
  (when (affected-by-spell ch +spell-chemical-stability+)
    (format stream "You feel chemically inert.~%"))
  (when (affected-by-spell ch +spell-albedo-shield+)
    (format stream "You are protected from electromagnetic attacks.~%"))
  (when (affected-by-spell ch +spell-gauss-shield+)
    (format stream "You feel somewhat protected from metal.~%"))
  (when (affected-by-spell ch +spell-dimensional-shift+)
    (format stream "You are traversing a parallel dimension.~%"))
  (when (affected-by-spell ch +spell-dimensional-void+)
    (format stream "You are disoriented from your foray into the interdimensional void!~%"))

  ;; cyborg affects
  (when (aff3-flagged ch +aff3-damage-control+)
    (format stream "Your Damage Control process is running.~%"))
  (when (affected-by-spell ch +skill-defensive-pos+)
    (format stream "You are postured defensively.~%"))
  (when (affected-by-spell ch +skill-offensive-pos+)
    (format stream "You are postured offensively.~%"))
  (when (affected-by-spell ch +skill-neural-bridging+)
    (format stream "Your neural pathways have been bridged.~%"))
  (when (affected-by-spell ch +skill-melee-combat-tac+)
    (format stream "Melee Combat Tactics are in effect.~%"))
  (when (affected-by-spell ch +skill-reflex-boost+)
    (format stream "Your Reflex Boosters are active.~%"))

  (when (aff3-flagged ch +aff3-shroud-obscurement+)
    (format stream "You are surrounded by an magical obscurement shroud.~%"))
  (when (affected-by-spell ch +spell-detect-scrying+)
    (format stream "You are sensitive to attempts to magical scrying.~%"))
  (when (affected-by-spell ch +skill-elusion+)
    (format stream "You are attempting to hide your tracks.~%"))
  (when (affected-by-spell ch +spell-telepathy+)
    (format stream "Your telepathic abilities are greatly enhanced.~%"))
  (when (affected-by-spell ch +spell-animal-kin+)
    (format stream "You are feeling a strong bond with animals.~%"))
  (when (affected-by-spell ch +spell-thorn-skin+)
    (format stream "There are thorns protruding from your skin.~%"))
  (when (affected-by-spell ch +skill-nanite-reconstruction+)
    (format stream "Your implants are undergoing nanite reconstruction~%"))
  (when (aff2-flagged ch +aff2-prot-rad+)
    (format stream "You are immune to the effects of radiation.~%"))

  ;; bard affects
  (when (affected-by-spell ch +song-misdirection-melisma+)
    (format stream "Your path is cloaked in the tendrils of song.~%"))
  (when (affected-by-spell ch +song-aria-of-armament+)
    (format stream "You feel protected by song.~%"))
  (when (affected-by-spell ch +song-melody-of-mettle+)
    (format stream "Your vitality is boosted by the Melody of Mettle.~%"))
  (when (affected-by-spell ch +song-defense-ditty+)
    (format stream "Harmonic resonance protects you from deleterious affects.~%"))
  (when (affected-by-spell ch +song-alrons-aria+)
    (format stream "Alron guides your hands.~%"))
  (when (affected-by-spell ch +song-verse-of-valor+)
    (format stream "The spirit of fallen heroes fills your being.~%"))
  (when (affected-by-spell ch +song-drifters-ditty+)
    (format stream "A pleasant tune gives you a pep in your step.~%"))
  (when (affected-by-spell ch +song-chant-of-light+)
    (format stream "You are surrounded by a warm glow.~%"))
  (when (affected-by-spell ch +song-aria-of-asylum+)
    (format stream "You are enveloped by a gossimer shield.~%"))
  (when (affected-by-spell ch +song-rhythm-of-rage+)
    (format stream "You are consumed by a feril rage!~%"))
  (when (affected-by-spell ch +song-power-overture+)
    (format stream "Your strength is bolstered by song.~%"))
  (when (affected-by-spell ch +song-guiharias-glory+)
    (format stream "The power of dieties is rushing through your veins.~%"))
  (let ((af (affected-by-spell ch +song-mirror-image-melody+)))
    (when af
      (format stream "You are being accompanied by ~d mirror image~:p.~%"
                    (modifier-of af))))
  (when (affected-by-spell ch +song-unladen-swallow-song+)
    (format stream "You are under the effect of an uplifting tune!~%"))
  (when (affected-by-spell ch +song-irresistable-dance+)
    (format stream "You are feet are dancing out of your control!~%"))
  (when (affected-by-spell ch +song-weight-of-the-world+)
    (format stream "The weight of the world rests lightly upon your shoulders.~%"))
  (when (affected-by-spell ch +song-eagles-overture+)
    (format stream "Other are impressed by your beautiful voice.~%"))
  (when (affected-by-spell ch +song-fortissimo+)
    (format stream "Your voice reverberates with vigor!~%"))
  (when (affected-by-spell ch +song-regalers-rhapsody+)
    (format stream "A tune has soothed your hunger and thirst.~%"))
  (when (affected-by-spell ch +song-wounding-whispers+)
    (format stream "You are surrounded by whirling slivers of sound.~%")))

(defcommand (ch "affects") ()
  (let ((affs (with-output-to-string (str)
                (send-bad-char-affects ch str)
                (send-good-char-affects ch str))))
    (if (zerop (length affs))
        (send-to-char ch "You feel pretty normal.~%")
        (with-pagination ((link-of ch))
          (send-to-char ch "Current affects:~%~a" affs)))))

(defcommand (ch "score") ()
  (with-pagination ((link-of ch))
    (send-to-char ch "&r*****************************************************************~%")
    (send-to-char ch "&y***************************&YS C O R E&y*****************************~%")
    (send-to-char ch "&g*****************************************************************&n~%")

    (send-to-char ch "~a, ~d year old ~(~a~) ~a ~a.  Your level is ~a.~%"
                  (name-of ch)
                  (age ch)
                  (sex-of ch)
                  (aref +player-races+ (race-of ch))
                  (aref +class-names+ (char-class-of ch))
                  (level-of ch))
    (when (is-remort ch)
      (send-to-char ch "You have remortalized as a ~a (generation ~d)~%"
                    (aref +class-names+ (remort-char-class-of ch))
                    (remort-gen-of ch)))
    ;; TODO: add birthday calculation here
    (send-to-char ch "~%")
    (send-to-char ch "Hit Points:  (&y~d&n/&g~d&n)           Armor Class: &g~d/10&n~%"
                  (hitp-of ch)
                  (max-hitp-of ch)
                  (armor-of ch))
    (send-to-char ch "Mana Points: (&y~d&n/&g~d&n)           Alignment: &g~d&n~%"
                  (mana-of ch)
                  (max-mana-of ch)
                  (alignment-of ch))
    (send-to-char ch "Move Points: (&y~d&n/&g~d&n)           Experience: &g~d&n~%"
                  (move-of ch)
                  (max-move-of ch)
                  (exp-of ch))
    (send-to-char ch "                                   &yKills&n: ~d, &rPKills&n: ~d, &gArena&n: ~d~%"
                  (mobkills-of ch)
                  (pkills-of ch)
                  (akills-of ch))
    (send-to-char ch "&r*****************************************************************&n~%")
    (send-to-char ch "You have &c~d&n life points.~%" (life-points-of ch))
    (send-to-char ch "You are &c~d&n cm tall, and weigh &c~d&n pounds.~%"
                  (height-of ch)
                  (weight-of ch))
    (unless (is-npc ch)
      (if (immortal-level-p ch)
          (send-to-char ch "&cPoofout:&n  ~a~%&cPoofin :&n  ~a~%"
                        (poofout-of ch)
                        (poofin-of ch))
          (send-to-char ch "You need &c~d&n exp to reach your next level.~%"
                        (- (aref +exp-scale+ (1+ (level-of ch))) (exp-of ch))))

      (multiple-value-bind (days secs)
          (floor (+ (timestamp-difference (now) (login-time-of ch))
                    (played-time-of ch))
                 86400)
        (multiple-value-bind (hours secs)
            (floor secs 3600)
          (declare (ignore secs))
          (send-to-char ch "You have existed here for ~d days and ~d hours.~%"
                        days hours)))

      (send-to-char ch "You are known as &y~a~@[~a~]&n.~%"
                    (name-of ch)
                    (title-of ch))

      (send-to-char ch "You have a reputation of being -~a-~%"
                    (aref +reputation-msg+ (reputation-rank ch))))

    (send-to-char ch "You are currently speaking ~a.~%"
                  (name-of (gethash (current-tongue-of ch) *tongues*)))

    (send-to-char ch "You carry &c~d&n gold coins.  You have &c~d&n cash credits.~%"
                  (gold-of ch)
                  (cash-of ch))
    (cond
      ((= (position-of ch) +pos-dead+)
       (send-to-char ch "&rYou are DEAD!&n~%"))
      ((= (position-of ch) +pos-mortallyw+)
       (send-to-char ch "&rYou are mortally wounded!  You should seek help!&n~%"))
      ((= (position-of ch) +pos-incap+)
       (send-to-char ch "&rYou are incapacitated, slowly fading away...&n~%"))
      ((= (position-of ch) +pos-stunned+)
       (send-to-char ch "&rYou are stunned!  You can't move!&n~%"))
      ((and (= (position-of ch) +pos-sleeping+)
            (aff3-flagged ch +aff3-stasis+))
       (send-to-char ch "You are inactive in a static state.~%"))
      ((= (position-of ch) +pos-sleeping+)
       (send-to-char ch "You are sleeping.~%"))
      ((= (position-of ch) +pos-resting+)
       (send-to-char ch "You are resting.~%"))
      ((and (= (position-of ch) +pos-sitting+)
            (aff2-flagged ch +aff2-meditate+))
       (send-to-char ch "You are meditating.~%"))
      ((= (position-of ch) +pos-sitting+)
       (send-to-char ch "You are sitting.~%"))
      ((and (= (position-of ch) +pos-fighting+)
            (fighting-of ch))
       (send-to-char ch "&yYou are fighting ~a.&n~%" (name-of (first (fighting-of ch)))))
      ((= (position-of ch) +pos-fighting+)
       (send-to-char ch "&yYou are fighting thin air.&n~%"))
      ((and (= (position-of ch) +pos-mounted+)
            (mounted-of ch))
       (send-to-char ch "&gYou are mounted on ~a.&n~%" (name-of (mounted-of ch))))
      ((= (position-of ch) +pos-mounted+)
       (send-to-char ch "&gYou are mounted on the thin air!?&n~%"))
      ((= (position-of ch) +pos-standing+)
       (send-to-char ch "&gYou are standing.&n~%"))
      ((= (position-of ch) +pos-flying+)
       (send-to-char ch "&gYou are hovering in midair.&n~%"))
      (t
       (send-to-char ch "&gYou are floating.&n~%")))

    (when (> (get-condition ch +drunk+) 10)
      (send-to-char ch "You are intoxicated.~%"))
    (when (zerop (get-condition ch +full+))
      (send-to-char ch "You are hungry.~%"))
    (when (zerop (get-condition ch +thirst+))
      (send-to-char ch "You are thirsty.~%"))

    (when (plr-flagged ch +plr-mortalized+)
      (send-to-char ch "You are mortalized.~%"))

    (let ((affs (with-output-to-string (str)
                  (send-bad-char-affects ch str)
                  (unless (pref-flagged ch +pref-noaffects+)
                    (send-good-char-affects ch str)))))
      (send-to-char ch "~a" affs))))

(defcommand (ch "time") ()
  (if (eql (time-frame-of (zone-of (in-room-of ch))) +time-timeless+)
      (send-to-char ch "Time has no meaning here.~%")
      (multiple-value-bind (hour day month year)
          (local-time-of (zone-of (in-room-of ch)))
        (send-to-char ch "It is ~d o'clock ~a, on the ~d~a day of the ~a, Year ~d.~%"
                      (if (zerop (mod hour 12)) 12 (mod hour 12))
                      (if (>= hour 12) "pm" "am")
                      (1+ day)
                      (case (mod (1+ day) 10)
                        (1 "st")
                        (2 "nd")
                        (3 "rd")
                        (t "th"))
                      (aref +month-name+ month)
                      year)
        (send-to-char ch "The moon is currently ~a.~%"
                      (aref +lunar-phases+ (lunar-phase *lunar-day*))))))

(defcommand (ch "attributes") ()
  (send-to-char ch "~a" (describe-attributes ch)))