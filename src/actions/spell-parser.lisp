(in-package :tempus)

(defclass spell-info ()
  ((idnum :accessor idnum-of :initarg :idnum :initform 0)
   (name :accessor name-of :initarg :name :initform "!UNUSED!")
   (min-position :accessor min-position-of :initarg :min-position :initform 0)
   (mana-min :accessor mana-min-of :initarg :mana-min :initform 0)
   (mana-max :accessor mana-max-of :initarg :mana-max :initform 0)
   (mana-change :accessor mana-change-of :initarg :mana-change :initform 0)
   (min-level :accessor min-level-of :initarg :min-level
              :initform (make-array +num-classes+ :initial-element 73))
   (min-gen :accessor min-gen-of :initarg :min-gen
              :initform (make-array +num-classes+ :initial-element 0))
   (routines :accessor routines-of :initarg :routines :initform 0)
   (violentp :accessor violentp :initarg :violentp :initform nil)
   (targets :accessor targets-of :initarg :targets :initform 0)
   (song-kind :accessor song-kind-of :initarg :song-kind :initform nil)
   (lyrics :accessor lyrics-of :initarg :lyrics :initform nil)
   (instrumentalp :accessor instrumentalp :initarg :instrumentalp :initform nil)
   (wearoff-msg :accessor wearoff-msg-of :initarg :wearoff-msg :initform nil)
   (func :accessor func-of :initarg :func :initform nil)))

(defun spell-mana-cost (ch spellnum)
  (let* ((sinfo (aref *spell-info* spellnum))
         (mana (max (mana-min-of sinfo)
                    (- (mana-max-of sinfo)
                       (* (mana-change-of sinfo)
                          (aref (min-level-of sinfo)
                                (if (< (char-class-of ch) 17)
                                    (char-class-of ch) 4)))))))
    (if (is-remort ch)
        (min (max (mana-min-of sinfo)
                    (- (mana-max-of sinfo)
                       (* (mana-change-of sinfo)
                          (aref (min-level-of sinfo)
                                (if (< (remort-char-class-of ch) 17)
                                    (remort-char-class-of ch) 4)))))
             mana)
        mana)))
(defun pad-song (lyrics)
  (let ((indent (- (position #\" lyrics) 4)))
    (cl-ppcre:regex-replace-all "\\n" lyrics (format nil "~%~vt" indent))))

(defun say-spell (ch spellnum tch tobj)
  (let ((to-char nil) (to-vict nil) (to-room nil))
    (cond
      ((spell-is-psionic spellnum)
       (cond
         ((and tobj (eql (in-room-of tobj) (in-room-of ch)))
          (setf to-char "You close your eyes and touch $p with a mental finger."
                to-room "$n momentarily closes $s eyes and concentrates."))
         ((or (null tch) (not (eql (in-room-of tch) (in-room-of ch))))
          (setf to-char "You close your eyes and slip into a psychic world."
                to-room "$n momentarily closes $s eyes and concentrates."))
         ((eql ch tch)
          (setf to-char "You close your eyes and concentrate."
                to-room "$n momentarily closes $s eyes and concentrates."))
         (t
          (setf to-char "You close your eyes and touch $N with a mental finger."
                to-vict "$n closes $s eyes and connects with your mind."
                to-room "$n momentarily closes $s eyes and concentrates."))))
      ((spell-is-physics spellnum)
       (cond
         ((and tobj (eql (in-room-of tobj) (in-room-of ch)))
          (setf to-char "You look directly at $p and make a calculation."
                to-room "$n looks directly at $p and makes a calculation."))
         ((or (null tch) (not (eql (in-room-of tch) (in-room-of ch))))
          (setf to-char "You close your eyes and slip into a deep calculation."
                to-room "$n closes $s eyes and makes a deep calculation."))
         ((eql ch tch)
          (setf to-char "You close your eyes and make a calculation."
                to-room "$n momentarily closes $s eyes and concentrates."))
         (t
          (setf to-char "You look at $N and make a calculation."
                to-vict "$n closes $s eyes and alters the reality around you."
                to-room "$n looks at $N and makes a calculation."))))
      ((spell-is-bardic spellnum)
         (let ((to-target (cond
                            (tobj
                             " to $p")
                            ((or (null tch)
                                 (eql tch ch))
                             "")
                            (t
                             " to $N")))
               (lyrics (lyrics-of (aref *spell-info* spellnum))))
           (if (instrumentalp (aref *spell-info* spellnum))
               (setf to-char (format nil "You begin to play ~a~a." lyrics to-target)
                     to-vict (if (eql tch ch) nil (format nil "$n begins playing ~a~a." lyrics to-target))
                     to-room (format nil "$n begins playing ~a~a." lyrics to-target))
               (setf to-char (pad-song (format nil "You sing~a, &c\"~a\"&n" to-target lyrics))
                     to-vict (if (eql tch ch) nil(pad-song (format nil "$n sings~a, &c\"~a\"&n" to-target lyrics)))
                     to-room (pad-song (format nil "$n sings~a, &c\"~a\"&n" to-target lyrics))))))
      (t
       (let ((spell-name (translate-with-tongue (gethash 1 *tongues*)
                                                (spell-to-str spellnum)
                                                0)))
         (cond
           ((and tobj (eql (in-room-of tobj) (in-room-of ch)))
            (setf to-char (format nil "You stare at $p and utter the words, '~a'." spell-name)
                  to-room (format nil "$n stares at $p and utters the words, '~a'." spell-name)))
           ((or (null tch) (not (eql (in-room-of tch) (in-room-of ch))))
            (setf to-char (format nil "You utter the words, '~a'." spell-name)
                  to-room (format nil "$n utters the words, '~a'." spell-name)))
           ((eql ch tch)
            (setf to-char (format nil "You close your eyes and utter the words, '~a'." spell-name)
                  to-room (format nil "$n closes $s eyes and utters the words, '~a'." spell-name)))
           (t
            (setf to-char (format nil "You stare at $N and utter, '~a'." spell-name)
                  to-vict (format nil "$n stares at you and utters, '~a'." spell-name)
                  to-room (format nil "$n stares at $N and utters, '~a'." spell-name)))))))
    (act ch :target tch :item tobj
         :subject-emit to-char
         :target-emit to-vict
         :not-target-emit to-room)))

(defun parse-spell-name (args)
  "Given the arguments to a casting command, returns the spellnum of
  the spell to cast and the arguments to the spell as a string.
  Returns NIL when no spell matches"
  (let* ((quote-pos-1 (position #\' args))
         (quote-pos-2 (and quote-pos-1 (position #\' args :start (1+ quote-pos-1)))))
    (when quote-pos-1
      (values
       (position (subseq args (1+ quote-pos-1) quote-pos-2)
                         *spell-info*
                         :key 'name-of
                         :test 'string-abbrev)
       (if quote-pos-2 (string-trim " " (subseq args (1+ quote-pos-2))) "")))))

(defun stats-can-learn-spell (level gen char-class remort-class spellnum)
  (let ((info (aref *spell-info* spellnum)))
    (or (and (>= level (aref (min-level-of info) char-class))
             (>= gen (aref (min-gen-of info) char-class)))
        (and (plusp gen)
             (>= level (aref (min-level-of info) remort-class))
             (zerop (aref (min-gen-of info) char-class))))))

(defun can-learn-spell (ch spellnum)
  (stats-can-learn-spell (level-of ch)
                         (remort-gen-of ch)
                         (char-class-of ch)
                         (remort-char-class-of ch)
                         spellnum))

(defun spell-kind-desc (spellnum)
  (let ((info (aref *spell-info* spellnum)))
    (cond
      ((logtest +mag-divine+ (routines-of info))
       "spell")
      ((logtest +mag-magic+ (routines-of info))
       "spell")
      ((logtest +mag-physics+ (routines-of info))
       "alteration")
      ((logtest +mag-psionic+ (routines-of info))
       "trigger")
      ((logtest +mag-bard+ (routines-of info))
       "song")
      (t
       "ability"))))

(defun find-spell-targets (ch args mode)
  "Given the caster, spell arguments, and mode of casting, returns the
spellnum and a list of targets to be affected by the spell.  If the
spell cannot be cast, returns NIL."
  (multiple-value-bind (spellnum spell-args)
      (parse-spell-name args)
    (let ((target-flags (and spellnum (targets-of (aref *spell-info* spellnum)))))
      (anaphora:acond
        ((null spellnum)
         (send-to-char ch "~a what?!?~%" mode)
         nil)
        ((and (can-learn-spell ch spellnum)
              (< (check-skill ch spellnum) 30))
         (send-to-char ch "You do not know that ~a!~%" (spell-kind-desc spellnum))
         nil)
        ((zerop (check-skill ch spellnum))
         (send-to-char ch "You are unfamiliar with that ~a.~%" (spell-kind-desc spellnum))
         nil)
        ((logtest target-flags +tar-ignore+)
         (values spellnum nil))
        ((logtest (flags-of (aref *spell-info* spellnum)) +mag-groups+)
         (values spellnum (present-group-members ch)))
        ((logtest (flags-of (aref *spell-info* spellnum)) +mag-masses+)
         (values spellnum (fighting-of ch)))
        ((logtest (flags-of (aref *spell-info* spellnum)) +mag-areas+)
         (values spellnum ()))
        ((string= spell-args "")
         (cond
           ((logtest target-flags +tar-fight-self+)
            (values t ch))
           ((and (logtest target-flags +tar-fight-vict+)
                 (fighting-of ch))
            (values spellnum (random-elt (fighting-of ch))))
           ((and (logtest target-flags +tar-char-room+)
                 (not (violentp (aref *spell-info* spellnum)))
                 (not (logtest target-flags +tar-unpleasant+)))
            (values spellnum ch))
           (t
            (send-to-char ch "Upon ~a should the spell be cast?~%"
                          (if (logtest target-flags
                                       (logior +tar-obj-room+ +tar-obj-inv+ +tar-obj-world+))
                              "what" "whom"))
            nil)))
        ((and (logtest target-flags +tar-dir+)
              (position spell-args +dirs+ :test 'string-abbrev))
         (values spellnum nil nil anaphora:it))
        ((and (logtest target-flags +tar-char-room+)
              (resolve-alias ch spell-args (people-of (in-room-of ch))))
         (values spellnum (first anaphora:it)))
        ((and (logtest target-flags +tar-char-world+)
              (resolve-alias ch spell-args *characters*))
         (values spellnum (first anaphora:it)))
        ((and (logtest target-flags +tar-obj-inv+)
              (resolve-alias ch spell-args (carrying-of ch)))
         (values spellnum nil (first anaphora:it)))
        ((and (logtest target-flags +tar-obj-equip+)
              (resolve-alias ch spell-args (coerce (remove nil (equipment-of ch)) 'list)))
         (values spellnum nil (first anaphora:it)))
        ((and (logtest target-flags +tar-obj-room+)
              (resolve-alias ch spell-args (contents-of (in-room-of ch))))
         (values spellnum nil (first anaphora:it)))
        ((and (logtest target-flags +tar-obj-world+)
              (resolve-alias ch spell-args *object-list*))
         (values spellnum nil (first anaphora:it)))
        ((logtest target-flags +tar-door+)
         (let ((door (find-door ch spell-args "cast")))
           (and door (values spellnum nil nil door))))
        (t
         (send-to-char ch "Cannot find the target of your spell!~%")
         nil)))))

(defun calc-failure-probability (ch spellnum)
  (let ((failure-prob 0))
    (when (and (or (is-cleric ch)
                   (is-knight ch))
               (spell-is-divine spellnum))
      (decf failure-prob (+ (wis-of ch)
                    (abs (floor (alignment-of ch)
                                70))))
      (when (is-neutral ch)
        (incf failure-prob 30)))
    (when (or (is-mage ch)
              (is-ranger ch))
      (decf failure-prob (+ (int-of ch) (dex-of ch))))

    (when (is-sick ch)
      (incf failure-prob 20))
    (when (is-confused ch)
      (incf failure-prob (- (random-range 35 55) (int-of ch))))
    (when (and (not (immortalp ch))
               (get-eq ch +wear-shield+))
      (incf failure-prob (weight-of (get-eq ch +wear-shield+))))

    (incf failure-prob (floor (* (+ (carry-weight-of ch) (worn-weight-of ch)) 8)
                              (can-carry-weight ch)))

    (let ((num-eq 0)
          (metal-weight 0))
      (loop for eq across (equipment-of ch)
           for implant across (implants-of ch)
           do
           (when eq
             (incf num-eq)
             (when (is-metal eq)
               (incf metal-weight (weight-of eq))))
           (when (and implant (is-metal implant))
             (incf metal-weight (weight-of implant))))
      (incf failure-prob (- +num-wears+ num-eq))
      (when (and (or (is-mage ch) (is-ranger ch))
                 (spell-is-magic spellnum))
        (incf failure-prob metal-weight)))

    failure-prob))

(defun spell-fumble (ch tch tobj spellnum)
  (act ch :all-emit "$n fumble$% $s spell!"))

(defun cast-spell (ch tch tobj tdir spellnum)
  (let ((mana-cost (spell-mana-cost ch spellnum)))
    (cond
      ((not (can-cast-spell ch (or tch tobj) spellnum mana-cost))
       nil)
      ((> (+ (calc-failure-probability ch spellnum)
             (random-range 0 75))
          (check-skill ch spellnum))
       (spell-fumble ch tch tobj spellnum))
      (t
       (wait-state ch (min 0 (- (rl-sec 3) (if (is-mage ch) (remort-gen-of ch) 0))))
       (when (plusp mana-cost)
         (setf (mana-of ch) (pin (- (mana-of ch) mana-cost) 0 (max-mana-of ch))))
       (gain-skill-proficiency ch spellnum)
       (say-spell ch spellnum tch tobj)
       (call-magic ch tch tobj tdir spellnum (+ (level-of ch)
                                                (floor (remort-gen-of ch) 2))
                   (cond
                     ((spell-is-psionic spellnum)
                      +cast-psionic+)
                     ((spell-is-physics spellnum)
                      +cast-physic+)
                     ((spell-is-bardic spellnum)
                      +cast-bard+)
                     (t
                      +cast-spell+)))))))

(defun can-cast-spell (ch target spellnum mana-cost)
  (let ((info (aref *spell-info* spellnum))
        (spell-desc (cond
                      ((spell-is-psionic spellnum) "trigger")
                      ((spell-is-physics spellnum) "alteration")
                      ((spell-is-bardic spellnum) "song")
                      (t "spell"))))
    (cond
      ((and (spell-is-magic spellnum)
            (not (or (is-mage ch)
                     (is-cleric ch)
                     (is-knight ch)
                     (is-ranger ch)
                     (immortalp ch))))
       (send-to-char ch "You are not learned in the ways of magic.~%")
       nil)
      ((and (spell-is-psionic spellnum)
            (not (or (is-psionic ch)
                     (immortalp ch))))
       (send-to-char ch "You are not able to trigger the mind's potential.~%")
       nil)
      ((> (worn-weight-of ch) (* (can-carry-weight ch) 0.9))
       (cond
         ((spell-is-psionic spellnum)
          (send-to-char ch "Your equipment is too heavy and bulky to concentrate!~%"))
         ((spell-is-physics spellnum)
          (send-to-char ch "Your equipment is too heavy and bulky to do the calculations!~%"))
         ((spell-is-bardic spellnum)
          (send-to-char ch "You're carrying too much to perform!~%"))
         (t
          (send-to-char ch "Your equipment is too heavy and bulky to cast anything useful!~%")))
       nil)
      ((and (spell-is-magic spellnum)
            (get-eq ch +wear-wield+)
            (is-obj-stat2 (get-eq ch +wear-wield+) +item2-two-handed+))
       (send-to-char ch "You can't cast spells while wielding a two handed weapon!~%")
       nil)
      ((and (spell-is-magic spellnum)
            (get-eq ch +wear-wield-2+))
       (send-to-char ch "You can't cast spells while dual-wielding!~%")
       nil)
      ((and (room-flagged (in-room-of ch) +room-nomagic+)
            (not (immortalp ch))
            (or (spell-is-magic spellnum)
                (spell-is-divine spellnum)))
       (act ch
            :subject-emit "Your magic fizzles out and dies."
            :place-emit "$n's magic fizzles out and dies.")
       nil)
      ((and (spell-is-psionic spellnum)
            (room-flagged (in-room-of ch) +room-nopsionics+)
            (not (immortalp ch)))
       (send-to-char ch "You cannot establish a mental link.~%")
       nil)
      ((and (spell-is-psionic spellnum)
            (mindlessp target))
       (act ch :target target
            :subject-emit "You cannot make a mindlink with $N!")
       nil)
      ((and (> (get-condition ch +drunk+) 5)
            (> (random-range 1 35) (int-of ch)))
       (cond
         ((spell-is-psionic spellnum)
          (send-to-char ch "Your mind is too clouded to trigger any points!"))
         ((spell-is-physics spellnum)
          (send-to-char ch "Your mind is too clouded to do complex mathematics!"))
         ((spell-is-bardic spellnum)
          (send-to-char ch "You're too drunk to remember the tune.~%"))
         (t
          (send-to-char ch "Your mind is too clouded to cast any spells.~%")))
       nil)
      ((and (plusp mana-cost)
            (> mana-cost (mana-of ch))
            (not (immortalp ch)))
       (cond
         ((spell-is-psionic spellnum)
          (send-to-char ch "You haven't the energy to trigger that potential!~%"))
         ((spell-is-physics spellnum)
          (send-to-char ch "You haven't the energy to make that alteration!~%"))
         ((spell-is-bardic spellnum)
          (send-to-char ch "You haven't the energy to sing that song!~%"))
         (t
          (send-to-char ch "You haven't the energy to cast that spell!~%")))
       nil)
      ((and (not (immortalp ch))
            (or (and (not (is-evil ch)) (spell-is-evil spellnum))
                (and (not (is-good ch)) (spell-is-good spellnum))))
       (send-to-char ch "You cannot cast that spell.~%")
       nil)
      ((< (position-of ch) (min-position-of (aref *spell-info* spellnum)))
       (cond
         ((= (position-of ch) +pos-sleeping+)
          (cond
            ((spell-is-physics spellnum)
             (send-to-char ch "You dream about great physics theories.~%"))
            ((spell-is-psionic spellnum)
             (send-to-char ch "You dream about great psionic powers.~%"))
            ((spell-is-bardic spellnum)
             (send-to-char ch "You dream about great musical talent.~%"))
            ((spell-is-divine spellnum)
             (send-to-char ch "You dream about great spiritual experiences.~%"))
            (t
             (send-to-char ch "You dream about great magical powers.~%"))))
         ((= (position-of ch) +pos-resting+)
          (send-to-char ch "You cannot concentrate while resting.~%"))
         ((= (position-of ch) +pos-sitting+)
          (send-to-char ch "You can't do this sitting!~%"))
         ((= (position-of ch) +pos-fighting+)
          (send-to-char ch "Impossible!  You can't concentrate enough!~%"))
         (t
          (send-to-char ch "You can't do much of anything like this!~%")))
       nil)
      ((and (aff-flagged ch +aff-charm+) (eql (master-of ch) target))
       (send-to-char ch "You are afraid you might hurt your master!~%")
       nil)
      ((and (not (eql target ch))
            (logtest (targets-of info) +tar-self-only+)
            (not (immortalp ch)))
       (send-to-char ch "You can only ~a yourself!~%"
                     (cond
                       ((spell-is-physics spellnum)
                        "alter this reality on")
                       ((spell-is-psionic spellnum)
                        "trigger this psi on")
                       ((spell-is-mercenary spellnum)
                        "apply this device to")
                       ((spell-is-bardic spellnum)
                        "evoke this song on")
                       (t
                        "cast this spell upon")))
       nil)
      ((and (eql target ch)
            (logtest (targets-of info) +tar-not-self+)
            (not (immortalp ch)))
       (send-to-char ch "You cannot ~a yourself!~%"
                     (cond
                       ((spell-is-physics spellnum)
                        "alter this reality on")
                       ((spell-is-psionic spellnum)
                        "trigger this psi on")
                       ((spell-is-mercenary spellnum)
                        "apply this device to")
                       ((spell-is-bardic spellnum)
                        "evoke this song on")
                       (t
                        "cast this spell upon")))
       nil)
      ((and (logtest (routines-of info) +mag-groups+)
            (not (aff-flagged ch +aff-group+)))
       (send-to-char ch "You can't do this if you're not in a group!~%")
       nil)
      ((and (room-is-underwater (in-room-of ch))
            (spell-flagged spellnum +mag-nowater+))
       (send-to-char ch "This ~a does not function underwater.~%" spell-desc)
       nil)
      ((and (not (room-is-outside (in-room-of ch)))
            (spell-flagged spellnum +mag-outdoors+))
       (send-to-char ch "This ~a can only be used outdoors.~%" spell-desc)
       nil)
      ((and (spell-flagged spellnum +mag-nosun+)
            (room-is-sunny (in-room-of ch)))
       (send-to-char ch "This ~a cannot be used in sunlight.~%" spell-desc)
       nil)
      ((and (null target)
            (not (logtest (targets-of (aref *spell-info* spellnum)) (logior +tar-ignore+ +tar-door+))))
       (cond
         ((spell-is-psionic spellnum)
          (send-to-char ch "Cannot find the target of your trigger!"))
         ((spell-is-physics spellnum)
          (send-to-char ch "Cannot find the target of your alteration!~%"))
         ((spell-is-bardic spellnum)
          (send-to-char ch "Cannot find the target of your song!~%"))
         (t
          (send-to-char ch "Cannot find the target of your spell!~%")))
       nil)
      (t
       ;; Passed all tests
       t))))


(defcommand (ch "cast") (:standing)
  (send-to-char ch "You were going to cast something?~%"))

(defcommand (ch "cast" args) (:standing)
  (multiple-value-bind (spellnum tch tobj tdir)
      (find-spell-targets ch args "cast")
    (when spellnum
      (cast-spell ch tch tobj tdir spellnum))))

(defcommand (ch "trigger") (:standing)
  (send-to-char ch "You were going to trigger a psionic potential?~%"))

(defcommand (ch "trigger" args) (:standing)
  (multiple-value-bind (spellnum tch tobj tdir)
      (find-spell-targets ch args "trigger")
    (when spellnum
      (cast-spell ch tch tobj tdir spellnum))))

(defcommand (ch "alter") (:standing)
  (send-to-char ch "You were going to alter a physical law?~%"))

(defcommand (ch "alter" args) (:standing)
  (multiple-value-bind (spellnum tch tobj tdir)
      (find-spell-targets ch args "alter")
    (when spellnum
      (cast-spell ch tch tobj tdir spellnum))))

(defcommand (ch "perform") (:standing)
  (send-to-char ch "You were going to perform something?~%"))

(defcommand (ch "perform" args) (:standing)
  (multiple-value-bind (spellnum tch tobj tdir)
      (find-spell-targets ch args "perform")
    (when spellnum
      (cast-spell ch tch tobj tdir spellnum))))

(defun clear-spells ()
  (dotimes (idx 1000)
    (if (aref *spell-info* idx)
        (reinitialize-instance (aref *spell-info* idx)
                               :idnum idx
                               :func (func-of (aref *spell-info* idx)))
        (setf (aref *spell-info* idx) (make-instance 'spell-info :idnum idx))))
  (setf (name-of (aref *spell-info* 0)) "!RESERVED!"))

(defun apply-attribute-to-spell (spell child)
  (string-case (first child)
    ("granted"
     (let ((char-class (parse-player-class (xml-attr child "class")))
           (level (xml-attr child "level" :numeric t))
           (gen (xml-attr child "gen" :numeric t :default 0)))
       (assert char-class nil
               "Granted class '~a' is not a valid class in spell!"
               (xml-attr child "class"))
       (assert (<= 1 level +lvl-ambassador+) nil
               "Granted level ~d is not a valid level" level)
       (assert (<= 0 gen 10) nil
               "Granted gen ~d is not a valid gen" level)
       (setf (aref (min-level-of spell) char-class) level)
       (setf (aref (min-gen-of spell) char-class) gen)))
    ("manacost"
     (setf (mana-max-of spell) (xml-attr child "initial" :numeric t))
     (setf (mana-change-of spell) (xml-attr child "level_dec" :numeric t))
     (setf (mana-min-of spell) (xml-attr child "minimum" :numeric t)))
    ("position"
     (let ((min (xml-attr child "minimum")))
       (assert min nil "Required property minimum missing from position element.")
       (let ((pos (position min +position-types+ :test #'string-equal)))
         (assert pos nil "Invalid minimum position '~a' for spell" min)
         (setf (min-position-of spell) pos))))
    ("target"
     (let ((type-str (xml-attr child "type"))
           (scope-str (xml-attr child "scope")))
       (setf (targets-of spell)
             (logior (targets-of spell)
                     (string-case type-str
                       ("door" +tar-door+)
                       ("direction" +tar-dir+)
                       ("self"
                        (string-case scope-str
                          ("fighting" +tar-fight-self+)
                          ("only" +tar-self-only+)
                          ("never" +tar-not-self+)
                          (t 0)))
                       ("creature"
                        (string-case scope-str
                          ("room" +tar-char-room+)
                          ("world" +tar-char-world+)
                          ("fighting" +tar-fight-vict+)
                          (t 0)))
                       ("object"
                        (string-case scope-str
                          ("room" +tar-obj-room+)
                          ("world" +tar-obj-world+)
                          ("inventory" +tar-obj-inv+)
                          ("equip" +tar-obj-equip+)
                          (t 0)))
                       (t 0))))))
    ("flag"
     (let ((value-str (xml-attr child "value")))
       (string-case value-str
         ("violent"
          (setf (violentp spell) t))
         ("unpleasant"
          (setf (targets-of spell) (logior (targets-of spell)
                                           +tar-unpleasant+)))
         (t
          (let ((flag (position value-str +spell-bit-keywords+ :test #'string-equal)))
            (assert flag nil "Invalid flag '~a' in spell" value-str)
            (setf (routines-of spell) (logior (routines-of spell)
                                              (ash 1 flag))))))))
    ("instrument"
     (string-case (xml-attr child "type")
       ("wind"
        (setf (song-kind-of spell) 'wind))
       ("percussion"
        (setf (song-kind-of spell) 'percussion))
       ("string"
        (setf (song-kind-of spell) 'string))
       (t
        (error "Invalid instrument type ~s in spell" (xml-attr child "type")))))
    ("description"
     (setf (lyrics-of spell) (third child))
     (setf (instrumentalp spell) t))
    ("lyrics"
     (setf (lyrics-of spell) (third child))
     (setf (instrumentalp spell) nil))))

(defun load-spell (node)
  (let* ((idnum (xml-attr node "id" :numeric t))
         (spell (aref *spell-info* idnum)))
    ;; for defined classes, initialize minimum level to ambassador
    (dotimes (idx +num-classes+)
      (setf (aref (min-level-of spell) idx) +lvl-ambassador+))

    (setf (name-of spell) (xml-attr node "name"))
    (dolist (child (cddr node))
      (when (listp child)
        (apply-attribute-to-spell spell child)))
    (when (null (func-of spell))
      (slog "WARNING: Spell ~a not implemented" (name-of spell)))
    (when (zerop (targets-of spell))
      (setf (targets-of spell) +tar-ignore+))))

(defun boot-spells ()
  (clear-spells)
  (let ((xml (cxml:parse-file (tempus-path "lib/etc/spells.xml") (cxml-xmls:make-xmls-builder))))
    (assert xml nil "Empty spells.xml file")
    (dolist (node (cddr xml))
      (when (and (listp node)
                 (or (string= (first node) "spell")
                     (string= (first node) "skill")))
        (load-spell node)))))


(defun call-magic (ch vict ovict dvict spellnum level casttype)
  (let* ((info (aref *spell-info* spellnum))
         (func (and info (func-of info))))
    (cond
      ((null info)
       (when ch
         (send-to-char ch "Oops, that spell doesn't exist.~%")))
      ((null func)
       (when ch
         (send-to-char ch "Oops, that spell isn't implemented.~%")))
      (t
       (let ((saved (and
                     vict
                     (not (eql ch vict))
                     (mag-savingthrow vict level
                                     (or (cdr (assoc casttype
                                                     `((,+cast-staff+ . ,+saving-rod+)
                                                       (,+cast-scroll+ . ,+saving-rod+)
                                                       (,+cast-potion+ . ,+saving-rod+)
                                                       (,+cast-wand+ . ,+saving-rod+)
                                                       (,+cast-psionic+ . ,+saving-psi+)
                                                       (,+cast-physic+ . ,+saving-phy+)
                                                       (,+cast-chem+ . ,+saving-chem+)
                                                       (,+cast-para+ . ,+saving-para+)
                                                       (,+cast-petri+ . ,+saving-petri+)
                                                       (,+cast-breath+ . ,+saving-breath+)
                                                       (,+cast-bard+ . ,+saving-breath+)
                                                       (,+cast-internal+ . ,+saving-none+))))
                                         +saving-breath+)))))
         (funcall func ch (or vict ovict dvict) level saved))
       (when (and (not (is-dead vict))
                  (not (is-dead ch))
                  (violentp info)
                  vict
                  (not (eql vict ch)))
         (pushnew vict (fighting-of ch)))))))

(defun mag-objectmagic (ch object arg)
  nil)