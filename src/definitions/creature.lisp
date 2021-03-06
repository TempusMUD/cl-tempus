(in-package :tempus)

(declaim (ftype (function (t) (values fixnum))
                level-of
                position-of
                max-hitp-of max-mana-of max-move-of
                hitp-of mana-of move-of
                str-of str-add-of int-of wis-of dex-of con-of cha-of))

(defclass char-ability-data ()
  ((str :accessor str-of :initarg :str :initform 11 :type (integer 0 25))
   (str-add :accessor str-add-of :initarg :str-add :initform 0 :type (integer 0 100))
   (int :accessor int-of :initarg :int :initform 11 :type (integer 0 25))
   (wis :accessor wis-of :initarg :wis :initform 11 :type (integer 0 25))
   (dex :accessor dex-of :initarg :dex :initform 11 :type (integer 0 25))
   (con :accessor con-of :initarg :con :initform 11 :type (integer 0 25))
   (cha :accessor cha-of :initarg :cha :initform 11 :type (integer 0 25))))

(defclass mob-shared-data ()
  ((vnum :accessor vnum-of :initarg :vnum :initform nil :type (integer 0 100000))
   (svnum :accessor svnum-of :initarg :svnum :initform nil :type (integer 0 100000))
   (number :accessor number-of :initarg :number :initform 0)
   (attack-type :accessor attack-type-of :initarg :attack-type :initform nil)
   (lair :accessor lair-of :initarg :lair :initform 0)
   (leader :accessor leader-of :initarg :leader :initform 0)
   (kills :accessor kills-of :initarg :kills :initform 0)
   (loaded :accessor loaded-of :initarg :loaded :initform 0)
   (default-pos :accessor default-pos-of :initarg :default-pos :initform nil)
   (damnodice :accessor damnodice-of :initarg :damnodice :initform nil)
   (damsizedice :accessor damsizedice-of :initarg :damsizedice :initform nil)
   (morale :accessor morale-of :initarg :morale :initform 100)
   (move-buf :accessor move-buf-of :initarg :move-buf :initform nil)
   (proto :accessor proto-of :initarg :proto :initform nil)
   (func :accessor func-of :initarg :func :initform nil)
   (func-param :accessor func-param-of :initarg :func-param :initform nil)
   (load-param :accessor load-param-of :initarg :load-param :initform nil)
   (prog-text :accessor prog-text-of :initarg :prog :initform nil)
   (progobj :accessor progobj-of :initarg :progobj :initform nil)
   (progobj-len :accessor progobj-len-of :initarg :progobj-len :initform nil)))

(defclass affected-type ()
  ((kind :accessor kind-of :initarg :kind :initform nil :type (integer 0 2048))
   (duration :accessor duration-of :initarg :duration :initform 0 :type (integer 0 2048))
   (modifier :accessor modifier-of :initarg :modifier :initform 0 :type (integer -10000 10000))
   (location :accessor location-of :initarg :location :initform 0)
   (level :accessor level-of :initarg :level :initform 0)
   (is-instant :accessor is-instant-of :initarg :is-instant :initform nil :type boolean)
   (bitvector :accessor bitvector-of :initarg :bitvector :initform 0 :type (unsigned-byte 32))
   (aff-index :accessor aff-index-of :initarg :aff-index :initform 0 :type (integer 1 3))
   (owner :accessor owner-of :initarg :owner :initform nil)))


(defclass grievance ()
  ((kind :accessor kind-of :initarg :kind)
   (player-idnum :accessor player-idnum-of :initarg :player-idnum)
   (time :accessor time-of :initarg :time)
   (reputation :accessor reputation-of :initarg :reputation)))

(defclass creature ()
  ((in-room :accessor in-room-of :initarg :in-room :initform nil)
   (fighting :accessor fighting-of :initarg :fighting :initform nil)
   (real-abils :accessor real-abils-of :initarg :real-abils
               :initform (make-instance 'char-ability-data))
   (aff-abils :accessor aff-abils-of :initarg :aff-abils
               :initform (make-instance 'char-ability-data))
   (affected :accessor affected-of :initarg :affected :initform nil)
   (equipment :accessor equipment-of :initarg :equipment :initform (make-array +num-wears+ :initial-element nil))
   (implants :accessor implants-of :initarg :implants :initform (make-array +num-wears+ :initial-element nil))
   (tattoos :accessor tattoos-of :initarg :tattoos :initform (make-array +num-wears+ :initial-element nil))
   (soilage :accessor soilage-of :initarg :soilage :initform (make-array +num-wears+ :initial-element 0))
   (carrying :accessor carrying-of :initarg :carrying :initform nil)
   (link :accessor link-of :initarg :link :initform nil)
   (followers :accessor followers-of :initarg :followers :initform nil)
   (master :accessor master-of :initarg :master :initform nil)

   ;; Originally from char_point_data
   (mana :accessor mana-of :initarg :mana :initform 100 :type fixnum)
   (max-mana :accessor max-mana-of :initarg :max-mana :initform 100 :type fixnum)
   (hitp :accessor hitp-of :initarg :hitp :initform 100 :type fixnum)
   (max-hitp :accessor max-hitp-of :initarg :max-hitp :initform 100 :type fixnum)
   (move :accessor move-of :initarg :move :initform 100 :type fixnum)
   (max-move :accessor max-move-of :initarg :max-move :initform 100 :type fixnum)
   (armor :accessor armor-of :initarg :armor :initform 100 :type fixnum)
   (gold :accessor gold-of :initarg :gold :initform 0)
   (cash :accessor cash-of :initarg :cash :initform 0)
   (exp :accessor exp-of :initarg :exp :initform 0)
   (hitroll :accessor hitroll-of :initarg :hitroll :initform 0 :type fixnum)
   (damroll :accessor damroll-of :initarg :damroll :initform 0 :type fixnum)
   (conditions :accessor conditions-of :initarg :conditions :initform (make-array 3 :element-type '(integer -1 24) :initial-contents '(0 23 23)))

   ;; Originally from char_special_data_saved
   (idnum :accessor idnum-of :initarg :idnum :initform nil :type fixnum)
   (remort-gen :accessor remort-gen-of :initarg :remort-gen :initform 0)
   (saves :accessor saves-of :initarg :saves :initform (make-array 10))
   (aff-flags :accessor aff-flags-of :initarg :aff-flags :initform 0 :type (unsigned-byte 32))
   (aff2-flags :accessor aff2-flags-of :initarg :aff2-flags :initform 0 :type (unsigned-byte 32))
   (aff3-flags :accessor aff3-flags-of :initarg :aff3-flags :initform 0 :type (unsigned-byte 32))
   (mob-flags :accessor mob-flags-of :initarg :mob-flags :initform 0 :type (unsigned-byte 32))
   (mob2-flags :accessor mob2-flags-of :initarg :mob2-flags :initform 0 :type (unsigned-byte 32))
   (alignment :accessor alignment-of :initarg :alignment :initform 0)

   ;; Originally from char_player_data
   (name :accessor name-of :initarg :name :initform nil)
   (aliases :accessor aliases-of :initarg :aliases :initform nil)
   (ldesc :accessor ldesc-of :initarg :ldesc :initform nil)
   (fdesc :accessor fdesc-of :initarg :fdesc :initform nil)
   (char-class :accessor char-class-of :initarg :char-class :initform 0)
   (remort-char-class :accessor remort-char-class-of :initarg :remort-char-class :initform -1)
   (weight :accessor weight-of :initarg :weight :initform 200)
   (height :accessor height-of :initarg :height :initform 198)
   (sex :accessor sex-of :initarg :sex :initform 'male)
   (race :accessor race-of :initarg :race :initform 0)
   (level :accessor level-of :initarg :level :initform 0)
   (age-adjust :accessor age-adjust-of :initarg :age-adjust :initform 0)
   (age :accessor age-of :initarg :age :initform nil)
   (birth-time :accessor birth-time-of :initarg :birth-time :initform nil)
   (login-time :accessor login-time-of :initarg :birth-time :initform nil)
   (death-time :accessor death-time-of :initarg :birth-time :initform nil)
   (played-time :accessor played-time-of :initarg :birth-time :initform 0)

   ;; Originally from char_special_data
   (defending :accessor defending-of :initarg :defending :initform nil)
   (hunting :accessor hunting-of :initarg :hunting :initform nil)
   (mounted :accessor mounted-of :initarg :mounted :initform nil)
   (carry-weight :accessor carry-weight-of :initarg :carry-weight :initform 0)
   (worn-weight :accessor worn-weight-of :initarg :worn-weight :initform 0)
   (timer :accessor timer-of :initarg :timer :initform nil)
   (meditate-timer :accessor meditate-timer-of :initarg :meditate-timer :initform nil)
   (flow-pulse :accessor flow-pulse-of :initform 0)
   (breath-count :accessor breath-count-of :initform 0)
   (fall-count :accessor fall-count-of :initform 0)
   (position :accessor position-of :initarg :position :initform +pos-standing+ :type fixnum)
   (carry-items :accessor carry-items-of :initarg :carry-items :initform 0)
   (weapon-proficiency :accessor weapon-proficiency-of :initarg :weapon-proficiency :initform nil)
   (mood :accessor mood-of :initarg :mood :initform nil)
   (saved :accessor saved-of :initarg :saved :initform nil)

   ;; Originally from char_language_data
   (tongues :accessor tongues-of :initarg :tongues
            :initform (make-array +max-tongues+ :initial-element 0))
   (current-tongue :accessor current-tongue-of :initarg :current-tongue :initform 0)
   (last-command :accessor last-command-of :initform nil)
   (repeat-cmd-count :accessor repeat-cmd-count-of :initform 0)))

(defclass mobile (creature)
  ((shared :accessor shared-of :initarg :shared :initform nil)
   (voice :accessor voice-of :initarg :voice :initform nil)
   (memory :accessor memory-of :initarg :memory :initform nil)
   (func-data :accessor func-data-of :initarg :func-data :initform nil)
   (wait-state :accessor wait-state-of :initarg :wait-state :initform 0)
   (last-direction :accessor last-direction-of :initarg :last-direction :initform nil)
   (mob-idnum :accessor mob-idnum-of :initarg :mob-idnum :initform nil)
   (prog-state :accessor prog-state-of :initarg :prog-state :initform nil)
   (prog-marker :accessor prog-marker-of :initarg :prog-marker :initform nil)))

(defclass player (creature)
  ((account :accessor account-of :initarg :account :initform nil)
   (title :accessor title-of :initarg :title :initform nil)
   (hometown :accessor hometown-of :initarg :hometown :initform 0)
   (poofin :accessor poofin-of :initarg :poofin :initform nil)
   (poofout :accessor poofout-of :initarg :poofout :initform nil)
   (command-aliases :accessor command-aliases-of :initarg :command-aliases :initform nil)
   (tongues-heard :accessor tongues-heard-of :initarg :tongues-heard :initform nil)
   (afk-reason :accessor afk-reason-of :initarg :afk-reason :initform nil)
   (afk-notifies :accessor afk-notifies-of :initform nil)
   (last-tell-from :accessor last-tell-from-of :initarg :last-tell-from :initform nil)
   (last-tell-to :accessor last-tell-to-of :initarg :last-tell-to :initform nil)
   (imprint-rooms :accessor imprint-rooms-of :initarg :imprint-rooms :initform nil)
   (recently-killed :accessor recently-killed-of :initarg :recently-killed :initform nil)
   (olc-obj :accessor olc-obj-of :initarg :olc-obj :initform nil)
   (olc-mob :accessor olc-mob-of :initarg :olc-mob :initform nil)
   (olc-shop :accessor olc-shop-of :initarg :olc-shop :initform nil)
   (olc-help :accessor olc-help-of :initarg :olc-help :initform nil)
   (olc-srch :accessor olc-srch-of :initarg :olc-srch :initform nil)
   (was-in-room :accessor was-in-room-of :initarg :was-in-room :initform nil)
   (olc-help-item :accessor olc-help-item-of :initarg :olc-help-item :initform nil)
   (thaw-time :accessor thaw-time-of :initarg :thaw-time :initform nil)
   (freezer-id :accessor freezer-id-of :initarg :freezer-id :initform nil)
   (rentcode :accessor rentcode-of :initarg :rentcode :initform nil)
   (rent-per-day :accessor rent-per-day-of :initarg :rent-per-day :initform 0)
   (desc-mode :accessor desc-mode-of :initarg :desc-mode :initform nil)
   (rent-currency :accessor rent-currency-of :initarg :rent-currency :initform nil)
   (skills :accessor skills-of :initarg :skills :initform (make-array 1000 :element-type '(integer -127 127)))
   (weap-spec :accessor weap-spec-of :initarg :weap-spec :initform (make-array 5 :initial-element nil))
   (wimp-level :accessor wimp-level-of :initarg :wimp-level :initform 0)
   (freeze-level :accessor freeze-level-of :initarg :freeze-level :initform nil)
   (invis-level :accessor invis-level-of :initarg :invis-level :initform 0)
   (load-room :accessor load-room-of :initarg :load-room :initform 3001)
   (home-room :accessor home-room-of :initarg :home-room :initform 3001)
   (prefs :accessor prefs-of :initarg :pref :initform (make-array 64 :element-type 'bit :initial-element 0))
   (clan :accessor clan-of :initarg :clan :initform 0)
   (broken-component :accessor broken-component-of :initarg :broken-component :initform 0)
   (imm-qp :accessor imm-qp-of :initarg :imm-qp :initform 0)
   (qlog-level :accessor qlog-level-of :initarg :qlog-level :initform 0)
   (speed :accessor speed-of :initarg :speed :initform 0)
   (qp-allowance :accessor qp-allowance-of :initarg :qp-allowance :initform 0)
   (badge :accessor badge-of :initarg :badge :initform "")
   (deity :accessor deity-of :initarg :deity :initform nil)
   (life-points :accessor life-points-of :initarg :life-points :initform 0)
   (pkills :accessor pkills-of :initarg :pkills :initform 0)
   (akills :accessor akills-of :initarg :akills :initform 0)
   (mobkills :accessor mobkills-of :initarg :mobkills :initform 0)
   (deaths :accessor deaths-of :initarg :deaths :initform 0)
   (old-char-class :accessor old-char-class-of :initarg :old-char-class :initform 0)
   (total-dam :accessor total-dam-of :initarg :total-dam :initform 0)
   (hold-load-room :accessor hold-load-room-of :initarg :hold-load-room :initform nil)
   (quest-id :accessor quest-id-of :initarg :quest-id :initform 0)
   (plr-bits :accessor plr-bits-of :initarg :plr-bits :initform 0)
   (plr2-bits :accessor plr2-bits-of :initarg :plr2-bits :initform 0)
   (reputation :accessor reputation-of :initarg :reputation :initform 0)
   (killer-severity :accessor killer-severity-of :initarg :killer-severity :initform nil)
   (mana-shield-low :accessor mana-shield-low-of :initarg :mana-shield-low :initform nil)
   (mana-shield-pct :accessor mana-shield-pct-of :initarg :mana-shield-pct :initform nil)
   (grievances :accessor grievances-of :initarg :grievances :initform nil)))

(defmethod print-object ((ch mobile) stream)
  (print-unreadable-object (ch stream :type t)
    (format stream "~a ~s"
            (when (shared-of ch) (tempus::vnum-of (shared-of ch)))
            (tempus::name-of ch))))

(defmethod print-object ((ch player) stream)
  (print-unreadable-object (ch stream :type t)
    (format stream "~a ~s" (idnum-of ch) (name-of ch))))

(defun copy-abilities (abils)
  (make-instance 'char-ability-data
                 :str (str-of abils)
                 :str-add (str-add-of abils)
                 :int (int-of abils)
                 :wis (wis-of abils)
                 :dex (dex-of abils)
                 :con (con-of abils)
                 :cha (cha-of abils)))

(defmacro define-attr-accessor (att)
  (let ((aff-name (intern (format nil "~a-OF" att)))
        (real-name (intern (format nil "REAL-~a-OF" att))))
  `(progn
     (defmethod ,aff-name ((ch creature))
       (,aff-name (aff-abils-of ch)))
     (defmethod (setf ,aff-name) (val (ch creature))
       (setf (,aff-name (aff-abils-of ch)) val))
     (defmethod ,real-name ((ch creature))
       (,aff-name (real-abils-of ch)))
     (defmethod (setf ,real-name) (val (ch creature))
       (setf (,aff-name (real-abils-of ch)) val)
       (affect-total ch)))))

(define-attr-accessor str)
(define-attr-accessor int)
(define-attr-accessor wis)
(define-attr-accessor dex)
(define-attr-accessor con)
(define-attr-accessor cha)

(defmethod vnum-of ((ch mobile))
  (if (shared-of ch)
      (vnum-of (shared-of ch))
      -1))
(defmethod speed-of ((ch mobile))
  0)
(defmethod (setf speed-of) (val (ch mobile))
  (declare (ignore val))
  ;; Does nothing
  nil)
(defmethod weap-spec-of ((ch mobile))
  #(nil nil nil nil nil))

(defun clone-mobile-proto (proto)
  (let ((maxhitp (if (zerop (max-hitp-of proto))
                     (+ (dice (hitp-of proto) (mana-of proto)) (move-of proto))
                     (random-range (hitp-of proto) (mana-of proto)))))
  (make-instance 'mobile
                 :shared (shared-of proto)
                 :real-abils (copy-abilities (real-abils-of proto))
                 :aff-abils (copy-abilities (aff-abils-of proto))
                 :mana (max-mana-of proto)
                 :max-mana (max-mana-of proto)
                 :hitp maxhitp
                 :max-hitp maxhitp
                 :move (max-move-of proto)
                 :max-move (max-move-of proto)
                 :armor (armor-of proto)
                 :gold (if (mob2-flagged proto +mob2-unapproved+)
                           0 (rand-value (gold-of proto)
                                         (* (gold-of proto) 0.15)
                                         nil nil))
                 :cash (if (mob2-flagged proto +mob2-unapproved+)
                           0 (rand-value (cash-of proto)
                                         (* (cash-of proto) 0.15)
                                         nil nil))
                 :exp (if (mob2-flagged proto +mob2-unapproved+)
                           0 (exp-of proto))
                 :hitroll (hitroll-of proto)
                 :damroll (damroll-of proto)
                 :remort-gen (remort-gen-of proto)
                 :saves (saves-of proto)
                 :aff-flags (aff-flags-of proto)
                 :aff2-flags (aff2-flags-of proto)
                 :aff3-flags (aff3-flags-of proto)
                 :mob-flags (logior (mob-flags-of proto) +mob-isnpc+)
                 :mob2-flags (mob2-flags-of proto)
                 :alignment (alignment-of proto)
                 :name (name-of proto)
                 :aliases (aliases-of proto)
                 :ldesc (ldesc-of proto)
                 :fdesc (fdesc-of proto)
                 :char-class (char-class-of proto)
                 :remort-char-class (remort-char-class-of proto)
                 :weight (weight-of proto)
                 :height (height-of proto)
                 :sex (sex-of proto)
                 :race (race-of proto)
                 :level (level-of proto)
                 :birth-time (birth-time-of proto)
                 :timer (timer-of proto)
                 :meditate-timer (meditate-timer-of proto)
                 :position (position-of proto)
                 :weapon-proficiency (weapon-proficiency-of proto)
                 :tongues (tongues-of proto)
                 :current-tongue (current-tongue-of proto)
                 :voice (voice-of proto))))

(defmethod modify-weight ((ch creature) mod-weight)
  (incf (weight-of ch) mod-weight))

(defmethod modify-carried-weight ((ch creature) mod-weight)
  (incf (carry-weight-of ch) mod-weight))

(defmethod modify-worn-weight ((ch creature) mod-weight)
  (incf (worn-weight-of ch) mod-weight))

(defun pref-flagged (ch pref)
  (and (typep ch 'player)
       (bitp (prefs-of ch) pref)))

(defun mob-flagged (ch flag)
  (and (not (typep ch 'player))
       (logtest (mob-flags-of ch) flag)))

(defun mob2-flagged (ch flag)
  (and (not (typep ch 'player))
       (logtest flag (mob2-flags-of ch))))

(defun awakep (ch)
  (> (position-of ch) +pos-sleeping+))

(defun alivep (ch)
  (/= (position-of ch) +pos-dead+))

(defun deadp (ch)
  (= (position-of ch) +pos-dead+))

(defun immortalp (ch)
  (and (>= (level-of ch) 50)
       (not (plr-flagged ch +plr-mortalized+))))
(defun immortal-level-p (ch)
  (>= (level-of ch) 50))

(defmethod approvedp ((ch mobile))
  (not (mob2-flagged ch +mob2-unapproved+)))
(defmethod approvedp ((ch player))
  t)

(defun check-skill (ch skill)
  (if (is-npc ch)
      100
      (aref (skills-of ch) skill)))

(defun skill-of (ch skill)
  ;; DEBUG: This maybe should be 50
  (if (is-npc ch)
      0
      (aref (skills-of ch) skill)))

(defun (setf skill-of) (val ch skill)
  (when (is-pc ch)
    (setf (aref (skills-of ch) skill) val)))

(defun check-tongue (ch tongue-id)
  (aref (tongues-of ch) tongue-id))

(defun aff-flagged (ch flag)
  (logtest (aff-flags-of ch) flag))
(defun aff2-flagged (ch flag)
  (logtest (aff2-flags-of ch) flag))
(defun aff3-flagged (ch flag)
  (logtest (aff3-flags-of ch) flag))
(defun plr-flagged (ch flag)
  (and (typep ch 'player)
       (logtest (plr-bits-of ch) flag)))
(defun plr2-flagged (ch flag)
  (and (typep ch 'player)
       (logtest (plr2-bits-of ch) flag)))
(defun is-good (ch) (>= (alignment-of ch) 350))
(defun is-evil (ch) (<= (alignment-of ch) -350))
(defun is-neutral (ch) (< -350 (alignment-of ch) 350))
(defun is-sick (ch) (aff3-flagged ch +aff3-sickness+))
(defun is-confused (ch) (aff-flagged ch +aff-confusion+))
(defun is-hamstrung (ch) (aff3-flagged ch +aff3-hamstrung+))
(defun has-poison-1 (ch) (aff-flagged ch +aff-poison+))
(defun has-poison-2 (ch) (aff3-flagged ch +aff3-poison-2+))

(defun has-poison-3 (ch) (aff3-flagged ch +aff3-poison-2+))
(defun is-poisoned (ch) (or (has-poison-1 ch)
                            (has-poison-2 ch)
                            (has-poison-3 ch)))

(defun is-soulless (ch)
  (or (mob-flagged ch +mob-soulless+)
      (plr2-flagged ch +plr2-soulless+)))

(defun is-class (ch char-class)
  (or (= (char-class-of ch) char-class)
      (and (is-remort ch)
           (= (remort-char-class-of ch) char-class))))
(defun is-magic-user (ch) (is-class ch +class-magic-user+))
(defun is-mage (ch) (is-class ch +class-magic-user+))
(defun is-cleric (ch) (is-class ch +class-cleric+))
(defun is-thief (ch) (is-class ch +class-thief+))
(defun is-warrior (ch) (is-class ch +class-warrior+))
(defun is-barb (ch) (is-class ch +class-barb+))
(defun is-psionic (ch) (is-class ch +class-psionic+))
(defun is-physic (ch) (is-class ch +class-physic+))
(defun is-cyborg (ch) (is-class ch +class-cyborg+))
(defun is-knight (ch) (is-class ch +class-knight+))
(defun is-ranger (ch) (is-class ch +class-ranger+))
(defun is-bard (ch) (is-class ch +class-bard+))
(defun is-monk (ch) (is-class ch +class-monk+))
(defun is-merc (ch) (is-class ch +class-mercenary+))
(defun is-spare1 (ch) (is-class ch +class-spare1+))
(defun is-spare2 (ch) (is-class ch +class-spare2+))
(defun is-spare3 (ch) (is-class ch +class-spare3+))
(defun is-skeleton (ch) (is-class ch +class-skeleton+))
(defun is-ghoul (ch) (is-class ch +class-ghoul+))
(defun is-shadow (ch) (is-class ch +class-shadow+))
(defun is-wight (ch) (is-class ch +class-wight+))
(defun is-wraith (ch) (is-class ch +class-wraith+))
(defun is-mummy (ch) (is-class ch +class-mummy+))
(defun is-spectre (ch) (is-class ch +class-spectre+))
(defun is-vampire (ch) (is-class ch +class-vampire+))
(defun is-npc-vampire (ch) (is-class ch +class-npc-vampire+))
(defun is-ghost (ch) (is-class ch +class-ghost+))
(defun is-lich (ch) (is-class ch +class-lich+))
(defun is-zombie (ch) (is-class ch +class-zombie+))

(defun is-race (ch race)
  (= (race-of ch) race))

(defun is-humanoid (ch)
  (or (and (<= (race-of ch) +race-devil+)
           (not (is-race ch +race-animal+))
           (not (is-race ch +race-dragon+))
           (not (is-race ch +race-elemental+)))
      (and (<= +race-bugbear+ (race-of ch) +race-deva+))
      (is-race ch +race-archon+)
      (is-race ch +race-illithid+)
      (is-race ch +race-githyanki+)
      (is-race ch +race-githzerai+)
      (is-race ch +race-kobold+)
      (is-race ch +race-mephit+)
      (is-race ch +race-daemon+)
      (is-race ch +race-rakshasa+)
      (is-race ch +race-rowlahr+)))

(defun is-human (ch) (= (race-of ch) +race-human+))
(defun is-elf (ch) (= (race-of ch) +race-elf+))
(defun is-drow (ch) (= (race-of ch) +race-drow+))
(defun is-dwarf (ch) (= (race-of ch) +race-dwarf+))
(defun is-tabaxi (ch) (= (race-of ch) +race-tabaxi+))
(defun is-half-orc (ch) (= (race-of ch) +race-half-orc+))
(defun is-animal (ch) (= (race-of ch) +race-animal+))
(defun is-elemental (ch) (= (race-of ch) +race-elemental+))
(defun is-dragon (ch) (= (race-of ch) +race-dragon+))
(defun is-halfling (ch) (= (race-of ch) +race-halfling+))
(defun is-giant (ch) (= (race-of ch) +race-giant+))
(defun is-orc (ch) (= (race-of ch) +race-orc+))
(defun is-goblin (ch) (= (race-of ch) +race-goblin+))
(defun is-minotaur (ch) (= (race-of ch) +race-minotaur+))
(defun is-troll (ch) (= (race-of ch) +race-troll+))
(defun is-golem (ch) (= (race-of ch) +race-golem+))
(defun is-ogre (ch) (= (race-of ch) +race-ogre+))
(defun is-devil (ch) (= (race-of ch) +race-devil+))
(defun is-demon (ch) (= (race-of ch) +race-demon+))
(defun is-slaad (ch) (= (race-of ch) +race-slaad+))
(defun is-trog (ch) (= (race-of ch) +race-troglodyte+))
(defun is-manticore (ch) (= (race-of ch) +race-manticore+))
(defun is-robot (ch) (= (race-of ch) +race-robot+))
(defun is-plant (ch) (= (race-of ch) +race-plant+))
(defun is-archon (ch) (= (race-of ch) +race-archon+))
(defun is-guardinal (ch) (= (race-of ch) +race-guardinal+))
(defun is-pudding (ch) (= (race-of ch) +race-pudding+))
(defun is-slime (ch) (= (race-of ch) +race-slime+))
(defun is-bugbear (ch) (= (race-of ch) +race-bugbear+))
(defun is-alien-1 (ch) (= (race-of ch) +race-alien-1+))
(defun is-fish (ch) (= (race-of ch) +race-fish+))
(defun is-rakshasa (ch) (= (race-of ch) +race-rakshasa+))
(defun is-rowlahr (ch) (= (race-of ch) +race-rowlahr+))

(defun noncorporealp (ch)
  (or (is-shadow ch)
      (is-wight ch)
      (is-wraith ch)
      (is-spectre ch)
      (is-ghost ch)
      (and (eql (race-of ch) +race-elemental+)
           (or (eql (char-class-of ch) +class-air+)
               (eql (char-class-of ch) +class-water+)
               (eql (char-class-of ch) +class-fire+)))))

(defun mindlessp (ch)
  (or (is-undead ch)
      (is-slime ch)
      (is-plant ch)
      (is-pudding ch)
      (is-robot ch)))

(defun get-eq (ch pos) (aref (equipment-of ch) pos))
(defun get-implant (ch pos) (aref (implants-of ch) pos))
(defun get-tattoo (ch pos) (aref (tattoos-of ch) pos))
(defun get-condition (ch cond) (aref (conditions-of ch) cond))

(defun char-withstands-fire (ch)
  (or (immortalp ch)
      (aff2-flagged ch +aff2-prot-fire+)
      (= (char-class-of ch) +class-fire+)
      (and (is-dragon ch) (= (char-class-of ch) +class-red+))
      (and (is-devil ch) (or
                          (= (plane-of (zone-of (in-room-of ch)))
                             +plane-hell-4+)
                          (= (plane-of (zone-of (in-room-of ch)))
                             +plane-hell-6+)))))

(defun char-withstands-heat (ch)
  (or (immortalp ch)
      (aff3-flagged ch +aff3-prot-heat+)
      (is-undead ch)
      (is-slaad ch)
      (and (is-npc ch) (<= 16100 (vnum-of ch) 16699))
      (= (char-class-of ch) +class-fire+)
      (and (is-dragon ch) (= (char-class-of ch) +class-red+))
      (and (is-devil ch) (or
                          (= (plane-of (zone-of (in-room-of ch)))
                             +plane-hell-4+)
                          (= (plane-of (zone-of (in-room-of ch)))
                             +plane-hell-6+)))))

(defun char-withstands-cold (ch)
  (or (immortalp ch)
      (aff2-flagged ch +aff2-endure-cold+)
      (aff2-flagged ch +aff2-ablaze+)
      (and (aff3-flagged ch +aff3-stasis+)
           (eql (position-of ch) +pos-sleeping+))
      (and (is-dragon ch)
           (eql (char-class-of ch) +class-white+))
      (is-undead ch)
      (eql (char-class-of ch) +class-frost+)
      (is-slaad ch)
      (and (is-devil ch)
           (or (eql (plane-of (zone-of (in-room-of ch))) +plane-hell-5+)
               (eql (plane-of (zone-of (in-room-of ch))) +plane-hell-8+)))))

(defun char-withstands-radiation (ch)
  (or (immortalp ch)
      (pref-flagged ch +pref-nohassle+)
      (aff2-flagged ch +aff2-prot-rad+)))

(defun char-has-blood (ch)
  (not (or (is-undead ch)
           (is-elemental ch)
           (is-golem ch)
           (is-robot ch)
           (is-plant ch)
           (is-alien-1 ch)
           (is-pudding ch)
           (is-slime ch))))

(defun is-dead (ch) (eql (position-of ch) +pos-dead+))
(defun is-remort (ch) (plusp (remort-gen-of ch)))
(defun is-pc (ch) (typep ch 'player))
(defun is-npc (ch) (typep ch 'mobile))
(defun is-undead (ch) (= (race-of ch) +race-undead+))
(defun is-pet (ch) (mob-flagged ch +mob-pet+))
(defun is-newbie (ch)
  (not (or (is-npc ch)
           (plr-flagged ch +plr-hardcore+)
           (plusp (remort-gen-of ch))
           (> (level-of ch) 24))))
(defun outsidep (ch)
  (and (not (room-flagged (in-room-of ch) +room-indoors+))
       (not (eql (terrain-of (in-room-of ch)) +sect-inside+))))

(defun has-symbol (ch)
  (or (is-soulless ch)
      (affected-by-spell ch +spell-stigmata+)
      (aff3-flagged ch +aff3-symbol-of-pain+)
      (aff3-flagged ch +aff3-tainted+)))

(defun can-detect-disguise (ch vict level)
  (or (pref-flagged ch +pref-holylight+)
      (aff2-flagged ch +aff2-true-seeing+)
      (> (+ (int-of ch) (wis-of ch))
         (+ level (cha-of vict)))))
(defun get-disguised-name (ch tch)
  (let* ((af (affected-by-spell tch +skill-disguise+))
         (mob (when af (real-mobile-proto (modifier-of af)))))
    (cond
      ((or (is-npc tch)
           (null af)
           (null mob))
       (name-of tch))
      ((can-detect-disguise ch tch (duration-of af))
       (format nil "~a (disguised as ~a)"
               (name-of tch)
               (name-of mob)))
      (t
       (name-of mob)))))
(defun testerp (ch)
  (security-is-really-member ch "Testers"))

(defun get-level-bonus (ch)
  100)

(defun get-skill-bonus (ch skill)
  100)

(defun in-same-group-p (ch tch)
  (and (aff-flagged ch +aff-group+)
       (aff-flagged tch +aff-group+)
       (or (eql ch (master-of tch))
           (eql tch (master-of ch))
           (eql (master-of ch) (master-of tch)))))

(defun illegal-soilpos (pos)
  (member pos (list +wear-light+
                    +wear-shield+
                    +wear-about+
                    +wear-wield+
                    +wear-hold+
                    +wear-belt+
                    +wear-wield-2+
                    +wear-ass+
                    +wear-neck-2+
                    +wear-random+)))

(defun char-soilage (ch pos)
  (aref (soilage-of ch) pos))
(defun char-soiled (ch pos soil)
  (logtest (char-soilage ch pos) (ash 1 soil)))

(defun wait-state (ch pulses)
  (cond
    ((is-npc ch)
     (incf (wait-state-of ch)))
    ((and (link-of ch)
          (not (immortalp ch)))
     (incf (wait-of (link-of ch)) pulses))))

(defmethod wait-of ((ch mobile))
  (wait-state-of ch))

(defmethod wait-of ((ch player))
  (if (link-of ch)
      (wait-of (link-of ch))
      0))

(defun affect-remove (ch af)
  "Given a creature affect AF, removes the effect of AF on the creature CH."
  (when (= (kind-of af) +spell-taint+)
    (apply-soil-to-char ch (get-eq ch +wear-head+) +soil-blood+ +wear-head+)
    (apply-soil-to-char ch (get-eq ch +wear-face+) +soil-blood+ +wear-face+)
    (apply-soil-to-char ch (get-eq ch +wear-eyes+) +soil-blood+ +wear-eyes+))
  (when (and (= (kind-of af) +spell-quad-damage+)
             (not (aff-flagged ch +aff-glowlight+))
             (not (aff2-flagged ch +aff2-fluorescent+))
             (not (aff2-flagged ch +aff2-divine-illumination+))
             (not (affected-by-spell ch +spell-quad-damage+)))
    (decf (light-of (in-room-of ch))))

  (affect-modify ch (location-of af) (modifier-of af) (bitvector-of af) (aff-index-of af) nil)
  (setf (affected-of ch) (delete af (affected-of ch))))

(defun affect-from-char (ch spell)
  "Removes all the affects from CH that come from the SPELL."
  (let ((doomed-afs (remove spell (affected-of ch) :test-not #'= :key 'kind-of)))
    (when doomed-afs
      (dolist (af doomed-afs)
        (affect-remove ch af))
      (when (wearoff-msg-of (aref *spell-info* spell))
        (send-to-char ch "~a~%" (wearoff-msg-of (aref *spell-info* spell)))))))

(defun perform-dismount (ch)
  (setf (mounted-of ch) nil))

(defun stop-hunting (ch)
  (setf (hunting-of ch) nil))

(defun stop-defending (ch)
  (setf (defending-of ch) nil))

(defun perform-return (ch mode)
  (unless (and (link-of ch) (original-actor-of (link-of ch)))
    (send-to-char ch "There is no need to return.~%")
    (return-from perform-return))

  (send-to-char ch "You return to your original body.~%")

  (let ((orig (original-actor-of (link-of ch))))
    (when (link-of (original-actor-of (link-of ch)))
      (setf (state-of (link-of (original-actor-of (link-of ch)))) 'disconnecting))

    (setf (actor-of (link-of ch)) (original-actor-of (link-of ch)))
    (setf (original-actor-of (link-of ch)) nil)

    (setf (link-of (actor-of (link-of ch))) (link-of ch))
    (setf (link-of ch) nil)

    (when (and (is-npc ch) (= (vnum-of ch) 1518))
      (char-from-room orig nil)
      (char-to-room orig (in-room-of ch) nil)
      (act orig :place-emit "$n materializes from a cloud of gas.")
      (unless (eql mode :noextract)
        (purge-creature ch t)))))

(defun extract-creature (ch link-state)
  "Extract a creature completely from the world and destroy its stuff"
  (when (and (not (is-npc ch)) (null (link-of ch)))
    (let ((cxn (find ch *cxns* :key 'original-actor-of)))
      (when cxn
        (perform-return (actor-of cxn) :forced))))

  (when (and (link-of ch) (original-actor-of (link-of ch)))
    (perform-return (actor-of (link-of ch)) :forced))

  (assert (in-room-of ch) nil "NIL room in extract-creature")

  (when (master-of ch)
    (stop-following ch))

  (dolist (tch (copy-list (followers-of ch)))
    (stop-following tch))

  ;; remove fighters, defenders, hunters, and mounters
  (dolist (tch *characters*)
    (when (eql (defending-of tch) ch)
      (stop-defending tch))
    (when (eql (mounted-of tch) ch)
      (perform-dismount tch))
    (when (eql (hunting-of tch) ch)
      (stop-hunting tch)))

  #+nil (destroy-attached-progs ch)
  (char-arrest-pardoned ch)

  (when (mounted-of ch)
    (perform-dismount ch))

  ;; TODO: make sure they aren't editing

  ;; Forget snooping, if applicable
  (when (link-of ch)
    (when (snooping-of (link-of ch))
      (setf (snooping-of (link-of ch)) nil))
    (dolist (snooper (snooped-by-of (link-of ch)))
      (cxn-write snooper "Your victim is no longer among us.~%")
      (setf (snooping-of snooper) nil)))

  ;; destroy all that equipment
  (loop for idx upto (1- +num-wears+) do
       (when (get-eq ch idx)
         (extract-obj (unequip-char ch idx :worn t)))
       (when (get-implant ch idx)
         (extract-obj (unequip-char ch idx :implant t)))
       (when (get-tattoo ch idx)
         (extract-obj (unequip-char ch idx :tattoo t))))

  (dolist (obj (carrying-of ch))
    (obj-from-char obj)
    (extract-obj obj))

  (when (and (link-of ch) (original-actor-of (link-of ch)))
    (perform-return ch :noextract))

  (remove-all-combat ch)

  (char-from-room ch nil)

  (setf *characters* (remove ch *characters*))
  (if (is-npc ch)
      (remhash (- (mob-idnum-of ch)) *character-map*)
      (remhash (idnum-of ch) *character-map*))

  #+nil (path-remove-object ch)

  (when (and (is-npc ch) (shared-of ch))
    (decf (number-of (shared-of ch))))

  (when (link-of ch)
    (setf (state-of (link-of ch)) link-state)))

(defun restore-creature (ch)
  (setf (hitp-of ch) (max-hitp-of ch))
  (setf (mana-of ch) (max-mana-of ch))
  (setf (move-of ch) (max-move-of ch))

  (unless (minusp (get-condition ch +full+))
    (setf (aref (conditions-of ch) +full+) 24))
  (unless (minusp (get-condition ch +thirst+))
    (setf (aref (conditions-of ch) +thirst+) 24))
  (setf (aref (conditions-of ch) +drunk+) 0)

  (when (immortalp ch)
    (loop for i from 1 upto +max-skills+ do
         (setf (aref (skills-of ch) i) 100))
    (loop for i from 1 upto (1- +max-tongues+) do
         (setf (aref (tongues-of ch) i) 100))
    (setf (str-of ch) 25)
    (setf (int-of ch) 25)
    (setf (wis-of ch) 25)
    (setf (dex-of ch) 25)
    (setf (con-of ch) 25)
    (setf (cha-of ch) 25)
    (setf (real-abils-of ch) (copy-abilities (aff-abils-of ch))))

  (update-pos ch))

(defun arena-die (ch)
  (remove-all-combat ch)
  (unless (is-npc ch)
    (setf (rentcode-of ch) +rent-rented+)
    (setf (rent-per-day-of ch) (if (immortal-level-p ch)
                                   0
                                   (calc-daily-rent ch 1 nil nil)))
    (setf (desc-mode-of ch) :unknown)
    (setf (rent-currency-of ch) (time-frame-of (zone-of (in-room-of ch))))
    (setf (load-room-of ch) (respawn-pt-of (zone-of (in-room-of ch))))
    (setf (login-time-of ch) (now))
    (save-player-objects ch)
    (save-player-to-xml ch)
    (unless (immortalp ch)
      (mudlog 'info t "~a has died in arena (~d/day, ~d ~a)"
              (name-of ch)
              (rent-per-day-of ch)
              (+ (gold-of ch)
                 (cash-of ch)
                 (past-bank-of (account-of ch))
                 (future-bank-of (account-of ch)))
              (if (eql (rent-currency-of ch) +time-electro+) "creds" "gold"))))

  (extract-creature ch 'afterlife))

(defun npk-die (ch)
  (remove-all-combat ch)
  (unless (is-npc ch)
    (setf (rentcode-of ch) +rent-quit+)
    (setf (rent-per-day-of ch) 0)
    (setf (desc-mode-of ch) 'afterlife)
    (setf (rent-currency-of ch) (time-frame-of (zone-of (in-room-of ch))))
    (setf (load-room-of ch) (respawn-pt-of (zone-of (in-room-of ch))))
    (setf (login-time-of ch) (now))
    (save-player-objects ch)
    (save-player-to-xml ch))

  (extract-creature ch 'afterlife))

(defun die (ch)
  (remove-all-combat ch)

  ;; If their stuff hasn't been moved out, they dt'd, so we need to
  ;; dump their stuff to the room.
  (loop
     for obj across (equipment-of ch)
     when obj do
       (unequip-char ch (worn-on-of obj) :worn nil)
       (obj-to-room obj (in-room-of ch)))
  (loop
     for obj across (implants-of ch)
     when obj do
       (unequip-char ch (worn-on-of obj) :worn nil)
       (obj-to-room obj (in-room-of ch)))
  (loop
     for obj across (tattoos-of ch)
     when obj do
       (unequip-char ch (worn-on-of obj) :worn nil)
       (extract-obj obj))

  (dolist (obj (copy-list (carrying-of ch)))
    (obj-from-char obj)
    (obj-to-room obj (in-room-of ch)))


  (unless (is-npc ch)
    (setf (rentcode-of ch) +rent-quit+)
    (setf (rent-per-day-of ch) 0)
    (setf (desc-mode-of ch) 'afterlife)
    (setf (rent-currency-of ch) (time-frame-of (zone-of (in-room-of ch))))
    (setf (load-room-of ch) (respawn-pt-of (zone-of (in-room-of ch))))
    (setf (login-time-of ch) (now))
    (save-player-objects ch)
    (save-player-to-xml ch))

  (extract-creature ch 'afterlife))


(defun get-first-printed-char (str)
  "Returns the position of the first character that isn't a terminal
control code."
  (loop for idx = 0 then (+ 2 idx)
        while (and (< idx (length str))
                   (eql (char str idx) #\&))
        finally (when (< idx (length str)) (return idx))))

(defun send-to-char (ch fmt &rest args)
  (when (link-of ch)
    (let* ((str (format nil "~?" fmt args))
           (first-char-pos (get-first-printed-char str)))
      (cxn-write (link-of ch) "~a"
                 (if first-char-pos
                     (string-upcase str
                                    :start first-char-pos
                                    :end (1+ first-char-pos))
                     str)))))

(defun ignite (ch &optional igniter)
  (affect-to-char ch
                  (make-instance 'affected-data
                                 :kind +spell-ablaze+
                                 :duration -1
                                 :bitvector +aff2-ablaze+
                                 :aff-index 2
                                 :owner (if igniter
                                            (idnum-of igniter)
                                            0))))

(defun extinguish (ch)
  (affect-from-char ch +spell-ablaze+))

(defun is-tarrasque (ch)
  (and (is-npc ch)
       (= (vnum-of ch) 24800)))

(defun can-go (ch door)
  (and (exit ch door)
       (to-room-of (exit ch door))
       (or (not (logtest (exit-info-of (exit ch door))
                         (logior +ex-closed+ +ex-nopass+ +ex-hidden+)))
           (immortalp ch)
           (noncorporealp ch))))

(defun is-water-elemental (ch)
  (and (is-elemental ch)
       (is-class ch +class-water+)))

(defun is-fire-elemental (ch)
  (and (is-elemental ch)
       (is-class ch +class-fire+)))

(defun char-likes-room (ch room)
  (not (or (and (is-water-elemental ch) (not (room-is-watery room)))
           (and (is-fire-elemental ch) (room-is-watery room))
           (and (room-flagged room +room-flame-filled+)
                (not (char-withstands-fire ch)))
           (and (room-flagged room +room-ice-cold+)
                (not (char-withstands-cold ch)))
           (and (room-flagged room +room-holyocean+)
                (is-evil ch))
           (not (can-travel-terrain ch (terrain-of room)))
           (and (mob2-flagged ch +mob2-stay-sect+)
                (not (eql (terrain-of (in-room-of ch))
                          (terrain-of room))))
           (and (not (can-see-room ch room))
                (or (not (get-eq ch +wear-light+))
                    (aref (value-of (get-eq ch +wear-light+)) 0))))))

(defun cost-modifier-of (buyer seller)
  "Returns a number from -44 to 44, indicating the percentage that
should be added or removed from a transaction involving the two
creatures."
  (* (- (cha-of seller) (cha-of buyer)) 2))

(defun adjusted-cost (buyer seller base-cost)
  (+ base-cost
     (floor (* base-cost (cost-modifier-of buyer seller)) 100)))

(defun creature-trusts-idnum (ch idnum)
  (or (find idnum (players-of (account-of ch)) :key 'idnum-of)
      (find idnum (trust-of (account-of ch)))))

(defun creature-trusts (ch target)
  (and (not (is-npc ch))
       (or (and (aff-flagged ch +aff-charm+)
                (eql (master-of ch)
                     target))
           (creature-trusts-idnum ch (idnum-of target)))))

(defun remember (ch vict)
  (push (idnum-of vict) (memory-of ch)))

(defun forget (ch vict)
  (setf (memory-of ch) (delete (idnum-of vict) (memory-of ch))))

(defun strength-damage-bonus (str)
  (floor (- (/ (* str str) 47) (/ str 10) 4)))

(defun strength-hit-bonus (str)
  (floor (- (/ str 3) 5)))

(defun hands-free (ch)
  (flet ((hands-used-by-pos (pos)
           (let ((obj (get-eq ch pos)))
             (cond
               ((null obj) 0)
               ((is-obj-stat2 obj +item2-two-handed+) 2)
               (t 1)))))
    (- 2
       (hands-used-by-pos +wear-wield+)
       (hands-used-by-pos +wear-wield-2+)
       (hands-used-by-pos +wear-shield+)
       (hands-used-by-pos +wear-hold+))))

(defun char-in-future? (ch)
  (eql (time-frame-of (zone-of (in-room-of ch))) +time-future+))
