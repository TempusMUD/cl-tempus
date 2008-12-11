(in-package :tempus)

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

(defclass char-ability-data ()
  ((str :accessor str-of :initarg :str :initform 11)
   (str-add :accessor str-add-of :initarg :str-add :initform 0)
   (int :accessor int-of :initarg :int :initform 11)
   (wis :accessor wis-of :initarg :wis :initform 11)
   (dex :accessor dex-of :initarg :dex :initform 11)
   (con :accessor con-of :initarg :con :initform 11)
   (cha :accessor cha-of :initarg :cha :initform 11)))

(defclass mob-shared-data ()
  ((vnum :accessor vnum-of :initarg :vnum :initform nil)
   (svnum :accessor svnum-of :initarg :svnum :initform nil)
   (number :accessor number-of :initarg :number :initform 0)
   (attack-type :accessor attack-type-of :initarg :attack-type :initform nil)
   (lair :accessor lair-of :initarg :lair :initform nil)
   (leader :accessor leader-of :initarg :leader :initform 0)
   (kills :accessor kills-of :initarg :kills :initform 0)
   (loaded :accessor loaded-of :initarg :loaded :initform 0)
   (default-pos :accessor default-pos-of :initarg :default-pos :initform nil)
   (damnodice :accessor damnodice-of :initarg :damnodice :initform nil)
   (damsizedice :accessor damsizedice-of :initarg :damsizedice :initform nil)
   (morale :accessor morale-of :initarg :morale :initform nil)
   (move-buf :accessor move-buf-of :initarg :move-buf :initform nil)
   (proto :accessor proto-of :initarg :proto :initform nil)
   (func :accessor func-of :initarg :func :initform nil)
   (func-param :accessor func-param-of :initarg :func-param :initform nil)
   (load-param :accessor load-param-of :initarg :load-param :initform nil)
   (prog-text :accessor prog-text-of :initarg :prog :initform nil)
   (progobj :accessor progobj-of :initarg :progobj :initform nil)
   (progobj-len :accessor progobj-len-of :initarg :progobj-len :initform nil)))

(defclass affected-type ()
  ((kind :accessor kind-of :initarg :kind :initform nil)
   (duration :accessor duration-of :initarg :duration :initform nil)
   (modifier :accessor modifier-of :initarg :modifier :initform nil)
   (location :accessor location-of :initarg :location :initform nil)
   (level :accessor level-of :initarg :level :initform nil)
   (is-instant :accessor is-instant-of :initarg :is-instant :initform nil)
   (bitvector :accessor bitvector-of :initarg :bitvector :initform nil)
   (aff-index :accessor aff-index-of :initarg :aff-index :initform nil)
   (owner :accessor owner-of :initarg :owner :initform nil)))

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
   (mana :accessor mana-of :initarg :mana :initform 100)
   (max-mana :accessor max-mana-of :initarg :max-mana :initform 100)
   (hitp :accessor hitp-of :initarg :hitp :initform 100)
   (max-hitp :accessor max-hitp-of :initarg :max-hitp :initform 100)
   (move :accessor move-of :initarg :move :initform 100)
   (max-move :accessor max-move-of :initarg :max-move :initform 100)
   (armor :accessor armor-of :initarg :armor :initform 100)
   (gold :accessor gold-of :initarg :gold :initform 0)
   (cash :accessor cash-of :initarg :cash :initform 0)
   (exp :accessor exp-of :initarg :exp :initform 0)
   (hitroll :accessor hitroll-of :initarg :hitroll :initform 0)
   (damroll :accessor damroll-of :initarg :damroll :initform 0)
   (conditions :accessor conditions-of :initarg :conditions :initform (make-array 3 :element-type '(integer -1 24) :initial-contents '(0 23 23)))

   ;; Originally from char_special_data_saved
   (idnum :accessor idnum-of :initarg :idnum :initform nil)
   (remort-gen :accessor remort-gen-of :initarg :remort-gen :initform 0)
   (saves :accessor saves-of :initarg :saves :initform (make-array 10))
   (aff-flags :accessor aff-flags-of :initarg :aff-flags :initform 0)
   (aff2-flags :accessor aff2-flags-of :initarg :aff2-flags :initform 0)
   (aff3-flags :accessor aff3-flags-of :initarg :aff3-flags :initform 0)
   (mob-flags :accessor mob-flags-of :initarg :mob-flags :initform 0)
   (mob2-flags :accessor mob2-flags-of :initarg :mob2-flags :initform 0)
   (alignment :accessor alignment-of :initarg :alignment :initform 0)

   ;; Originally from char_player_data
   (name :accessor name-of :initarg :name :initform nil)
   (aliases :accessor aliases-of :initarg :aliases :initform nil)
   (ldesc :accessor ldesc-of :initarg :ldesc :initform nil)
   (fdesc :accessor fdesc-of :initarg :fdesc :initform nil)
   (char-class :accessor char-class-of :initarg :char-class :initform 0)
   (remort-char-class :accessor remort-char-class-of :initarg :remort-char-class :initform 0)
   (weight :accessor weight-of :initarg :weight :initform 100)
   (height :accessor height-of :initarg :height :initform 100)
   (sex :accessor sex-of :initarg :sex :initform 'male)
   (race :accessor race-of :initarg :race :initform 0)
   (level :accessor level-of :initarg :level :initform 0)
   (age-adjust :accessor age-adjust-of :initarg :age-adjust :initform 0)
   (age :accessor age-of :initarg :age :initform nil)
   (birth-time :accessor birth-time-of :initarg :birth-time :initform nil)
   (login-time :accessor login-time-of :initarg :birth-time :initform nil)
   (death-time :accessor death-time-of :initarg :birth-time :initform nil)
   (played-time :accessor played-time-of :initarg :birth-time :initform nil)

   ;; Originally from char_special_data
   (defending :accessor defending-of :initarg :defending :initform nil)
   (hunting :accessor hunting-of :initarg :hunting :initform nil)
   (mounted :accessor mounted-of :initarg :mounted :initform nil)
   (carry-weight :accessor carry-weight-of :initarg :carry-weight :initform 0)
   (worn-weight :accessor worn-weight-of :initarg :worn-weight :initform 0)
   (timer :accessor timer-of :initarg :timer :initform nil)
   (meditate-timer :accessor meditate-timer-of :initarg :meditate-timer :initform nil)
   (cur-flow-pulse :accessor cur-flow-pulse-of :initarg :cur-flow-pulse :initform nil)
   (breath-count :accessor breath-count-of :initarg :breath-count :initform nil)
   (fall-count :accessor fall-count-of :initarg :fall-count :initform nil)
   (position :accessor position-of :initarg :position :initform +pos-standing+)
   (carry-items :accessor carry-items-of :initarg :carry-items :initform 0)
   (weapon-proficiency :accessor weapon-proficiency-of :initarg :weapon-proficiency :initform nil)
   (mood :accessor mood-of :initarg :mood :initform nil)
   (saved :accessor saved-of :initarg :saved :initform nil)

   ;; Originally from char_language_data
   (tongues :accessor tongues-of :initarg :tongues
            :initform (make-array +max-tongues+ :initial-element 0))
   (current-tongue :accessor current-tongue-of :initarg :current-tongue :initform nil)
   (last-command :accessor last-command-of :initform nil)
   (repeat-cmd-count :accessor repeat-cmd-count-of :initform 0)))

(defclass mobile (creature)
  ((shared :accessor shared-of :initarg :shared :initform nil)
   (memory :accessor memory-of :initarg :memory :initform nil)
   (func-data :accessor func-data-of :initarg :func-data :initform nil)
   (wait-state :accessor wait-state-of :initarg :wait-state :initform nil)
   (last-direction :accessor last-direction-of :initarg :last-direction :initform nil)
   (mob-idnum :accessor mob-idnum-of :initarg :mob-idnum :initform nil)
   (prog-state :accessor prog-state-of :initarg :prog-state :initform nil)
   (prog-marker :accessor prog-marker-of :initarg :prog-marker :initform nil)))

(defclass player (creature)
  ((account :accessor account-of :initarg :account :initform nil)
   (title :accessor title-of :initarg :title :initform nil)
   (hometown :accessor hometown-of :initarg :hometown :initform nil)
   (poofin :accessor poofin-of :initarg :poofin :initform nil)
   (poofout :accessor poofout-of :initarg :poofout :initform nil)
   (command-aliases :accessor command-aliases-of :initarg :command-aliases :initform nil)
   (tongues-heard :accessor tongues-heard-of :initarg :tongues-heard :initform nil)
   (afk-reason :accessor afk-reason-of :initarg :afk-reason :initform nil)
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
   (rent-per-day :accessor rent-per-day-of :initarg :rent-per-day :initform nil)
   (desc-mode :accessor desc-mode-of :initarg :desc-mode :initform nil)
   (rent-currency :accessor rent-currency-of :initarg :rent-currency :initform nil)
   (skills :accessor skills-of :initarg :skills :initform (make-array 1000 :element-type '(integer 0 127)))
   (weap-spec :accessor weap-spec-of :initarg :weap-spec :initform nil)
   (wimp-level :accessor wimp-level-of :initarg :wimp-level :initform nil)
   (freeze-level :accessor freeze-level-of :initarg :freeze-level :initform nil)
   (invis-level :accessor invis-level-of :initarg :invis-level :initform 0)
   (load-room :accessor load-room-of :initarg :load-room :initform nil)
   (home-room :accessor home-room-of :initarg :home-room :initform nil)
   (prefs :accessor prefs-of :initarg :pref :initform (make-array 64 :element-type 'bit :initial-element 0))
   (clan :accessor clan-of :initarg :clan :initform nil)
   (broken-component :accessor broken-component-of :initarg :broken-component :initform nil)
   (imm-qp :accessor imm-qp-of :initarg :imm-qp :initform nil)
   (qlog-level :accessor qlog-level-of :initarg :qlog-level :initform nil)
   (speed :accessor speed-of :initarg :speed :initform 0)
   (qp-allowance :accessor qp-allowance-of :initarg :qp-allowance :initform nil)
   (badge :accessor badge-of :initarg :badge :initform nil)
   (deity :accessor deity-of :initarg :deity :initform nil)
   (life-points :accessor life-points-of :initarg :life-points :initform nil)
   (pkills :accessor pkills-of :initarg :pkills :initform 0)
   (akills :accessor akills-of :initarg :akills :initform 0)
   (mobkills :accessor mobkills-of :initarg :mobkills :initform 0)
   (deaths :accessor deaths-of :initarg :deaths :initform 0)
   (old-char-class :accessor old-char-class-of :initarg :old-char-class :initform 0)
   (total-dam :accessor total-dam-of :initarg :total-dam :initform nil)
   (hold-load-room :accessor hold-load-room-of :initarg :hold-load-room :initform nil)
   (quest-id :accessor quest-id-of :initarg :quest-id :initform nil)
   (plr-bits :accessor plr-bits-of :initarg :plr-bits :initform 0)
   (plr2-bits :accessor plr2-bits-of :initarg :plr2-bits :initform 0)
   (reputation :accessor reputation-of :initarg :reputation :initform 0)
   (killer-severity :accessor killer-severity-of :initarg :killer-severity :initform nil)
   (mana-shield-low :accessor mana-shield-low-of :initarg :mana-shield-low :initform nil)
   (mana-shield-pct :accessor mana-shield-pct-of :initarg :mana-shield-pct :initform nil)))

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

(defmethod str-of ((ch creature))
  (str-of (aff-abils-of ch)))
(defmethod str-add-of ((ch creature))
  (str-add-of (aff-abils-of ch)))
(defmethod int-of ((ch creature))
  (int-of (aff-abils-of ch)))
(defmethod wis-of ((ch creature))
  (wis-of (aff-abils-of ch)))
(defmethod dex-of ((ch creature))
  (dex-of (aff-abils-of ch)))
(defmethod con-of ((ch creature))
  (con-of (aff-abils-of ch)))
(defmethod cha-of ((ch creature))
  (cha-of (aff-abils-of ch)))
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
                 :current-tongue (current-tongue-of proto))))

(defmethod modify-weight ((ch creature) mod-weight)
  (incf (weight-of ch) mod-weight))

(defmethod modify-carried-weight ((ch creature) mod-weight)
  (incf (carry-weight-of ch) mod-weight))

(defmethod modify-worn-weight ((ch creature) mod-weight)
  (incf (worn-weight-of ch) mod-weight))

(defun pref-flagged (ch pref)
  (bitp (prefs-of ch) pref))

(defun mob-flagged (ch flag)
  (and (not (typep ch 'player))
       (logtest (mob-flags-of ch) flag)))

(defun mob2-flagged (ch flag)
  (and (not (typep ch 'player))
       (logtest flag (mob2-flags-of ch))))

(defun deadp (ch)
  (= (position-of ch) +pos-dead+))

(defun immortalp (ch)
  (>= (level-of ch) 50))

(defun immortal-level-p (ch)
  (>= (level-of ch) 50))

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

(defun check-skill (ch skill)
  (aref (skills-of ch) skill))

(defun skill-of (ch skill)
  (aref (skills-of ch) skill))

(defun (setf skill-of) (val ch skill)
  (setf (aref (skills-of ch) skill) val))

(defun is-soulless (ch)
  (or (mob-flagged ch +mob-soulless+)
      (plr2-flagged ch +plr2-soulless+)))

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
(defun is-hamstrung (ch) (aff3-flagged ch +aff3-hamstrung+))
(defun has-poison-1 (ch) (aff-flagged ch +aff-poison+))
(defun has-poison-2 (ch) (aff3-flagged ch +aff3-poison-2+))

(defun has-poison-3 (ch) (aff3-flagged ch +aff3-poison-2+))
(defun is-poisoned (ch) (or (has-poison-1 ch)
                            (has-poison-2 ch)
                            (has-poison-3 ch)))
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

(defun is-remort (ch) (plusp (remort-gen-of ch)))
(defun is-npc (ch) (typep ch 'mobile))
(defun is-undead (ch) (= (race-of ch) +race-undead+))

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
  nil)

(defun get-level-bonus (ch)
  0)

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
  (when (link-of ch)
    (incf (wait-of (link-of ch)) pulses)))

(defun affect-remove (ch af)
  (setf (affects-of ch) (delete af (affects-of ch))))