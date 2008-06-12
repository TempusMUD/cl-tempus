(in-package :tempus)

(defparameter +race-human+ 0)
(defparameter +race-elf+ 1)
(defparameter +race-dwarf+ 2)
(defparameter +race-half-orc+ 3)
(defparameter +race-klingon+ 4)
(defparameter +race-halfling+ 5)
(defparameter +race-tabaxi+ 6)
(defparameter +race-drow+ 7)
(defparameter +race-mobile+ 10)
(defparameter +race-undead+ 11)
(defparameter +race-humanoid+ 12)
(defparameter +race-animal+ 13)
(defparameter +race-dragon+ 14)
(defparameter +race-giant+ 15)
(defparameter +race-orc+ 16)
(defparameter +race-goblin+ 17)
(defparameter +race-hafling+ 18)
(defparameter +race-minotaur+ 19)
(defparameter +race-troll+ 20)
(defparameter +race-golem+ 21)
(defparameter +race-elemental+ 22)
(defparameter +race-ogre+ 23)
(defparameter +race-devil+ 24)
(defparameter +race-troglodyte+ 25)
(defparameter +race-manticore+ 26)
(defparameter +race-bugbear+ 27)
(defparameter +race-draconian+ 28)
(defparameter +race-duergar+ 29)
(defparameter +race-slaad+ 30)
(defparameter +race-robot+ 31)
(defparameter +race-demon+ 32)
(defparameter +race-deva+ 33)
(defparameter +race-plant+ 34)
(defparameter +race-archon+ 35)
(defparameter +race-pudding+ 36)
(defparameter +race-alien-1+ 37)
(defparameter +race-pred-alien+ 38)
(defparameter +race-slime+ 39)
(defparameter +race-illithid+ 40)
(defparameter +race-fish+ 41)
(defparameter +race-beholder+ 42)
(defparameter +race-gaseous+ 43)
(defparameter +race-githyanki+ 44)
(defparameter +race-insect+ 45)
(defparameter +race-daemon+ 46)
(defparameter +race-mephit+ 47)
(defparameter +race-kobold+ 48)
(defparameter +race-umber-hulk+ 49)
(defparameter +race-wemic+ 50)
(defparameter +race-rakshasa+ 51)
(defparameter +race-spider+ 52)
(defparameter +race-griffin+ 53)
(defparameter +race-rotarian+ 54)
(defparameter +race-half-elf+ 55)
(defparameter +race-celestial+ 56)
(defparameter +race-guardinal+ 57)
(defparameter +race-olympian+ 58)
(defparameter +race-yugoloth+ 59)
(defparameter +race-rowlahr+ 60)
(defparameter +race-githzerai+ 61)

(defparameter +sex-neuter+ 0)
(defparameter +sex-male+ 1)
(defparameter +sex-female+ 2)

(defparameter +mob-spec+ (ash 1 0))	; Mob has a callable spec-proc
(defparameter +mob-sentinel+ (ash 1 1))	; Mob should not move
(defparameter +mob-scavenger+ (ash 1 2))	; Mob picks up stuff on the ground
(defparameter +mob-isnpc+ (ash 1 3))	; (R) Automatically set on all Mobs
(defparameter +mob-aware+ (ash 1 4))	; Mob can't be backstabbed
(defparameter +mob-aggressive+ (ash 1 5))	; Mob hits players in the room
(defparameter +mob-stay-zone+ (ash 1 6))	; Mob shouldn't wander out of zone
(defparameter +mob-wimpy+ (ash 1 7))	; Mob flees if severely injured
(defparameter +mob-aggr-evil+ (ash 1 8))	; auto attack evil PC's
(defparameter +mob-aggr-good+ (ash 1 9))	; auto attack good PC's
(defparameter +mob-aggr-neutral+ (ash 1 10))	; auto attack neutral PC's
(defparameter +mob-memory+ (ash 1 11))	; remember attackers if attacked
(defparameter +mob-helper+ (ash 1 12))	; attack PCs fighting other NPCs
(defparameter +mob-nocharm+ (ash 1 13))	; Mob can't be charmed
(defparameter +mob-nosummon+ (ash 1 14))	; Mob can't be summoned
(defparameter +mob-nosleep+ (ash 1 15))	; Mob can't be slept
(defparameter +mob-nobash+ (ash 1 16))	; Mob can't be bashed (e.g. trees)
(defparameter +mob-noblind+ (ash 1 17))	; Mob can't be blinded
(defparameter +mob-noturn+ (ash 1 18))	; Hard to turn
(defparameter +mob-nopetri+ (ash 1 19))	; Cannot be petrified
(defparameter +mob-pet+ (ash 1 20))	; Mob is a conjured pet and shouldn't
										 ; get nor give any xp in any way.
(defparameter +mob-soulless+ (ash 1 21))	; Mobile is Soulless - Unholy compact.
(defparameter +mob-spirit-tracker+ (ash 1 22))	; Can track through !track
(defparameter +mob-utility+ (ash 1 23)) ; Can't be seen, hit, etc...

(defparameter +mob2-script+ (ash 1 0))
(defparameter +mob2-mount+ (ash 1 1))
(defparameter +mob2-stay-sect+ (ash 1 2))	; Can't leave SECT-type.
(defparameter +mob2-atk-mobs+ (ash 1 3))	; Aggro Mobs will attack other mobs
(defparameter +mob2-hunt+ (ash 1 4))	; Mob will hunt attacker
(defparameter +mob2-looter+ (ash 1 5))	; Loots corpses
(defparameter +mob2-nostun+ (ash 1 6))
(defparameter +mob2-seller+ (ash 1 7))	; If shopkeeper, sells anywhere.
(defparameter +mob2-wont-wear+ (ash 1 8))	; Wont wear shit it picks up (SHPKPER)
(defparameter +mob2-silent-hunter+ (ash 1 9))
(defparameter +mob2-familiar+ (ash 1 10))	; mages familiar
(defparameter +mob2-no-flow+ (ash 1 11))	; Mob doesn't flow
(defparameter +mob2-unapproved+ (ash 1 12))	; Mobile not approved for game play
(defparameter +mob2-renamed+ (ash 1 13))	; Mobile renamed
(defparameter +mob2-noaggro-race+ (ash 1 14))	; wont attack members of own race

(defparameter +pref-brief+ 0) ; Room descs won't normally be shown
(defparameter +pref-nohaggle+ 1) ;
(defparameter +pref-deaf+ 2)     ; Can't hear shouts
(defparameter +pref-notell+ 3)   ; Can't receive tells
(defparameter +pref-disphp+ 4)	; Display hit points in prompt
(defparameter +pref-dispmana+ 5) ; Display mana points in prompt
(defparameter +pref-dispmove+ 6) ; Display move points in prompt
(defparameter +pref-autoexit+ 7) ; Display exits in a room
(defparameter +pref-nohassle+ 8) ; Aggr mobs won't attack
(defparameter +pref-nasty+ 9) ; Can hear nasty words on channel
(defparameter +pref-summonable+ 10)	; Can be summoned
(defparameter +pref-unused-2+ 11) ; No repetition of comm commands
(defparameter +pref-holylight+ 12)	; Can see in dark
(defparameter +pref-color-1+ 13)	; Color (low bit)
(defparameter +pref-color-2+ 14)	; Color (high bit)
(defparameter +pref-nowiz+ 15)      ; Can't hear wizline
(defparameter +pref-log1+ 16)	; On-line System Log (low bit)
(defparameter +pref-log2+ 17)  ; On-line System Log (high bit)
(defparameter +pref-noauct+ 18)	; Can't hear auction channel
(defparameter +pref-nogoss+ 19)	; Can't hear gossip channel
(defparameter +pref-nogratz+ 20)	; Can't hear grats channel
(defparameter +pref-roomflags+ 21) ; Can see room flags (ROOM-x)
(defparameter +pref-nosnoop+ 22) ; Can not be snooped by immortals
(defparameter +pref-nomusic+ 23) ; Can't hear music channel
(defparameter +pref-nospew+ 24)  ; Can't hear spews
(defparameter +pref-gagmiss+ 25) ; Doesn't see misses during fight
(defparameter +pref-noproject+ 26) ; Cannot hear the remort channel
(defparameter +pref-nopetition+ 27) ;
(defparameter +pref-noclansay+ 28) ; Doesnt hear clan says and such
(defparameter +pref-noidentify+ 29)	; Saving throw is made when id'd
(defparameter +pref-nodream+ 30)

;; PREF 2 Flags

(defparameter +pref-debug+ 31)          ; Sees info on fight.
(defparameter +pref-newbie-helper+ 32)	; sees newbie arrivals
(defparameter +pref-auto-diagnose+ 33) ; automatically see condition of enemy
(defparameter +pref-autopage+ 34) ; Beeps when ch receives a tell
(defparameter +pref-noaffects+ 35) ; Affects are not shown in score
(defparameter +pref-noholler+ 36)  ; Gods only
(defparameter +pref-noimmchat+ 37) ; Gods only
(defparameter +pref-unused-1+ 38) ; auto-sets title to clan stuff
(defparameter +pref-clan-hide+ 39) ; don't show badge in who list
(defparameter +pref-unused-2+ 40) ; interrupts while d->showstr-point
(defparameter +pref-autoprompt+ 41)	; always draw new prompt
(defparameter +pref-nowho+ 42)      ; don't show in who
(defparameter +pref-anonymous+ 43) ; don't show char-class, level
(defparameter +pref-notrailers+ 44)	; don't show trailer affects
(defparameter +pref-vt100+ 45)	; Players uses VT100 inferface
(defparameter +pref-autosplit+ 46) ;
(defparameter +pref-autoloot+ 47)  ;
(defparameter +pref-pkiller+ 48) ; player can attack other players
(defparameter +pref-nogecho+ 49) ; Silly Gecho things
(defparameter +pref-nowrap+ 50)	; turns off autowrap temporarily.
(defparameter +pref-dispalign+ 51)  ;
(defparameter +pref-worldwrite+ 52) ; allows worldwrite to work
(defparameter +pref-noguildsay+ 53) ;
(defparameter +pref-disptime+ 54) ; show localtime in the prompt
(defparameter +pref-disp-vnums+ 55) ; show vnums after items ldesc
(defparameter +pref-count+ 56)

(defun is-race (ch race)
  (= (race-of ch) race))
(defun is-goblin (ch)
  (is-race ch +race-goblin+))
(defun is-orc (ch)
  (is-race ch +race-orc+))
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
   (number :accessor number-of :initarg :number :initform nil)
   (attack-type :accessor attack-type-of :initarg :attack-type :initform nil)
   (lair :accessor lair-of :initarg :lair :initform nil)
   (leader :accessor leader-of :initarg :leader :initform nil)
   (kills :accessor kills-of :initarg :kills :initform nil)
   (loaded :accessor loaded-of :initarg :loaded :initform nil)
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
   (equipment :accessor equipment-of :initarg :equipment :initform nil)
   (implants :accessor implants-of :initarg :implants :initform nil)
   (tattoos :accessor tattoos-of :initarg :tattoos :initform nil)
   (carrying :accessor carrying-of :initarg :carrying :initform nil)
   (link :accessor link-of :initform nil)
   (account :accessor account-of :initarg :account :initform nil)
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

   ;; Originally from char_special_data_saved
   (idnum :accessor idnum-of :initarg :idnum :initform nil)
   (remort-gen :accessor remort-gen-of :initarg :remort-gen :initform nil)
   (saves :accessor saves-of :initarg :saves :initform nil)
   (aff-flags :accessor aff-flags-of :initarg :aff-flags :initform nil)
   (aff2-flags :accessor aff2-flags-of :initarg :aff2-flags :initform nil)
   (aff3-flags :accessor aff3-flags-of :initarg :aff3-flags :initform nil)
   (mob-flags :accessor mob-flags-of :initarg :mob-flags :initform nil)
   (mob2-flags :accessor mob2-flags-of :initarg :mob2-flags :initform nil)
   (alignment :accessor alignment-of :initarg :alignment :initform nil)

   ;; Originally from char_player_data
   (name :accessor name-of :initarg :name :initform nil)
   (short-descr :accessor short-descr-of :initarg :short-descr :initform nil)
   (long-descr :accessor long-descr-of :initarg :long-descr :initform nil)
   (description :accessor description-of :initarg :description :initform nil)
   (title :accessor title-of :initarg :title :initform nil)
   (char-class :accessor char-class-of :initarg :char-class :initform nil)
   (remort-char-class :accessor remort-char-class-of :initarg :remort-char-class :initform nil)
   (weight :accessor weight-of :initarg :weight :initform nil)
   (height :accessor height-of :initarg :height :initform nil)
   (hometown :accessor hometown-of :initarg :hometown :initform nil)
   (sex :accessor sex-of :initarg :sex :initform nil)
   (race :accessor race-of :initarg :race :initform nil)
   (level :accessor level-of :initarg :level :initform nil)
   (age-adjust :accessor age-adjust-of :initarg :age-adjust :initform nil)
   (age :accessor age-of :initarg :age :initform nil)
   (birth-time :accessor birth-time-of :initarg :birth-time :initform nil)
   (login-time :accessor login-time-of :initarg :birth-time :initform nil)
   (death-time :accessor death-time-of :initarg :birth-time :initform nil)
   (played-time :accessor played-time-of :initarg :birth-time :initform nil)

   ;; Originally from char_special_data
   (defending :accessor defending-of :initarg :defending :initform nil)
   (hunting :accessor hunting-of :initarg :hunting :initform nil)
   (mounted :accessor mounted-of :initarg :mounted :initform nil)
   (carry-weight :accessor carry-weight-of :initarg :carry-weight :initform nil)
   (worn-weight :accessor worn-weight-of :initarg :worn-weight :initform nil)
   (timer :accessor timer-of :initarg :timer :initform nil)
   (meditate-timer :accessor meditate-timer-of :initarg :meditate-timer :initform nil)
   (cur-flow-pulse :accessor cur-flow-pulse-of :initarg :cur-flow-pulse :initform nil)
   (breath-count :accessor breath-count-of :initarg :breath-count :initform nil)
   (fall-count :accessor fall-count-of :initarg :fall-count :initform nil)
   (position :accessor position-of :initarg :position :initform nil)
   (carry-items :accessor carry-items-of :initarg :carry-items :initform nil)
   (weapon-proficiency :accessor weapon-proficiency-of :initarg :weapon-proficiency :initform nil)
   (mood :accessor mood-of :initarg :mood :initform nil)
   (saved :accessor saved-of :initarg :saved :initform nil)

   ;; Originally from char_language_data
   (tongues-heard :accessor tongues-heard-of :initarg :tongues-heard :initform nil)
   (tongues :accessor tongues-of :initarg :tongues
            :initform (make-array +max-tongues+ :initial-element nil))
   (current-tongue :accessor current-tongue-of :initarg :current-tongue :initform nil)
   ))

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
  ((saved :accessor saved-of :initarg :saved :initform nil)
   (poofin :accessor poofin-of :initarg :poofin :initform nil)
   (poofout :accessor poofout-of :initarg :poofout :initform nil)
   (aliases :accessor aliases-of :initarg :aliases :initform nil)
   (last-tell-from :accessor last-tell-from-of :initarg :last-tell-from :initform nil)
   (last-tell-to :accessor last-tell-to-of :initarg :last-tell-to :initform nil)
   (imprint-rooms :accessor imprint-rooms-of :initarg :imprint-rooms :initform nil)
   (recently-killed :accessor recently-killed-of :initarg :recently-killed :initform nil)
   (soilage :accessor soilage-of :initarg :soilage :initform nil)
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
   (skills :accessor skills-of :initarg :skills :initform nil)
   (weap-spec :accessor weap-spec-of :initarg :weap-spec :initform nil)
   (wimp-level :accessor wimp-level-of :initarg :wimp-level :initform nil)
   (freeze-level :accessor freeze-level-of :initarg :freeze-level :initform nil)
   (invis-level :accessor invis-level-of :initarg :invis-level :initform nil)
   (load-room :accessor load-room-of :initarg :load-room :initform nil)
   (home-room :accessor home-room-of :initarg :home-room :initform nil)
   (prefs :accessor prefs-of :initarg :pref :initform nil)
   (conditions :accessor conditions-of :initarg :conditions :initform nil)
   (clan :accessor clan-of :initarg :clan :initform nil)
   (broken-component :accessor broken-component-of :initarg :broken-component :initform nil)
   (imm-qp :accessor imm-qp-of :initarg :imm-qp :initform nil)
   (qlog-level :accessor qlog-level-of :initarg :qlog-level :initform nil)
   (speed :accessor speed-of :initarg :speed :initform nil)
   (qp-allowance :accessor qp-allowance-of :initarg :qp-allowance :initform nil)
   (badge :accessor badge-of :initarg :badge :initform nil)
   (deity :accessor deity-of :initarg :deity :initform nil)
   (life-points :accessor life-points-of :initarg :life-points :initform nil)
   (pkills :accessor pkills-of :initarg :pkills :initform nil)
   (akills :accessor akills-of :initarg :akills :initform nil)
   (mobkills :accessor mobkills-of :initarg :mobkills :initform nil)
   (deaths :accessor deaths-of :initarg :deaths :initform nil)
   (old-char-class :accessor old-char-class-of :initarg :old-char-class :initform nil)
   (total-dam :accessor total-dam-of :initarg :total-dam :initform nil)
   (hold-load-room :accessor hold-load-room-of :initarg :hold-load-room :initform nil)
   (quest-id :accessor quest-id-of :initarg :quest-id :initform nil)
   (plr2-bits :accessor plr2-bits-of :initarg :plr2-bits :initform nil)
   (reputation :accessor reputation-of :initarg :reputation :initform nil)
   (killer-severity :accessor killer-severity-of :initarg :killer-severity :initform nil)
   (mana-shield-low :accessor mana-shield-low-of :initarg :mana-shield-low :initform nil)
   (mana-shield-pct :accessor mana-shield-pct-of :initarg :mana-shield-pct :initform nil)))

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

(defmethod modify-weight ((ch creature) mod-weight)
  (incf (weight-of ch) mod-weight))

(defmethod modify-carried-weight ((ch creature) mod-weight)
  (incf (carry-weight-of ch) mod-weight))

(defmethod modify-worn-weight ((ch creature) mod-weight)
  (incf (worn-weight-of ch) mod-weight))

(defun pref-flagged (ch pref)
  (bitp (prefs-of ch) pref))

(defun mob-flagged (ch flag)
  (logtest flag (mob-flags-of ch)))

(defun mob2-flagged (ch flag)
  (logtest flag (mob2-flags-of ch)))

(defun immortalp (ch)
  (>= (level-of ch) 50))

(defun noncorporealp (ch)
  nil)