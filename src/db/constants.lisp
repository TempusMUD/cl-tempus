(in-package #:tempus)

(defparameter +opt-usec+ 100000)        ; 10 passes per second
(defparameter +passes-per-sec+ (floor 1000000 +opt-usec+))
(defun rl-sec (pulses)
  (* pulses +passes-per-sec+))

(defparameter +pulse-zone+ (rl-sec 10))
(defparameter +pulse-mobile+ (rl-sec 4))
(defparameter +pulse-mobile-spec+ (rl-sec 2))
(defparameter +pulse-violence+ (rl-sec 2))
(defparameter +seg-violence+ 7)
(defparameter +fire-tick+ (rl-sec 3))
(defparameter +pulse-flows+ (rl-sec 1))

(defparameter +ansi-levels+ '("none" "sparse" "normal" "complete"))
(defparameter +compact-levels+ '("off" "minimal" "partial" "full"))

(defparameter +zone-flags+
  #("AUTOSAVE"
    "RESETSAVE"
    "NOTIFYOWNER"
    "LOCKED"
    "!MAGIC"
    "!LAW"
    "!WEATHER"
    "!CRIME"
    "FROZEN"
    "ISOLATED"
    "SNDPRF"
    "NOIDLE"
    "FULLCONTROL"
    "PAUSED"
    "EVILAMBIENCE"
    "GOODAMBIENCE"
    "NORECALC"
    "17"
    "18"
    "SEARCH_APPR"
    "MOBS_APPR"
    "OBJS_APPR"
    "ROOMS_APPR"
    "ZCMDS_APPR"
    "INPLAY"
    "MOBS_MOD"
    "OBJS_MOD"
    "RMS_MOD"
    "ZON_MOD"))

(defparameter +reset-mode+
  #("NEVER" "EMPTY" "ALWAYS"))
(defparameter +zone-pk-flags+
  #("!PK" "NPK" "CPK"))

(defparameter +dirs+ #("north" "east" "south" "west" "up" "down" "future" "past"))
(defparameter +to-dirs+ #("north" "east" "south" "west" "up" "down" "into the future" "into the past"))
(defparameter +from-dirs+ #("the south" "the west" "the north" "the east" "below" "above" "the past" "the future"))
(defparameter +rev-dir+ #(2 3 0 1 5 4 7 6))
(defparameter +num-of-dirs+ (length +dirs+))

(defparameter +room-bits+
  #("DRK" "DTH" "!MOB" "IND" "NV" "SDPF" "!TRK" "!MAG" "TNL" "!TEL"
    "GDR" "HAS" "HCR" "COMFORT" "SMOKE" "!FLEE" "!PSI" "!SCI" "!RCL"
    "CLAN" "ARENA" "DOCK" "BURN" "FREEZ" "NULLMAG" "HOLYO" "RAD"
    "SLEEP" "EXPLOD" "POISON" "VACUUM" "\n"))

(defparameter +room-flags+
  #("dark" "deathtrap" "nomob" "indoor" "noviolence" "soundproof"
    "notrack" "nomagic" "RES (tunnel)" "noteleport" "godroom"
    "RES (house)" "RES (house crash)" "comfortable" "smoke-filled"
    "noflee" "nopsionics" "noscience" "norecall" "clanroom" "arena"
    "dock" "burning" "freezing" "nullmagic" "holyocean" "radioactive"
    "sleep gas" "explosive gas" "poison gas" "vacuum"))

(defparameter +exit-bits+
  #("DOOR" "CLOSED" "LOCKED" "PICKPROOF" "HEAVY" "HARD-PICK"
    "!MOB" "HIDDEN" "!SCAN" "TECH" "ONE-WAY" "NOPASS"
    "THORNS" "THORNS_NOPASS" "STONE" "ICE" "FIRE" "FIRE_NOPASS"
    "FLESH" "IRON" "ENERGY_F" "ENERGY_F_NOPASS" "FORCE" "SPECIAL"
    "REINF" "SECRET"))

(defparameter +exit-flags+
  #("door" "closed" "locked" "pickproof" "heavy" "hardpick"
    "nomob" "hidden" "noscan" "tech" "oneway" "nopass"
    "thorns" "thornsnopass" "stone" "ice" "fire" "firenopass"
    "flesh" "iron" "energy" "energynopass" "force" "special"
    "reinf" "secret"))

(defparameter +sector-types+
  #("inside" "city" "field" "forest" "hills" "mountains"
    "water (swim)" "water (no swim)" "underwater" "open air" "notime"
    "climbing" "outer space" "road" "vehicle" "farmland" "swamp"
    "desert" "fire river" "jungle" "pitch surface" "pitch submerged"
    "beach" "astral" "elemental fire" "elemental earth" "elemental air"
    "elemental water" "elemental positive" "elemental negative"
    "elemental smoke" "elemental ice" "elemental ooze"
    "elemental magma" "elemental lightning" "elemental steam"
    "elemental radiance" "elemental minerals" "elemental vacuum"
    "elemental salt" "elemental ash" "elemental dust" "blood" "rock"
    "muddy" "trail" "tundra" "catacombs" "cracked road" "deep ocean"))

;;; Room search commands
(defparameter +search-com-none+ 0)
(defparameter +search-com-door+ 1)
(defparameter +search-com-mobile+ 2)
(defparameter +search-com-object+ 3)
(defparameter +search-com-remove+ 4)
(defparameter +search-com-give+ 5)
(defparameter +search-com-equip+ 6)
(defparameter +search-com-transport+ 7)
(defparameter +search-com-spell+ 8)
(defparameter +search-com-damage+ 9)
(defparameter +search-com-spawn+ 10)
(defparameter +search-com-loadroom+ 11)
(defparameter +num-search-com+ 12)

;;; Room search flags
(defparameter +search-repeatable+ (ash 1 0))
(defparameter +search-tripped+ (ash 1 1))
(defparameter +search-ignore+ (ash 1 2))
(defparameter +search-clanpasswd+ (ash 1 3))
(defparameter +search-trig-enter+ (ash 1 4))
(defparameter +search-trig-fall+ (ash 1 5)) ; triggers when a player falls into the room (e.g. a spike pit)
(defparameter +search-notrig-fly+ (ash 1 6))
(defparameter +search-nomob+ (ash 1 7))
(defparameter +search-newbie-only+ (ash 1 8))
(defparameter +search-nomessage+ (ash 1 9))
(defparameter +search-noevil+ (ash 1 10))
(defparameter +search-noneutral+ (ash 1 11))
(defparameter +search-nogood+ (ash 1 12))
(defparameter +search-nomage+ (ash 1 13))
(defparameter +search-nocleric+ (ash 1 14))
(defparameter +search-nothief+ (ash 1 15))
(defparameter +search-nobarb+ (ash 1 16))
(defparameter +search-noranger+ (ash 1 17))
(defparameter +search-noknight+ (ash 1 18))
(defparameter +search-nomonk+ (ash 1 19))
(defparameter +search-nopsionic+ (ash 1 20))
(defparameter +search-nophysic+ (ash 1 21))
(defparameter +search-nomerc+ (ash 1 22))
(defparameter +search-nobard+ (ash 1 23))
(defparameter +search-noabbrev+ (ash 1 24))
(defparameter +search-noaffmob+ (ash 1 25))
(defparameter +search-noplayer+ (ash 1 26))
(defparameter +search-remort-only+ (ash 1 27))
(defparameter +search-match-all+ (ash 1 28))
(defparameter +search-no-look+ (ash 1 29))
(defparameter +search-fail-trip+ (ash 1 30))

(defparameter +num-search-bits+ 31)

(defparameter +search-commands+
  #("None" "Door" "Mobile" "Object" "Remove" "Give" "Equip"
    "Transport" "Spell" "Damage" "Spawn" "Loadroom"))

(defparameter +search-bits+
  #("REPEATABLE" "TRIPPED" "IGNORE" "CLANPASSWD" "TRIG_ENTER" "TRIG_FALL"
    "NOTRIG_FLY" "NOMOB" "NEWBIE_ONLY" "NOMSG" "NOEVIL" "NONEU" "NOGOOD"
    "NOMAGE" "NOCLER" "NOTHI" "NOBARB" "NORANG" "NOKNI" "NOMONK" "NOPSI"
    "NOPHY" "NOMERC" "NOBARD" "NOABBREV" "NOAFFMOB" "NOPLAYER"
    "REMORT_ONLY" "MATCH_ALL" "NOLOOK" "FAIL_TRIP"))

(defparameter +planes+
  #("Prime One"
    "Prime Two"
    "Neverwhere"
    "Underdark"
    "Western"
    "Morbidian"
    "Prime Seven"
    "Prime Eight"
    "Prime Nine"
    "Prime Ten"
    "Astral"
    "Avernus--Hell"
    "Dis--Hell"
    "Minauros--Hell"
    "Phlegethos--Hell"
    "Stygia--Hell"
    "Malbolge--Hell"
    "Maladomini--Hell"
    "Caina--Hell"
    "Nessus--Hell"
    "Ghenna"
    "21"
    "22"
    "23"
    "24"
    "The Abyss"
    "26"
    "27"
    "28"
    "29"
    "30"
    "31"
    "32"
    "33"
    "34"
    "35"
    "36"
    "37"
    "38"
    "OLC"
    "Olympus"
    "Costal"
    "42"
    "Heaven"
    "Elysium"
    "45"
    "46"
    "47"
    "48"
    "49"
    "DOOM"
    "Shadow"
    "52"
    "53"
    "54"
    "55"
    "56"
    "57"
    "58"
    "59"
    "60"
    "61"
    "62"
    "63"
    "64"
    "65"
    "Odyssey"
    "Paraelemental Smoke"
    "Paraelemental Ice"
    "Paraelemental Magma"
    "Elemental Water"
    "Elemental Fire"
    "Elemental Air"
    "Elemental Earth"
    "Elemental Positive"
    "Elemental Negative"
    "Paraelemental Magma"
    "Paraelemental Ooze"))

(defparameter +time-frames+
  #("Timeless" "Modrian Era" "Electro Era"))

(defparameter +default-staff-lvl+ 12)
(defparameter +default-wand-lvl+ 12)

(defparameter +cast-undefined+ -1)
(defparameter +cast-spell+ 0)
(defparameter +cast-potion+ 1)
(defparameter +cast-wand+ 2)
(defparameter +cast-staff+ 3)
(defparameter +cast-scroll+ 4)
(defparameter +cast-para+ 5)
(defparameter +cast-petri+ 6)
(defparameter +cast-rod+ 7)
(defparameter +cast-breath+ 8)
(defparameter +cast-chem+ 9)
(defparameter +cast-psionic+ 10)
(defparameter +cast-physic+ 11)
(defparameter +cast-internal+ 12)
(defparameter +cast-mercenary+ 13)
(defparameter +cast-bard+ 14)

(defparameter +mag-damage+ (ash 1 0))
(defparameter +mag-affects+ (ash 1 1))
(defparameter +mag-unaffects+ (ash 1 2))
(defparameter +mag-points+ (ash 1 3))
(defparameter +mag-alter-objs+ (ash 1 4))
(defparameter +mag-groups+ (ash 1 5))
(defparameter +mag-masses+ (ash 1 6))
(defparameter +mag-areas+ (ash 1 7))
(defparameter +mag-summons+ (ash 1 8))
(defparameter +mag-creations+ (ash 1 9))
(defparameter +mag-manual+ (ash 1 10))
(defparameter +mag-objects+ (ash 1 11))
(defparameter +mag-touch+ (ash 1 12))
(defparameter +mag-magic+ (ash 1 13))
(defparameter +mag-divine+ (ash 1 14))
(defparameter +mag-physics+ (ash 1 15))
(defparameter +mag-psionic+ (ash 1 16))
(defparameter +mag-biologic+ (ash 1 17))
(defparameter +cyb-activate+ (ash 1 18))
(defparameter +mag-evil+ (ash 1 19))
(defparameter +mag-good+ (ash 1 20))
(defparameter +mag-exits+ (ash 1 21))
(defparameter +mag-outdoors+ (ash 1 22))
(defparameter +mag-nowater+ (ash 1 23))
(defparameter +mag-waterzap+ (ash 1 24))
(defparameter +mag-nosun+ (ash 1 25))
(defparameter +mag-zen+ (ash 1 26))
(defparameter +mag-mercenary+ (ash 1 27))
(defparameter +mag-bard+ (ash 1 28))

(defparameter +type-undefined+ -1)
(defparameter +spell-reserved-dbc+ 0)  ; SKILL NUMBER ZERO -- RESERVED

;; Special mobile vnums
(defparameter +unholy-stalker-vnum+ 1513)
(defparameter +zombie-vnum+ 1512)

;; Special object vnums
(defparameter +quad-vnum+ 1578)

;;; PLAYER SPELLS -- Numbered from 1 to MAX_SPELLS

(defparameter +spell-armor+ 1)        ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-teleport+ 2)     ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-bless+ 3)        ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-blindness+ 4)    ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-burning-hands+ 5) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-call-lightning+ 6) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-charm+ 7)        ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-chill-touch+ 8)  ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-clone+ 9)        ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-color-spray+ 10) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-control-weather+ 11) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-create-food+ 12) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-create-water+ 13) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-cure-blind+ 14)  ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-cure-critic+ 15) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-cure-light+ 16)  ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-curse+ 17)       ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-detect-align+ 18) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-detect-invis+ 19) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-detect-magic+ 20) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-detect-poison+ 21) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-dispel-evil+ 22) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-earthquake+ 23)  ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-enchant-weapon+ 24) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-energy-drain+ 25) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-fireball+ 26)    ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-harm+ 27)        ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-heal+ 28)        ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-invisible+ 29)   ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-lightning-bolt+ 30) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-locate-object+ 31) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-magic-missile+ 32) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-poison+ 33)      ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-prot-from-evil+ 34) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-remove-curse+ 35) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-sanctuary+ 36)   ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-shocking-grasp+ 37) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-sleep+ 38)       ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-strength+ 39)    ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-summon+ 40)      ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-ventriloquate+ 41) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-word-of-recall+ 42) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-remove-poison+ 43) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-sense-life+ 44)  ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-animate-dead+ 45) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-dispel-good+ 46) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-group-armor+ 47) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-group-heal+ 48)  ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-group-recall+ 49) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-infravision+ 50) ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-waterwalk+ 51)   ; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-mana-shield+ 52)
(defparameter +spell-identify+ 53)
(defparameter +spell-glowlight+ 54)
(defparameter +spell-blur+ 55)
(defparameter +spell-knock+ 56)
(defparameter +spell-dispel-magic+ 57)
(defparameter +spell-warding-sigil+ 58)
(defparameter +spell-dimension-door+ 59)
(defparameter +spell-minor-creation+ 60)
(defparameter +spell-telekinesis+ 61)
(defparameter +spell-sword+ 62)
(defparameter +spell-word-stun+ 63)
(defparameter +spell-prismatic-spray+ 64)
(defparameter +spell-fire-shield+ 65)
(defparameter +spell-detect-scrying+ 66)
(defparameter +spell-clairvoyance+ 67)
(defparameter +spell-slow+ 68)
(defparameter +spell-greater-enchant+ 69)
(defparameter +spell-enchant-armor+ 70)
(defparameter +spell-minor-identify+ 71)
(defparameter +spell-fly+ 72)
(defparameter +spell-greater-heal+ 73)
(defparameter +spell-cone-cold+ 74)
(defparameter +spell-true-seeing+ 75)
(defparameter +spell-prot-from-good+ 76)
(defparameter +spell-magical-prot+ 77)
(defparameter +spell-undead-prot+ 78)
(defparameter +spell-spirit-hammer+ 79)
(defparameter +spell-pray+ 80)
(defparameter +spell-flame-strike+ 81)
(defparameter +spell-endure-cold+ 82)
(defparameter +spell-magical-vestment+ 83)
(defparameter +spell-rejuvenate+ 84)
(defparameter +spell-regenerate+ 85)
(defparameter +spell-command+ 86)
(defparameter +spell-air-walk+ 87)
(defparameter +spell-divine-illumination+ 88)
(defparameter +spell-goodberry+ 89)
(defparameter +spell-barkskin+ 90)
(defparameter +spell-invis-to-undead+ 91)
(defparameter +spell-haste+ 92)
(defparameter +spell-animal-kin+ 93)
(defparameter +spell-charm-animal+ 94)
(defparameter +spell-refresh+ 95)
(defparameter +spell-breathe-water+ 96)
(defparameter +spell-conjure-elemental+ 97)
(defparameter +spell-greater-invis+ 98)
(defparameter +spell-prot-from-lightning+ 99)
(defparameter +spell-prot-from-fire+ 100)
(defparameter +spell-restoration+ 101)
(defparameter +spell-word-of-intellect+ 102)
(defparameter +spell-gust-of-wind+ 103)
(defparameter +spell-retrieve-corpse+ 104)
(defparameter +spell-local-teleport+ 105)
(defparameter +spell-displacement+ 106)
(defparameter +spell-peer+ 107)
(defparameter +spell-meteor-storm+ 108)
(defparameter +spell-protect-from-devils+ 109)
(defparameter +spell-symbol-of-pain+ 110)
(defparameter +spell-icy-blast+ 111)
(defparameter +spell-astral-spell+ 112)
(defparameter +spell-vestigial-rune+ 113)
(defparameter +spell-disruption+ 114)
(defparameter +spell-stone-to-flesh+ 115)
(defparameter +spell-remove-sickness+ 116)
(defparameter +spell-shroud-obscurement+ 117)
(defparameter +spell-prismatic-sphere+ 118)
(defparameter +spell-wall-of-thorns+ 119)
(defparameter +spell-wall-of-stone+ 120)
(defparameter +spell-wall-of-ice+ 121)
(defparameter +spell-wall-of-fire+ 122)
(defparameter +spell-wall-of-iron+ 123)
(defparameter +spell-thorn-trap+ 124)
(defparameter +spell-fiery-sheet+ 125)
(defparameter +spell-chain-lightning+ 126)
(defparameter +spell-hailstorm+ 127)
(defparameter +spell-ice-storm+ 128)
(defparameter +spell-shield-of-righteousness+ 129) ; group protection
(defparameter +spell-blackmantle+ 130)      ; blocks healing spells
(defparameter +spell-sanctification+ 131)   ; 2x dam vs. evil
(defparameter +spell-stigmata+ 132)         ; causes a bleeding wound
(defparameter +spell-summon-legion+ 133)    ; knights summon devils
(defparameter +spell-entangle+ 134)         ; rangers entangle in veg.
(defparameter +spell-anti-magic-shell+ 135)
(defparameter +spell-divine-intervention+ 136)
(defparameter +spell-sphere-of-desecration+ 137)
(defparameter +spell-malefic-violation+ 138) ; cuts thru good sanct
(defparameter +spell-righteous-penetration+ 139) ; cuts thru evil sanct
(defparameter +spell-unholy-stalker+ 140) ; evil cleric hunter mob
(defparameter +spell-inferno+ 141)        ; evil cleric room affect
(defparameter +spell-vampiric-regeneration+ 142) ; evil cleric vamp. regen
(defparameter +spell-banishment+ 143)  ; evil cleric sends devils away
(defparameter +spell-control-undead+ 144) ; evil clerics charm undead
(defparameter +spell-stoneskin+ 145)      ; remort rangers stone skin
(defparameter +spell-sun-ray+ 146)        ; Good cleric remort,
                                        ; destroys undead.
(defparameter +spell-taint+ 147)    ; Evil knight remort spell, taint.
(defparameter +spell-locust-regeneration+ 148) ; Mage remort skill, drains mana
(defparameter +spell-divine-power+ 149) ; Good cleric remort skill.
(defparameter +spell-death-knell+ 150)  ; Evil cleric remort skill.
(defparameter +spell-telepathy+ 151)
(defparameter +spell-damn+ 152)
(defparameter +spell-calm+ 153)
(defparameter +spell-thorn-skin+ 154)
(defparameter +spell-envenom+ 155)
(defparameter +spell-elemental-brand+ 156)
(defparameter +spell-thorn-skin-casting+ 157)

(defparameter +spell-fire-breathing+ 158)
(defparameter +spell-frost-breathing+ 159)
(defparameter +spell-flame-of-faith+ 160)
(defparameter +spell-ablaze+ 161) ; Only here to allow an ablaze affect

;;; ************************* Psionic Triggers **************
(defparameter +spell-power+ 201)        ; Strength
(defparameter +spell-intellect+ 202)
(defparameter +spell-confusion+ 203)
(defparameter +spell-fear+ 204)
(defparameter +spell-satiation+ 205)    ; fills hunger
(defparameter +spell-quench+ 206)       ; fills thirst
(defparameter +spell-confidence+ 207)   ; sets nopain
(defparameter +spell-nopain+ 208)
(defparameter +spell-dermal-hardening+ 209)
(defparameter +spell-wound-closure+ 210)
(defparameter +spell-antibody+ 211)
(defparameter +spell-retina+ 212)
(defparameter +spell-adrenaline+ 213)
(defparameter +spell-breathing-stasis+ 214)
(defparameter +spell-vertigo+ 215)
(defparameter +spell-metabolism+ 216) ; Increased healing, hunger, thirst
(defparameter +spell-ego-whip+ 217)
(defparameter +spell-psychic-crush+ 218)
(defparameter +spell-relaxation+ 219) ; speeds mana regen, weakens char
(defparameter +spell-weakness+ 220)     ; minus str
(defparameter +spell-fortress-of-will+ 221)
(defparameter +spell-cell-regen+ 222)
(defparameter +spell-psishield+ 223)
(defparameter +spell-psychic-surge+ 224)
(defparameter +spell-psychic-conduit+ 225)
(defparameter +spell-psionic-shatter+ 226)
(defparameter +spell-id-insinuation+ 227)
(defparameter +spell-melatonic-flood+ 228)
(defparameter +spell-motor-spasm+ 229)
(defparameter +spell-psychic-resistance+ 230)
(defparameter +spell-mass-hysteria+ 231)
(defparameter +spell-group-confidence+ 232)
(defparameter +spell-clumsiness+ 233)
(defparameter +spell-endurance+ 234)
(defparameter +spell-amnesia+ 235)      ; psi remorts
(defparameter +spell-nullpsi+ 236)      ; remove psi affects
(defparameter +spell-psionic-shockwave+ 237)
(defparameter +spell-distraction+ 238)
(defparameter +spell-call-rodent+ 239)
(defparameter +spell-call-bird+ 240)
(defparameter +spell-call-reptile+ 241)
(defparameter +spell-call-beast+ 242)
(defparameter +spell-call-predator+ 243)
(defparameter +spell-spirit-track+ 244)
(defparameter +spell-psychic-feedback+ 245)

;;; *************************  Mercenary Devices *****************
(defparameter +spell-decoy+ 237)
;;; ************************** Physic Alterations ****************
(defparameter +spell-acidity+ 301)
(defparameter +spell-attraction-field+ 302)
(defparameter +spell-nuclear-wasteland+ 303)
(defparameter +spell-fluoresce+ 304)
(defparameter +spell-gamma-ray+ 305)
(defparameter +spell-halflife+ 306)
(defparameter +spell-microwave+ 307)
(defparameter +spell-oxidize+ 308)
(defparameter +spell-random-coordinates+ 309) ; random teleport
(defparameter +spell-repulsion-field+ 310)
(defparameter +spell-transmittance+ 311)        ; transparency
(defparameter +spell-spacetime-imprint+ 312) ; sets room as teleport spot
(defparameter +spell-spacetime-recall+ 313) ; teleports to imprint telep spot
(defparameter +spell-time-warp+ 314) ; random teleport into other time
(defparameter +spell-tidal-spacewarp+ 315)  ; fly
(defparameter +spell-fission-blast+ 316)    ; full-room damage
(defparameter +spell-refraction+ 317)       ; like displacement
(defparameter +spell-electroshield+ 318)    ; prot_lightning
(defparameter +spell-vacuum-shroud+ 319) ; eliminates breathing and fire
(defparameter +spell-densify+ 320)     ; increase weight of obj & char
(defparameter +spell-chemical-stability+ 321) ; prevent/stop acidity
(defparameter +spell-entropy-field+ 322) ; drains move on victim (time effect)
(defparameter +spell-gravity-well+ 323)  ; time effect crushing damage
(defparameter +spell-capacitance-boost+ 324) ; increase maxmv
(defparameter +spell-electric-arc+ 325)      ; lightning bolt
(defparameter +spell-sonic-boom+ 326)   ; area damage + wait state
(defparameter +spell-lattice-hardening+ 327) ; dermal hard or increase obj maxdam
(defparameter +spell-nullify+ 328)           ; like dispel magic
(defparameter +spell-force-wall+ 329)        ; sets up an exit blocker
(defparameter +spell-unused-330+ 330)
(defparameter +spell-phasing+ 331)              ; invuln.
(defparameter +spell-absorption-shield+ 332) ; works like mana shield
(defparameter +spell-temporal-compression+ 333) ; works like haste
(defparameter +spell-temporal-dilation+ 334)    ; works like slow
(defparameter +spell-gauss-shield+ 335)     ; half damage from metal
(defparameter +spell-albedo-shield+ 336)    ; reflects e/m attacks
(defparameter +spell-thermostatic-field+ 337) ; sets prot_heat + end_cold
(defparameter +spell-radioimmunity+ 338)      ; sets prot_rad
(defparameter +spell-transdimensionality+ 339) ; randomly teleport to another plane
(defparameter +spell-area-stasis+ 340)         ; sets !phy room flag
(defparameter +spell-electrostatic-field+ 341) ; protective static field does damage to attackers
(defparameter +spell-emp-pulse+ 342) ; Shuts off devices, communicators
                                        ; deactivats all cyborg programs
                                        ; blocked by emp shield
(defparameter +spell-quantum-rift+ 343) ; Shuts off devices, communicators
(defparameter +spell-item-repulsion-field+ 344)
(defparameter +spell-item-attraction-field+ 345)
(defparameter +spell-dimensional-shift+ 498) ;added at top of spells to avoid interference with future bard songs
(defparameter +spell-dimensional-void+ 499) ;This is the negative affect thrown by dimensional shift

;;; *********************** Bard songs HERE man

(defparameter +song-instant-audience+ 346) ; conjures an audience, like summon elem
(defparameter +song-wall-of-sound+ 347) ; seals an exit, broken by shatter
(defparameter +song-lament-of-longing+ 349) ; creates a portal to another character
(defparameter +song-misdirection-melisma+ 350) ; misleads a tracker
(defparameter +song-aria-of-armament+ 351) ; Similar to armor, group spell
(defparameter +song-lullaby+ 352)          ; puts a room to sleep
(defparameter +song-verse-of-vulnerability+ 353) ; lowers AC of target
(defparameter +song-exposure-overture+ 354) ; Area affect, causes targets to vis
(defparameter +song-verse-of-vibration+ 355) ; motor spasm++
(defparameter +song-regalers-rhapsody+ 356) ; caster and groupies get satiated
(defparameter +song-melody-of-mettle+ 357) ; caster and group get con and maxhit
(defparameter +song-lustration-melisma+ 358) ; caster and group cleansed of blindness, poison, sickness
(defparameter +song-defense-ditty+ 359) ; save spell, psi, psy based on gen
(defparameter +song-alrons-aria+ 360)   ; singer/group confidence
(defparameter +song-song-shield+ 361)   ; self only, like psi block
(defparameter +song-verse-of-valor+ 362) ; self/group increase hitroll
(defparameter +song-hymn-of-peace+ 363) ; stops fighting in room, counters req of rage
(defparameter +song-song-of-silence+ 364) ; Area, disallow speaking, casting. singing
(defparameter +song-drifters-ditty+ 365)  ; self/group increases move
(defparameter +song-unravelling-diapason+ 366)   ;dispel magic
(defparameter +song-rhapsody-of-depression+ 367) ; Area, slow all but grouped
(defparameter +song-chant-of-light+ 368) ; group, light and prot_cold
(defparameter +song-aria-of-asylum+ 369) ; self/group/target up to 25 percent dam reduction
(defparameter +song-white-noise+ 370)    ; single target, confusion
(defparameter +song-rhythm-of-rage+ 371) ; self only, berserk, counter = hymn of peace
(defparameter +song-power-overture+ 372) ; self only, increase strength and hitroll
(defparameter +song-guiharias-glory+ 373) ; self/group, + damroll
(defparameter +song-sirens-song+ 374)     ; single target, charm
(defparameter +song-sonic-disruption+ 375)    ; area, medium damage
(defparameter +song-mirror-image-melody+ 376) ; causes multiple images of the singer
(defparameter +song-clarifying-harmonies+ 377) ; identify
(defparameter +song-unladen-swallow-song+ 378) ; group flight
(defparameter +song-irresistable-dance+ 379)   ; Target, -hitroll
(defparameter +song-rhythm-of-alarm+ 380) ; room affect, notifies the bard of person entering room
(defparameter +song-rhapsody-of-remedy+ 381) ; self/target, heal
(defparameter +song-shatter+ 382) ; target; damage persons/objects, penetrate WALL O SOUND
(defparameter +song-home-sweet-home+ 383)     ; recall
(defparameter +song-weight-of-the-world+ 384) ; self/group/target, like telekinesis
(defparameter +song-purple-haze+ 385) ; area, pauses fighting for short time
(defparameter +song-wounding-whispers+ 386) ; self, like blade barrier
(defparameter +song-dirge+ 387)             ; area, high damage
(defparameter +song-eagles-overture+ 388) ; self, group, target, increases cha
(defparameter +song-ghost-instrument+ 389) ; causes instrument to replay next some played
(defparameter +song-lichs-lyrics+ 390) ; area, only affects living creatures
(defparameter +song-fortissimo+ 391) ; increases damage of sonic attacks
(defparameter +song-insidious-rhythm+ 392) ; target, - int
(defparameter +song-requiem+ 395) ; allows bard song to affect undead at half potency

;;; if you add a new bard skill/song here, change the NUM_BARD_SONGS const in act.bard.cc

;;; ********************  MONK ZENS  ******************
(defparameter +zen-healing+ 401)
(defparameter +zen-awareness+ 402)
(defparameter +zen-oblivity+ 403)
(defparameter +zen-motion+ 404)
(defparameter +zen-translocation+ 405)
(defparameter +zen-celerity+ 406)
(defparameter +zen-dispassion+ 407)
(defparameter +max-spells+ 500)

;;; PLAYER SKILLS - Numbered from MAX_SPELLS+1 to MAX_SKILLS
(defparameter +skill-backstab+ 501)   ; Reserved Skill[] DO NOT CHANGE
(defparameter +skill-bash+ 502)       ; Reserved Skill[] DO NOT CHANGE
(defparameter +skill-hide+ 503)       ; Reserved Skill[] DO NOT CHANGE
(defparameter +skill-kick+ 504)       ; Reserved Skill[] DO NOT CHANGE
(defparameter +skill-pick-lock+ 505)  ; Reserved Skill[] DO NOT CHANGE
(defparameter +skill-punch+ 506)      ; Reserved Skill[] DO NOT CHANGE
(defparameter +skill-rescue+ 507)     ; Reserved Skill[] DO NOT CHANGE
(defparameter +skill-sneak+ 508)      ; Reserved Skill[] DO NOT CHANGE
(defparameter +skill-steal+ 509)      ; Reserved Skill[] DO NOT CHANGE
(defparameter +skill-track+ 510)      ; Reserved Skill[] DO NOT CHANGE
(defparameter +skill-piledrive+ 511)
(defparameter +skill-sleeper+ 512)
(defparameter +skill-elbow+ 513)
(defparameter +skill-knee+ 514)
(defparameter +skill-berserk+ 515)
(defparameter +skill-stomp+ 516)
(defparameter +skill-bodyslam+ 517)
(defparameter +skill-choke+ 518)
(defparameter +skill-clothesline+ 519)
(defparameter +skill-tag+ 520)
(defparameter +skill-intimidate+ 521)
(defparameter +skill-speed-healing+ 522)
(defparameter +skill-stalk+ 523)
(defparameter +skill-hearing+ 524)
(defparameter +skill-bearhug+ 525)
(defparameter +skill-bite+ 526)
(defparameter +skill-dbl-attack+ 527)
(defparameter +skill-bandage+ 528)
(defparameter +skill-firstaid+ 529)
(defparameter +skill-medic+ 530)
(defparameter +skill-consider+ 531)
(defparameter +skill-glance+ 532)
(defparameter +skill-headbutt+ 533)
(defparameter +skill-gouge+ 534)
(defparameter +skill-stun+ 535)
(defparameter +skill-feign+ 536)
(defparameter +skill-conceal+ 537)
(defparameter +skill-plant+ 538)
(defparameter +skill-hotwire+ 539)
(defparameter +skill-shoot+ 540)
(defparameter +skill-behead+ 541)
(defparameter +skill-disarm+ 542)
(defparameter +skill-spinkick+ 543)
(defparameter +skill-roundhouse+ 544)
(defparameter +skill-sidekick+ 545)
(defparameter +skill-spinfist+ 546)
(defparameter +skill-jab+ 547)
(defparameter +skill-hook+ 548)
(defparameter +skill-sweepkick+ 549)
(defparameter +skill-trip+ 550)
(defparameter +skill-uppercut+ 551)
(defparameter +skill-groinkick+ 552)
(defparameter +skill-claw+ 553)
(defparameter +skill-rabbitpunch+ 554)
(defparameter +skill-impale+ 555)
(defparameter +skill-pele-kick+ 556)
(defparameter +skill-lunge-punch+ 557)
(defparameter +skill-tornado-kick+ 558)
(defparameter +skill-circle+ 559)
(defparameter +skill-triple-attack+ 560)

;;; ******************  MONK SKILLS  ******************
(defparameter +skill-palm-strike+ 561)
(defparameter +skill-throat-strike+ 562)
(defparameter +skill-whirlwind+ 563)
(defparameter +skill-hip-toss+ 564)
(defparameter +skill-combo+ 565)
(defparameter +skill-death-touch+ 566)
(defparameter +skill-crane-kick+ 567)
(defparameter +skill-ridgehand+ 568)
(defparameter +skill-scissor-kick+ 569)
(defparameter +skill-pinch-alpha+ 570)
(defparameter +skill-pinch-beta+ 571)
(defparameter +skill-pinch-gamma+ 572)
(defparameter +skill-pinch-delta+ 573)
(defparameter +skill-pinch-epsilon+ 574)
(defparameter +skill-pinch-omega+ 575)
(defparameter +skill-pinch-zeta+ 576)
(defparameter +skill-meditate+ 577)
(defparameter +skill-kata+ 578)
(defparameter +skill-evasion+ 579)

(defparameter +skill-second-weapon+ 580)
(defparameter +skill-scanning+ 581)
(defparameter +skill-cure-disease+ 582)
(defparameter +skill-battle-cry+ 583)
(defparameter +skill-autopsy+ 584)
(defparameter +skill-transmute+ 585)    ; physic transmute objs
(defparameter +skill-metalworking+ 586)
(defparameter +skill-leatherworking+ 587)
(defparameter +skill-demolitions+ 588)
(defparameter +skill-psiblast+ 589)
(defparameter +skill-psilocate+ 590)
(defparameter +skill-psidrain+ 591)     ; drain mana from vict
(defparameter +skill-gunsmithing+ 592)  ; repair gunz
(defparameter +skill-elusion+ 593)      ; !track
(defparameter +skill-pistolwhip+ 594)
(defparameter +skill-crossface+ 595)    ; rifle whip
(defparameter +skill-wrench+ 596)       ; break neck
(defparameter +skill-cry-from-beyond+ 597)
(defparameter +skill-kia+ 598)
(defparameter +skill-wormhole+ 599)     ; physic's wormhole
(defparameter +skill-lecture+ 600)      ; physic's boring-ass lecture

(defparameter +skill-turn+ 601)         ; Cleric's turn
(defparameter +skill-analyze+ 602)      ; Physic's analysis
(defparameter +skill-evaluate+ 603)     ; Physic's evaluation
(defparameter +skill-holy-touch+ 604)   ; Knight's skill
(defparameter +skill-night-vision+ 605)
(defparameter +skill-empower+ 606)
(defparameter +skill-swimming+ 607)
(defparameter +skill-throwing+ 608)
(defparameter +skill-riding+ 609)
(defparameter +skill-pipemaking+ 610)   ;Make a pipe!
(defparameter +skill-charge+ 611)       ; BANG!
(defparameter +skill-counter-attack+ 612)


;;; ****************  CYBORG SKILLS  *******************
(defparameter +skill-reconfigure+ 613)  ; Re-allocate stats
(defparameter +skill-reboot+ 614)       ; Start over from scratch
(defparameter +skill-motion-sensor+ 615) ; Detect Entries into room
(defparameter +skill-stasis+ 616)        ; State of rapid healing
(defparameter +skill-energy-field+ 617)  ; Protective field
(defparameter +skill-reflex-boost+ 618)  ; Speeds up processes
(defparameter +skill-power-boost+ 619)   ; Increases Strength
(defparameter +skill-unused-1+ 620)      ;
(defparameter +skill-fastboot+ 621)      ; Reboots are faster
(defparameter +skill-self-destruct+ 622) ; Effective self destructs
(defparameter +skill-unused-2+ 623)      ;
(defparameter +skill-bioscan+ 624)       ; Sense Life scan
(defparameter +skill-discharge+ 625)     ; Discharge attack
(defparameter +skill-selfrepair+ 626)    ; Repair hit points
(defparameter +skill-cyborepair+ 627)    ; Repair other borgs
(defparameter +skill-overhaul+ 628)      ; Overhaul other borgs
(defparameter +skill-damage-control+ 629) ; Damage Control System
(defparameter +skill-electronics+ 630)    ; Operation of Electronics
(defparameter +skill-hacking+ 631)        ; hack electronic systems
(defparameter +skill-cyberscan+ 632)      ; scan others for implants
(defparameter +skill-cybo-surgery+ 633)   ; implant objects
(defparameter +skill-energy-weapons+ 634) ; energy weapon use
(defparameter +skill-proj-weapons+ 635)   ; projectile weapon use
(defparameter +skill-speed-loading+ 636)  ; speed load weapons
(defparameter +skill-hyperscan+ 637)  ; aware of hidden objs and traps
(defparameter +skill-overdrain+ 638)    ; overdrain batteries
(defparameter +skill-de-energize+ 639)  ; drain energy from chars
(defparameter +skill-assimilate+ 640)   ; assimilate objects
(defparameter +skill-radionegation+ 641) ; immunity to radiation
(defparameter +skill-implant-w+ 642) ; Extra attacks with implant weapons.
(defparameter +skill-adv-implant-w+ 643)    ; ""
(defparameter +skill-offensive-pos+ 644)    ; Offensive Posturing
(defparameter +skill-defensive-pos+ 645)    ; Defensive Posturing
(defparameter +skill-melee-combat-tac+ 646) ; Melee combat tactics
(defparameter +skill-neural-bridging+ 647)  ; Cogenic Neural Bridging
                                        ; (Ambidextarity)
;;; Cyborg skills continue around 675

(defparameter +skill-retreat+ 648)      ; controlled flee
(defparameter +skill-disguise+ 649)     ; look like a mob
(defparameter +skill-ambush+ 650)       ; surprise victim

(defparameter +skill-chemistry+ 651)    ; merc skill
(defparameter +skill-advanced-cybo-surgery+ 652)
(defparameter +skill-spare3+ 653)
(defparameter +skill-spare4+ 654)
(defparameter +skill-beguile+ 655)
(defparameter +skill-spare6+ 656)
(defparameter +skill-spare7+ 657)
(defparameter +skill-spare8+ 658)

;;; -------------  Hood Skillz... yeah. Just two
(defparameter +skill-snatch+ 667)
(defparameter +skill-drag+ 668)

;;; static const int SKILL_TAUNT = 669;

;;; ------------  Mercenary Skills -------------------
(defparameter +skill-hamstring+ 666)
(defparameter +skill-snipe+ 669)        ; sniper skill for mercs
(defparameter +skill-infiltrate+ 670) ; merc skill, improvement on sneak
(defparameter +skill-shoulder-throw+ 671) ; grounding skill between hiptoss
                                        ; and sweepkick

;;; Bard Skills
(defparameter +skill-scream+ 672) ; damage like psiblast, chance to stun
(defparameter +skill-ventriloquism+ 673) ; makes objects talk
(defparameter +skill-tumbling+ 674)      ; like uncanny dodge
(defparameter +skill-lingering-song+ 676) ; increases duration of song affects

;;; Overflow Cyborg
(defparameter +skill-nanite-reconstruction+ 675) ; repairs implants
;;; static const int SKILL_ARTERIAL_FLOW = 676; ; Arterial Flow Enhancements
(defparameter +skill-optimmunal-resp+ 677) ; Genetek Optimmunal Node
(defparameter +skill-adrenal-maximizer+ 678) ; Shukutei Adrenal Maximizer

(defparameter +skill-energy-conversion+ 679) ; physic's energy conversion

                                        ;*****************  PROFICENCIES  ******************
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter +skill-prof-pound+ 681)
  (defparameter +skill-prof-whip+ 682)
  (defparameter +skill-prof-pierce+ 683)
  (defparameter +skill-prof-slash+ 684)
  (defparameter +skill-prof-crush+ 685)
  (defparameter +skill-prof-blast+ 686)
  (defparameter +skill-break-door+ 687)
  (defparameter +skill-archery+ 688)
  (defparameter +skill-bow-fletch+ 689)
  (defparameter +skill-read-scrolls+ 690)
  (defparameter +skill-use-wands+ 691))


;;; New skills may be added here up to MAX_SKILLS (700)
(defparameter +skill-discipline-of-steel+ 692)
(defparameter +skill-strike+ 693)
(defparameter +skill-cleave+ 694)
(defparameter +skill-great-cleave+ 695)
(defparameter +skill-appraise+ 696)
(defparameter +skill-garotte+ 697)
(defparameter +skill-shield-mastery+ 698)
(defparameter +skill-uncanny-dodge+ 699)

;;; *  NON-PLAYER AND OBJECT SPELLS AND SKILLS
;;  The practice levels for the spells and skills below are _not_ recorded
;;  in the playerfile; therefore, the intended use is for spells and skills
;;  associated with objects (such as SPELL_IDENTIFY used with scrolls of
;;  identify) or non-players (such as NPC-only spells).


(defparameter +spell-fire-breath+ 702)
(defparameter +spell-gas-breath+ 703)
(defparameter +spell-frost-breath+ 704)
(defparameter +spell-acid-breath+ 705)
(defparameter +spell-lightning-breath+ 706)
(defparameter +type-lava-breath+ 707)
(defparameter +spell-earth-elemental+ 711)
(defparameter +spell-fire-elemental+ 712)
(defparameter +spell-water-elemental+ 713)
(defparameter +spell-air-elemental+ 714)
(defparameter +spell-hell-fire+ 717)
(defparameter +javelin-of-lightning+ 718)
(defparameter +spell-trog-stench+ 719)
(defparameter +spell-manticore-spikes+ 720)
(defparameter +type-catapult+ 730)
(defparameter +type-balista+ 731)
(defparameter +type-boiling-oil+ 732)
(defparameter +type-hotsands+ 733)
(defparameter +spell-mana-restore+ 734)
(defparameter +spell-blade-barrier+ 735)
(defparameter +spell-summon-minotaur+ 736)
(defparameter +type-stygian-lightning+ 737)
(defparameter +spell-skunk-stench+ 738)
(defparameter +spell-petrify+ 739)
(defparameter +spell-sickness+ 740)
(defparameter +spell-essence-of-evil+ 741)
(defparameter +spell-essence-of-good+ 742)
(defparameter +spell-shrinking+ 743)
(defparameter +spell-hell-frost+ 744)
(defparameter +type-alien-blood+ 745)
(defparameter +type-surgery+ 746)
(defparameter +smoke-effects+ 747)
(defparameter +spell-hell-fire-storm+ 748)
(defparameter +spell-hell-frost-storm+ 749)
(defparameter +spell-shadow-breath+ 750)
(defparameter +spell-steam-breath+ 751)
(defparameter +type-trampling+ 752)
(defparameter +type-gore-horns+ 753)
(defparameter +type-tail-lash+ 754)
(defparameter +type-boiling-pitch+ 755)
(defparameter +type-falling+ 756)
(defparameter +type-holyocean+ 757)
(defparameter +type-freezing+ 758)
(defparameter +type-drowning+ 759)
(defparameter +type-ablaze+ 760)
(defparameter +spell-quad-damage+ 761)
(defparameter +type-vader-choke+ 762)
(defparameter +type-acid-burn+ 763)
(defparameter +spell-zhengis-fist-of-annihilation+ 764)
(defparameter +type-rad-sickness+ 765)
(defparameter +type-flamethrower+ 766)
(defparameter +type-malovent-holytouch+ 767) ; When holytouch wears off.
(defparameter +spell-youth+ 768)
(defparameter +type-swallow+ 769)
(defparameter +top-npc-spell+ 770)

(defparameter +top-spell-define+ 799)
;;; NEW NPC/OBJECT SPELLS can be inserted here up to 799


;;; WEAPON ATTACK TYPES

(defparameter +type-hit+ 800)
(defparameter +type-sting+ 801)
(defparameter +type-whip+ 802)
(defparameter +type-slash+ 803)
(defparameter +type-bite+ 804)
(defparameter +type-bludgeon+ 805)
(defparameter +type-crush+ 806)
(defparameter +type-pound+ 807)
(defparameter +type-claw+ 808)
(defparameter +type-maul+ 809)
(defparameter +type-thrash+ 810)
(defparameter +type-pierce+ 811)
(defparameter +type-blast+ 812)
(defparameter +type-punch+ 813)
(defparameter +type-stab+ 814)
(defparameter +type-energy-gun+ 815)
(defparameter +type-rip+ 816)
(defparameter +type-chop+ 817)
(defparameter +type-shoot+ 818)


(defparameter +top-attacktype+ 819)
;;; new attack types can be added here - up to TYPE_SUFFERING

;;;energy weapon attack types
(defparameter +type-egun-laser+ 820)
(defparameter +type-egun-plasma+ 821)
(defparameter +type-egun-ion+ 822)
(defparameter +type-egun-photon+ 823)
(defparameter +type-egun-sonic+ 824)
(defparameter +type-egun-particle+ 825)
(defparameter +type-egun-gamma+ 826)
(defparameter +type-egun-lightning+ 827)
(defparameter +type-egun-top+ 828)

;;;energy weapon spec types
(defparameter +type-egun-spec-lightning+ 831)
(defparameter +type-egun-spec-sonic+ 832)


(defparameter +type-crushing-depth+ 892) ; in deep ocean without vehicle
(defparameter +type-taint-burn+ 893)     ; casting while tainted
(defparameter +type-pressure+ 894)
(defparameter +type-suffocating+ 895)
(defparameter +type-anguish+ 896) ; Soulless and good aligned. dumbass.
(defparameter +type-bleed+ 897)         ; Open wound
(defparameter +type-overload+ 898)      ; cyborg overloading systems.
(defparameter +type-suffering+ 899)

(defparameter +saving-para+ 0)
(defparameter +saving-rod+ 1)
(defparameter +saving-petri+ 2)
(defparameter +saving-breath+ 3)
(defparameter +saving-spell+ 4)
(defparameter +saving-chem+ 5)
(defparameter +saving-psi+ 6)
(defparameter +saving-phy+ 7)
(defparameter +saving-none+ 50)

(defparameter +tar-ignore+ 1)
(defparameter +tar-char-room+ 2)
(defparameter +tar-char-world+ 4)
(defparameter +tar-fight-self+ 8)
(defparameter +tar-fight-vict+ 16)
(defparameter +tar-self-only+ 32) ; Only a check, use with i.e. TAR_CHAR_ROOM
(defparameter +tar-not-self+ 64) ; Only a check, use with i.e. TAR_CHAR_ROOM
(defparameter +tar-obj-inv+ 128)
(defparameter +tar-obj-room+ 256)
(defparameter +tar-obj-world+ 512)
(defparameter +tar-obj-equip+ 1024)
(defparameter +tar-door+ 2048)
(defparameter +tar-unpleasant+ 4096)
(defparameter +tar-dir+ 8192)

(defparameter +mag-damage+ (ash 1 0))
(defparameter +mag-affects+ (ash 1 1))
(defparameter +mag-unaffects+ (ash 1 2))
(defparameter +mag-points+ (ash 1 3))
(defparameter +mag-alter-objs+ (ash 1 4))
(defparameter +mag-groups+ (ash 1 5))
(defparameter +mag-masses+ (ash 1 6))
(defparameter +mag-areas+ (ash 1 7))
(defparameter +mag-summons+ (ash 1 8))
(defparameter +mag-creations+ (ash 1 9))
(defparameter +mag-manual+ (ash 1 10))
(defparameter +mag-objects+ (ash 1 11))
(defparameter +mag-touch+ (ash 1 12))
(defparameter +mag-magic+ (ash 1 13))
(defparameter +mag-divine+ (ash 1 14))
(defparameter +mag-physics+ (ash 1 15))
(defparameter +mag-psionic+ (ash 1 16))
(defparameter +mag-biologic+ (ash 1 17))
(defparameter +cyb-activate+ (ash 1 18))
(defparameter +mag-evil+ (ash 1 19))
(defparameter +mag-good+ (ash 1 20))
(defparameter +mag-exits+ (ash 1 21))
(defparameter +mag-outdoors+ (ash 1 22))
(defparameter +mag-nowater+ (ash 1 23))
(defparameter +mag-waterzap+ (ash 1 24))
(defparameter +mag-nosun+ (ash 1 25))
(defparameter +mag-zen+ (ash 1 26))
(defparameter +mag-mercenary+ (ash 1 27))
(defparameter +mag-bard+ (ash 1 28))

(defparameter +soil-water+ (ash 1 0))
(defparameter +soil-blood+ (ash 1 1))
(defparameter +soil-mud+ (ash 1 2))
(defparameter +soil-shit+ (ash 1 3))
(defparameter +soil-urine+ (ash 1 4))
(defparameter +soil-mucus+ (ash 1 5))
(defparameter +soil-saliva+ (ash 1 6))
(defparameter +soil-acid+ (ash 1 7))
(defparameter +soil-oil+ (ash 1 8))
(defparameter +soil-sweat+ (ash 1 9))
(defparameter +soil-grease+ (ash 1 10))
(defparameter +soil-soot+ (ash 1 11))
(defparameter +soil-slime+ (ash 1 12))
(defparameter +soil-sticky+ (ash 1 13))
(defparameter +soil-vomit+ (ash 1 14))
(defparameter +soil-rust+ (ash 1 15))
(defparameter +soil-char+ (ash 1 16))
(defparameter +top-soil+ 17)

;;; PC char_classes
(defparameter +class-help+ -2)
(defparameter +class-undefined+ -1)
(defparameter +class-none+ -1)
(defparameter +class-magic-user+ 0)
(defparameter +class-mage+ +class-magic-user+)
(defparameter +class-cleric+ 1)
(defparameter +class-thief+ 2)
(defparameter +class-warrior+ 3)
(defparameter +class-barb+ 4)
(defparameter +class-psionic+ 5)    ; F
(defparameter +class-physic+ 6) ; F
(defparameter +class-cyborg+ 7) ; F
(defparameter +class-knight+ 8)
(defparameter +class-ranger+ 9)
(defparameter +class-bard+ 10)  ; N
(defparameter +class-monk+ 11)
(defparameter +class-vampire+ 12)
(defparameter +class-mercenary+ 13)
(defparameter +class-spare1+ 14)
(defparameter +class-spare2+ 15)
(defparameter +class-spare3+ 16)

(defparameter +num-classes+ 17) ; This must be the number of char_classes!!
(defparameter +class-normal+ 50)
(defparameter +class-bird+ 51)
(defparameter +class-predator+ 52)
(defparameter +class-snake+ 53)
(defparameter +class-horse+ 54)
(defparameter +class-small+ 55)
(defparameter +class-medium+ 56)
(defparameter +class-large+ 57)
(defparameter +class-scientist+ 58)
(defparameter +class-skeleton+ 60)
(defparameter +class-ghoul+ 61)
(defparameter +class-shadow+ 62)
(defparameter +class-wight+ 63)
(defparameter +class-wraith+ 64)
(defparameter +class-mummy+ 65)
(defparameter +class-spectre+ 66)
(defparameter +class-npc-vampire+ 67)
(defparameter +class-ghost+ 68)
(defparameter +class-lich+ 69)
(defparameter +class-zombie+ 70)

(defparameter +class-earth+ 81) ; Elementals
(defparameter +class-fire+ 82)
(defparameter +class-water+ 83)
(defparameter +class-air+ 84)
(defparameter +class-lightning+ 85)
(defparameter +class-green+ 91) ; Dragons
(defparameter +class-white+ 92)
(defparameter +class-black+ 93)
(defparameter +class-blue+ 94)
(defparameter +class-red+ 95)
(defparameter +class-silver+ 96)
(defparameter +class-shadow-d+ 97)
(defparameter +class-deep+ 98)
(defparameter +class-turtle+ 99)
(defparameter +class-least+ 101)    ; Devils
(defparameter +class-lesser+ 102)
(defparameter +class-greater+ 103)
(defparameter +class-duke+ 104)
(defparameter +class-arch+ 105)
(defparameter +class-hill+ 111)
(defparameter +class-stone+ 112)
(defparameter +class-frost+ 113)
(defparameter +class-fire-g+ 114)
(defparameter +class-cloud+ 115)
(defparameter +class-storm+ 116)
(defparameter +class-slaad-red+ 120)    ; Slaad
(defparameter +class-slaad-blue+ 121)
(defparameter +class-slaad-green+ 122)
(defparameter +class-slaad-grey+ 123)
(defparameter +class-slaad-death+ 124)
(defparameter +class-slaad-lord+ 125)
(defparameter +class-demon-i+ 130)  ; Demons of the Abyss
(defparameter +class-demon-ii+ 131)
(defparameter +class-demon-iii+ 132)
(defparameter +class-demon-iv+ 133)
(defparameter +class-demon-v+ 134)
(defparameter +class-demon-vi+ 135)
(defparameter +class-demon-semi+ 136)
(defparameter +class-demon-minor+ 137)
(defparameter +class-demon-major+ 138)
(defparameter +class-demon-lord+ 139)
(defparameter +class-demon-prince+ 140)
(defparameter +class-deva-astral+ 150)
(defparameter +class-deva-monadic+ 151)
(defparameter +class-deva-movanic+ 152)
(defparameter +class-mephit-fire+ 160)
(defparameter +class-mephit-lava+ 161)
(defparameter +class-mephit-smoke+ 162)
(defparameter +class-mephit-steam+ 163)
(defparameter +class-daemon-arcana+ 170)
(defparameter +class-daemon-charona+ 171)
(defparameter +class-daemon-dergho+ 172)
(defparameter +class-daemon-hydro+ 173)
(defparameter +class-daemon-pisco+ 174)
(defparameter +class-daemon-ultro+ 175)
(defparameter +class-daemon-yagno+ 176)
(defparameter +class-daemon-pyro+ 177)
(defparameter +class-godling+ 178)
(defparameter +class-diety+ 179)

(defparameter +top-class+ 180)

(defparameter +borg-power+ 0)
(defparameter +borg-speed+ 0)
(defparameter +borg-mentant+ 0)

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
(defparameter +num-races+ 62)
(defparameter +num-pc-races+ 9)

(defparameter +sexes+ #("neuter" "male" "female"))
(defparameter +sex-neuter+ 0)
(defparameter +sex-male+ 1)
(defparameter +sex-female+ 2)

;;; Positions
(defconstant +bottom-pos+ 0)
(defconstant +pos-dead+ 0) ; dead
(defconstant +pos-mortallyw+ 1)    ; mortally wounded
(defconstant +pos-incap+ 2)    ; incapacitated
(defconstant +pos-stunned+ 3)  ; stunned
(defconstant +pos-sleeping+ 4) ; sleeping
(defconstant +pos-resting+ 5)  ; resting
(defconstant +pos-sitting+ 6)  ; sitting
(defconstant +pos-fighting+ 7) ; fighting
(defconstant +pos-standing+ 8) ; standing
(defconstant +pos-flying+ 9)   ; flying around
(defconstant +pos-mounted+ 10)
(defconstant +pos-swimming+ 11)
(defconstant +top-pos+ 11)

(defparameter +position-types+
  #("dead" "mortally wounded" "incapacitated" "stunned" "sleeping" "resting" "sitting" "fighting" "standing" "flying" "mounted" "swimming"))

;;; Player flags: used by Creature.char_specials.act
(defparameter +plr-hardcore+ (ash 1 0))   ; Player is a hardcore character
(defparameter +plr-unused1+ (ash 1 1))
(defparameter +plr-frozen+ (ash 1 2))   ; Player is frozen
(defparameter +plr-dontset+ (ash 1 3))  ; Don't EVER set (ISNPC bit)
(defparameter +plr-writing+ (ash 1 4))  ; Player writing (board/mail/olc)
(defparameter +plr-mailing+ (ash 1 5))  ; Player is writing mail
(defparameter +plr-crash+ (ash 1 6))    ; Player needs to be crash-saved
(defparameter +plr-siteok+ (ash 1 7))   ; Player has been site-cleared
(defparameter +plr-noshout+ (ash 1 8))  ; Player not allowed to shout/goss
(defparameter +plr-notitle+ (ash 1 9))  ; Player not allowed to set title
(defparameter +plr-unused2+ (ash 1 10))
(defparameter +plr-unused3+ (ash 1 11))
(defparameter +plr-noclanmail+ (ash 1 12))  ; Player doesn't get clanmail
(defparameter +plr-nodelete+ (ash 1 13))    ; Player shouldn't be deleted
(defparameter +plr-invstart+ (ash 1 14))    ; Player should enter game wizinvis
(defparameter +plr-cryo+ (ash 1 15))    ; Player is cryo-saved (purge prog)
(defparameter +plr-afk+ (ash 1 16)) ; Player is away from keyboard
(defparameter +plr-clan-leader+ (ash 1 17)) ; The head of the respective clan
(defparameter +plr-unused4+ (ash 1 18))
(defparameter +plr-olc+ (ash 1 19)) ; Player is descripting olc
(defparameter +plr-halt+ (ash 1 20))    ; Player is halted
(defparameter +plr-olcgod+ (ash 1 21))  ; Player can edit at will
(defparameter +plr-tester+ (ash 1 22))  ; Player is a tester
(defparameter +plr-unused5+ (ash 1 23)) ; Quest god
(defparameter +plr-mortalized+ (ash 1 24))  ; God can be killed
(defparameter +plr-unused6+ (ash 1 25))
(defparameter +plr-unused7+ (ash 1 26))
(defparameter +plr-nopost+ (ash 1 27))
(defparameter +plr-log+ (ash 1 28)) ; log all cmds
(defparameter +plr-unused8+ (ash 1 29)) ; player approved for port olc
(defparameter +plr-nopk+ (ash 1 30))    ; player cannot pk

;; Player Flags Mark II
(defparameter +plr2-soulless+ (ash 1 0))    ; Signing the Unholy Compact.
(defparameter +plr2-buried+ (ash 1 1))  ; Player has died way too many times.
(defparameter +plr2-in-combat+ (ash 1 2))

(defparameter +mob-spec+ (ash 1 0))     ; Mob has a callable spec-proc
(defparameter +mob-sentinel+ (ash 1 1)) ; Mob should not move
(defparameter +mob-scavenger+ (ash 1 2)) ; Mob picks up stuff on the ground
(defparameter +mob-isnpc+ (ash 1 3)) ; (R) Automatically set on all Mobs
(defparameter +mob-aware+ (ash 1 4)) ; Mob can't be backstabbed
(defparameter +mob-aggressive+ (ash 1 5)) ; Mob hits players in the room
(defparameter +mob-stay-zone+ (ash 1 6)) ; Mob shouldn't wander out of zone
(defparameter +mob-wimpy+ (ash 1 7))   ; Mob flees if severely injured
(defparameter +mob-aggr-evil+ (ash 1 8))     ; auto attack evil PC's
(defparameter +mob-aggr-good+ (ash 1 9))     ; auto attack good PC's
(defparameter +mob-aggr-neutral+ (ash 1 10)) ; auto attack neutral PC's
(defparameter +mob-memory+ (ash 1 11)) ; remember attackers if attacked
(defparameter +mob-helper+ (ash 1 12)) ; attack PCs fighting other NPCs
(defparameter +mob-nocharm+ (ash 1 13))  ; Mob can't be charmed
(defparameter +mob-nosummon+ (ash 1 14)) ; Mob can't be summoned
(defparameter +mob-nosleep+ (ash 1 15))  ; Mob can't be slept
(defparameter +mob-nobash+ (ash 1 16)) ; Mob can't be bashed (e.g. trees)
(defparameter +mob-noblind+ (ash 1 17)) ; Mob can't be blinded
(defparameter +mob-noturn+ (ash 1 18))  ; Hard to turn
(defparameter +mob-nopetri+ (ash 1 19)) ; Cannot be petrified
(defparameter +mob-pet+ (ash 1 20)) ; Mob is a conjured pet and shouldn't
                                        ; get nor give any xp in any way.
(defparameter +mob-soulless+ (ash 1 21))    ; Mobile is Soulless - Unholy compact.
(defparameter +mob-spirit-tracker+ (ash 1 22))  ; Can track through !track
(defparameter +mob-utility+ (ash 1 23)) ; Can't be seen, hit, etc...

(defparameter +mob2-script+ (ash 1 0))
(defparameter +mob2-mount+ (ash 1 1))
(defparameter +mob2-stay-sect+ (ash 1 2))   ; Can't leave SECT-type.
(defparameter +mob2-atk-mobs+ (ash 1 3))    ; Aggro Mobs will attack other mobs
(defparameter +mob2-hunt+ (ash 1 4))    ; Mob will hunt attacker
(defparameter +mob2-looter+ (ash 1 5))  ; Loots corpses
(defparameter +mob2-nostun+ (ash 1 6))
(defparameter +mob2-seller+ (ash 1 7))  ; If shopkeeper, sells anywhere.
(defparameter +mob2-wont-wear+ (ash 1 8))   ; Wont wear shit it picks up (SHPKPER)
(defparameter +mob2-silent-hunter+ (ash 1 9))
(defparameter +mob2-familiar+ (ash 1 10))   ; mages familiar
(defparameter +mob2-no-flow+ (ash 1 11))    ; Mob doesn't flow
(defparameter +mob2-unapproved+ (ash 1 12)) ; Mobile not approved for game play
(defparameter +mob2-renamed+ (ash 1 13))    ; Mobile renamed
(defparameter +mob2-noaggro-race+ (ash 1 14))   ; wont attack members of own race

(defparameter +pref-brief+ 0) ; Room descs won't normally be shown
(defparameter +pref-nohaggle+ 1) ;
(defparameter +pref-deaf+ 2)     ; Can't hear shouts
(defparameter +pref-notell+ 3)   ; Can't receive tells
(defparameter +pref-disphp+ 4)  ; Display hit points in prompt
(defparameter +pref-dispmana+ 5) ; Display mana points in prompt
(defparameter +pref-dispmove+ 6) ; Display move points in prompt
(defparameter +pref-autoexit+ 7) ; Display exits in a room
(defparameter +pref-nohassle+ 8) ; Aggr mobs won't attack
(defparameter +pref-nasty+ 9) ; Can hear nasty words on channel
(defparameter +pref-summonable+ 10) ; Can be summoned
(defparameter +pref-noplug+ 11) ; Can't hear crier plug tempus
(defparameter +pref-holylight+ 12)  ; Can see in dark
(defparameter +pref-color-1+ 13)    ; Color (low bit)
(defparameter +pref-color-2+ 14)    ; Color (high bit)
(defparameter +pref-nowiz+ 15)      ; Can't hear wizline
(defparameter +pref-log1+ 16)   ; On-line System Log (low bit)
(defparameter +pref-log2+ 17)  ; On-line System Log (high bit)
(defparameter +pref-noauct+ 18) ; Can't hear auction channel
(defparameter +pref-nogoss+ 19) ; Can't hear gossip channel
(defparameter +pref-nogratz+ 20)    ; Can't hear grats channel
(defparameter +pref-roomflags+ 21) ; Can see room flags (ROOM-x)
(defparameter +pref-nosnoop+ 22) ; Can not be snooped by immortals
(defparameter +pref-nomusic+ 23) ; Can't hear music channel
(defparameter +pref-nospew+ 24)  ; Can't hear spews
(defparameter +pref-gagmiss+ 25) ; Doesn't see misses during fight
(defparameter +pref-noproject+ 26) ; Cannot hear the remort channel
(defparameter +pref-nopetition+ 27) ;
(defparameter +pref-noclansay+ 28) ; Doesnt hear clan says and such
(defparameter +pref-noidentify+ 29) ; Saving throw is made when id'd
(defparameter +pref-nodream+ 30)

;; PREF 2 Flags

(defparameter +pref-debug+ 31)          ; Sees info on fight.
(defparameter +pref-newbie-helper+ 32)  ; sees newbie arrivals
(defparameter +pref-auto-diagnose+ 33) ; automatically see condition of enemy
(defparameter +pref-autopage+ 34) ; Beeps when ch receives a tell
(defparameter +pref-noaffects+ 35) ; Affects are not shown in score
(defparameter +pref-noholler+ 36)  ; Gods only
(defparameter +pref-noimmchat+ 37) ; Gods only
(defparameter +pref-unused-1+ 38) ; auto-sets title to clan stuff
(defparameter +pref-clan-hide+ 39) ; don't show badge in who list
(defparameter +pref-unused-2+ 40) ; interrupts while d->showstr-point
(defparameter +pref-autoprompt+ 41) ; always draw new prompt
(defparameter +pref-nowho+ 42)      ; don't show in who
(defparameter +pref-anonymous+ 43) ; don't show char-class, level
(defparameter +pref-notrailers+ 44) ; don't show trailer affects
(defparameter +pref-vt100+ 45)  ; Players uses VT100 inferface
(defparameter +pref-autosplit+ 46) ;
(defparameter +pref-autoloot+ 47)  ;
(defparameter +pref-pkiller+ 48) ; player can attack other players
(defparameter +pref-nogecho+ 49) ; Silly Gecho things
(defparameter +pref-nowrap+ 50) ; turns off autowrap temporarily.
(defparameter +pref-dispalign+ 51)  ;
(defparameter +pref-worldwrite+ 52) ; allows worldwrite to work
(defparameter +pref-noguildsay+ 53) ;
(defparameter +pref-disptime+ 54) ; show localtime in the prompt
(defparameter +pref-disp-vnums+ 55) ; show vnums after items ldesc
(defparameter +pref-count+ 56)

;;; Affect bits: used in Creature.char_specials.saved.affected_by
;;; WARNING: In the world files, NEVER set the bits marked "R" ("Reserved")
(defparameter +aff-blind+ (ash 1 0))    ; (R) Char is blind
(defparameter +aff-invisible+ (ash 1 1))    ; Char is invisible
(defparameter +aff-detect-align+ (ash 1 2)) ; Char is sensitive to align
(defparameter +aff-detect-invis+ (ash 1 3)) ; Char can see invis chars
(defparameter +aff-detect-magic+ (ash 1 4)) ; Char is sensitive to magic
(defparameter +aff-sense-life+ (ash 1 5))   ; Char can sense hidden life
(defparameter +aff-waterwalk+ (ash 1 6))    ; Char can walk on water
(defparameter +aff-sanctuary+ (ash 1 7))    ; Char protected by sanct.
(defparameter +aff-group+ (ash 1 8))    ; (R) Char is grouped
(defparameter +aff-curse+ (ash 1 9))    ; Char is cursed
(defparameter +aff-infravision+ (ash 1 10)) ; Char can see in dark
(defparameter +aff-poison+ (ash 1 11))  ; (R) Char is poisoned
(defparameter +aff-protect-evil+ (ash 1 12))    ; Char protected from evil
(defparameter +aff-protect-good+ (ash 1 13))    ; Char protected from good
(defparameter +aff-sleep+ (ash 1 14))   ; (R) Char magically asleep
(defparameter +aff-notrack+ (ash 1 15)) ; Char can't be tracked
(defparameter +aff-inflight+ (ash 1 16))    ; Room for future expansion
(defparameter +aff-time-warp+ (ash 1 17))   ; Room for future expansion
(defparameter +aff-sneak+ (ash 1 18))   ; Char can move quietly
(defparameter +aff-hide+ (ash 1 19))    ; Char is hidden
(defparameter +aff-waterbreath+ (ash 1 20)) ; Room for future expansion
(defparameter +aff-charm+ (ash 1 21))   ; Char is charmed
(defparameter +aff-confusion+ (ash 1 22))   ; Char is confused
(defparameter +aff-nopain+ (ash 1 23))  ; Char feels no pain
(defparameter +aff-retina+ (ash 1 24))  ; Char's retina is stimulated
(defparameter +aff-adrenaline+ (ash 1 25))  ; Char's adrenaline is pumping
(defparameter +aff-confidence+ (ash 1 26))  ; Char is confident
(defparameter +aff-rejuv+ (ash 1 27))   ; Char is rejuvenating
(defparameter +aff-regen+ (ash 1 28))   ; Body is regenerating
(defparameter +aff-glowlight+ (ash 1 29))   ; Light spell is operating
(defparameter +aff-blur+ (ash 1 30))    ; Blurry image
(defparameter +num-aff-flags+ 31)

(defparameter +aff2-fluorescent+ (ash 1 0))
(defparameter +aff2-transparent+ (ash 1 1))
(defparameter +aff2-slow+ (ash 1 2))
(defparameter +aff2-haste+ (ash 1 3))
(defparameter +aff2-mounted+ (ash 1 4)) ;DO NOT SET THIS IN MOB FILE
(defparameter +aff2-fire-shield+ (ash 1 5)) ; affected by Fire Shield
(defparameter +aff2-berserk+ (ash 1 6))
(defparameter +aff2-intimidated+ (ash 1 7))
(defparameter +aff2-true-seeing+ (ash 1 8))
(defparameter +aff2-divine-illumination+ (ash 1 9))
(defparameter +aff2-protect-undead+ (ash 1 10))
(defparameter +aff2-invis-to-undead+ (ash 1 11))
(defparameter +aff2-animal-kin+ (ash 1 12))
(defparameter +aff2-endure-cold+ (ash 1 13))
(defparameter +aff2-paralyzed+ (ash 1 14))
(defparameter +aff2-prot-lightning+ (ash 1 15))
(defparameter +aff2-prot-fire+ (ash 1 16))
(defparameter +aff2-telekinesis+ (ash 1 17))    ; Char can carry more stuff
(defparameter +aff2-prot-rad+ (ash 1 18))   ; Enables Autoexits ! :)
(defparameter +aff2-ablaze+ (ash 1 19))
(defparameter +aff2-neck-protected+ (ash 1 20)) ; Can't be beheaded
(defparameter +aff2-displacement+ (ash 1 21))
(defparameter +aff2-prot-devils+ (ash 1 22))
(defparameter +aff2-meditate+ (ash 1 23))
(defparameter +aff2-evade+ (ash 1 24))
(defparameter +aff2-blade-barrier+ (ash 1 25))
(defparameter +aff2-oblivity+ (ash 1 26))
(defparameter +aff2-energy-field+ (ash 1 27))
(defparameter +aff2-petrified+ (ash 1 28))
(defparameter +aff2-vertigo+ (ash 1 29))
(defparameter +aff2-prot-demons+ (ash 1 30))
(defparameter +num-aff2-flags+ 31)

(defparameter +aff3-attraction-field+ (ash 1 0))
(defparameter +aff3-energy-leak+ (ash 1 1))
(defparameter +aff3-poison-2+ (ash 1 2))
(defparameter +aff3-poison-3+ (ash 1 3))
(defparameter +aff3-sickness+ (ash 1 4))
(defparameter +aff3-self-destruct+ (ash 1 5))   ; Self-destruct sequence init
(defparameter +aff3-damage-control+ (ash 1 6))  ; Damage control for cyborgs
(defparameter +aff3-stasis+ (ash 1 7))  ; Borg is in static state
(defparameter +aff3-prismatic-sphere+ (ash 1 8))    ; Defensive
(defparameter +aff3-radioactive+ (ash 1 9))
(defparameter +aff3-detect-poison+ (ash 1 10))
(defparameter +aff3-mana-tap+ (ash 1 11))
(defparameter +aff3-energy-tap+ (ash 1 12))
(defparameter +aff3-sonic-imagery+ (ash 1 13))
(defparameter +aff3-shroud-obscurement+ (ash 1 14))
(defparameter +aff3-nobreathe+ (ash 1 15))
(defparameter +aff3-prot-heat+ (ash 1 16))
(defparameter +aff3-psishield+ (ash 1 17))
(defparameter +aff3-psychic-crush+ (ash 1 18))
(defparameter +aff3-double-damage+ (ash 1 19))
(defparameter +aff3-acidity+ (ash 1 20))
(defparameter +aff3-hamstrung+ (ash 1 21))  ; Bleeding badly from the leg
(defparameter +aff3-gravity-well+ (ash 1 22))   ; Pissed off a phyz and got hit by gravity well
(defparameter +aff3-symbol-of-pain+ (ash 1 23)) ; Char's mind is burning with pain
(defparameter +aff3-emp-shield+ (ash 1 24)) ; EMP SHIELDING
(defparameter +aff3-inst-aff+ (ash 1 25))   ; Affected by an instant affect
(defparameter +aff3-tainted+ (ash 1 27))    ; Knight spell, "taint"
(defparameter +aff3-infiltrate+ (ash 1 28)) ; Merc skill infiltrate
(defparameter +aff3-divine-power+ (ash 1 29))
(defparameter +aff3-mana-leak+ (ash 1 30))
(defparameter +num-aff3-flags+ 31)

(defparameter +array-aff-1+ 1)
(defparameter +array-aff-2+ 2)
(defparameter +array-aff-3+ 3)



;;; Modifier constants used with obj affects ('A' fields)
(defparameter +apply-none+ 0)   ; No effect
(defparameter +apply-str+ 1)    ; Apply to strength
(defparameter +apply-dex+ 2)    ; Apply to dexterity
(defparameter +apply-int+ 3)    ; Apply to intellegence
(defparameter +apply-wis+ 4)    ; Apply to wisdom
(defparameter +apply-con+ 5)    ; Apply to constitution
(defparameter +apply-cha+ 6)    ; Apply to charisma
(defparameter +apply-class+ 7)  ; Reserved
(defparameter +apply-level+ 8)  ; Reserved
(defparameter +apply-age+ 9)    ; Apply to age
(defparameter +apply-char-weight+ 10)   ; Apply to weight
(defparameter +apply-char-height+ 11)   ; Apply to height
(defparameter +apply-mana+ 12)  ; Apply to max mana
(defparameter +apply-hit+ 13)   ; Apply to max hit points
(defparameter +apply-move+ 14)  ; Apply to max move points
(defparameter +apply-gold+ 15)  ; Reserved
(defparameter +apply-exp+ 16)   ; Reserved
(defparameter +apply-ac+ 17)    ; Apply to Armor Class
(defparameter +apply-hitroll+ 18)   ; Apply to hitroll
(defparameter +apply-damroll+ 19)   ; Apply to damage roll
(defparameter +apply-saving-para+ 20)   ; Apply to save throw: paralz
(defparameter +apply-saving-rod+ 21)    ; Apply to save throw: rods
(defparameter +apply-saving-petri+ 22)  ; Apply to save throw: petrif
(defparameter +apply-saving-breath+ 23) ; Apply to save throw: breath
(defparameter +apply-saving-spell+ 24)  ; Apply to save throw: spells
(defparameter +apply-sneak+ 25)
(defparameter +apply-hide+ 26)
(defparameter +apply-race+ 27)
(defparameter +apply-sex+ 28)
(defparameter +apply-backstab+ 29)
(defparameter +apply-pick-lock+ 30)
(defparameter +apply-punch+ 31)
(defparameter +apply-shoot+ 32)
(defparameter +apply-kick+ 33)
(defparameter +apply-track+ 34)
(defparameter +apply-impale+ 35)
(defparameter +apply-behead+ 36)
(defparameter +apply-throwing+ 37)
(defparameter +apply-riding+ 38)
(defparameter +apply-turn+ 39)
(defparameter +apply-saving-chem+ 40)
(defparameter +apply-saving-psi+ 41)
(defparameter +apply-align+ 42)
(defparameter +apply-saving-phy+ 43)
(defparameter +apply-caster+ 44)    ; special usage
(defparameter +apply-weaponspeed+ 45)
(defparameter +apply-disguise+ 46)
(defparameter +apply-nothirst+ 47)
(defparameter +apply-nohunger+ 48)
(defparameter +apply-nodrunk+ 49)
(defparameter +apply-speed+ 50)
(defparameter +num-applies+ 51)


;;; other miscellaneous defines ******************************************


;;; Player conditions
(defparameter +drunk+ 0)
(defparameter +full+ 1)
(defparameter +thirst+ 2)



;;; other #defined constants *********************************************

(defparameter +lvl-grimp+ 72)
(defparameter +lvl-lucifer+ 70)
(defparameter +lvl-impl+ 69)
(defparameter +lvl-entity+ 68)
(defparameter +lvl-ancient+ +lvl-entity+)
(defparameter +lvl-creator+ 67)
(defparameter +lvl-grgod+ 66)
(defparameter +lvl-timegod+ 65)
(defparameter +lvl-deity+ 64)
(defparameter +lvl-god+ 63) ; Lesser God
(defparameter +lvl-energy+ 62)
(defparameter +lvl-force+ 61)
(defparameter +lvl-power+ 60)
(defparameter +lvl-being+ 59)
(defparameter +lvl-spirit+ 58)
(defparameter +lvl-element+ 57)
(defparameter +lvl-demi+ 56)
(defparameter +lvl-eternal+ 55)
(defparameter +lvl-ethereal+ 54)
(defparameter +lvl-luminary+ 53)
(defparameter +lvl-builder+ 52)
(defparameter +lvl-immort+ 51)
(defparameter +lvl-ambassador+ 50)

(defparameter +lvl-freeze+ +lvl-immort+)
(defparameter +lvl-can-ban+ +lvl-god+)
(defparameter +lvl-violence+ +lvl-creator+)
(defparameter +lvl-logall+ +lvl-creator+)
(defparameter +lvl-can-return+ 10)

;;; Character equipment positions: used as index for Creature.equipment[]

;; NOTE: Don't confuse these constants with the ITEM_ bitvectors
;; which control the valid places you can wear a piece of equipment
(defparameter +wear-light+ 0)
(defparameter +wear-finger-r+ 1)
(defparameter +wear-finger-l+ 2)
(defparameter +wear-neck-1+ 3)
(defparameter +wear-neck-2+ 4)
(defparameter +wear-body+ 5)
(defparameter +wear-head+ 6)
(defparameter +wear-legs+ 7)
(defparameter +wear-feet+ 8)
(defparameter +wear-hands+ 9)
(defparameter +wear-arms+ 10)
(defparameter +wear-shield+ 11)
(defparameter +wear-about+ 12)
(defparameter +wear-waist+ 13)
(defparameter +wear-wrist-r+ 14)
(defparameter +wear-wrist-l+ 15)
(defparameter +wear-wield+ 16)
(defparameter +wear-hold+ 17)
(defparameter +wear-crotch+ 18)
(defparameter +wear-eyes+ 19)
(defparameter +wear-back+ 20)
(defparameter +wear-belt+ 21)
(defparameter +wear-face+ 22)
(defparameter +wear-ear-l+ 23)
(defparameter +wear-ear-r+ 24)
(defparameter +wear-wield-2+ 25)
(defparameter +wear-ass+ 26)
(defparameter +num-wears+ 27)   ; This must be the # of eq positions!!
(defparameter +wear-random+ 28)
(defparameter +wear-mshield+ 29) ; This is for mana shield messages just increase it if new wear positions are added

(defparameter +sun-types+
  #("dark" "rise" "light" "set"))
(defparameter +sky-types+
  #("clear" "cloudy" "rain" "storm"))
(defparameter +lunar-phases+
  #("new"
    "waxing crescent"
    "first quarter"
    "waxing gibbous"
    "full"
    "waning gibbous"
    "last quarter"
    "waning crescent"))

(defparameter +moon-sky-types+
  (new-hash-table :none "not visible"
                  :rising "rising"
                  :east "in the east"
                  :high "directly overhead"
                  :west "in the west"
                  :setting "setting"))

(defparameter +daylight-modifiers+ #(-1 -1 0 0 0 0 1 1 1 1 0 0 0 0 -1 -1))

(defparameter +eq-pos-order+
  (coerce (list +wear-head+ +wear-face+ +wear-eyes+ +wear-ear-l+
                +wear-ear-r+ +wear-neck-1+ +wear-neck-2+ +wear-about+
                +wear-body+ +wear-back+ +wear-arms+ +wear-shield+
                +wear-wrist-r+ +wear-wrist-l+ +wear-light+ +wear-hold+
                +wear-wield+ +wear-wield-2+ +wear-waist+ +wear-belt+
                +wear-crotch+ +wear-legs+ +wear-feet+ +wear-ass+
                0 0 0 0 0 0)
          'vector))

(defparameter +soilage-bits+
  #("wet" "bloody" "muddy" "covered with feces"
    "dripping with urine" "smeared with mucus"
    "spattered with saliva" "covered with acid"
    "oily" "sweaty" "greasy" "sooty" "slimy" "sticky"
    "covered with vomit" "rusty" "charred"))

(defparameter +wear-descriptions+
  #("!light!" "right finger" "left finger" "neck" "!neck!" "body"
    "head" "legs" "feet" "hands" "arms" "!shield" "!about!" "waist"
    "right wrist" "left wrist" "!wield!" "!hold!" "crotch" "eyes"
    "back" "!belt!" "face" "left ear" "right ear" "!wield2!" "!ass!"
    "\n"))

(defparameter +char-class-abbrevs+
  #("Mage" "Cler" "Thie" "Warr" "Barb" "Psio" "Phyz" "Borg" "Knig"
    "Rang" "Bard" "Monk" "Vamp" "Merc" "Spa1" "Spa2" "Spa3" "ERR" "ERR"
    "ERR" "ERR" "ERR" "ERR" "ERR" "ERR" "ERR" "ERR" "ERR" "ERR" "ERR"
    "ERR" "ERR" "ERR" "ERR" "ERR" "ERR" "ERR" "ERR" "ERR" "ERR" "ERR"
    "ERR" "ERR" "ERR" "ERR" "ERR" "ERR" "ERR" "ERR" "ERR" "Norm" "Bird"
    "Pred" "Snak" "Hrse" "Smll" "Medm" "Lrge" "Scin" "ERR" "Skel" "Ghou"
    "Shad" "Wigt" "Wrth" "Mumy" "Spct" "Vamp" "Ghst" "Lich" "Zomb" "ERR"
    "ERR" "ERR" "ERR" "ERR" "ERR" "ERR" "ERR" "ERR" "ERR" "Eart" "Fire"
    "Watr" "Air " "Lgtn" "ERR" "ERR" "ERR" "ERR" "ERR" "Gren" "Whte"
    "Blck" "Blue" "Red" "Slvr" "Shad" "Deep" "Turt" "ILL" "Lest" "Lssr"
    "Grtr" "Duke" "Arch" "ILL" "ILL" "ILL" "ILL" "ILL" "Hill" "Ston"
    "Frst" "Fire" "Clod" "Strm" "ILL" "ILL" "ILL" "Red" "Blue" "Gren"
    "Grey" "Deth" "Lord" "ILL" "ILL" "ILL" "ILL" "Tp I" "T II" "TIII"
    "TIV" "Tp V" "T VI" "Semi" "Minr" "Majr" "Lord" "Prnc" "ILL" "ILL" "ILL"
    "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "Astl" "Mond" "Movn" "ILL"
    "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL"
    "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL"
    "ILL" "ILL" "Gdlg" "Diet" "ILL" "ILL" "ILL"))

(defparameter +class-names+
  #("Mage" "Cleric" "Thief" "Warrior" "Barbarian" "Psionic" "Physic" "Cyborg" "Knight" "Ranger" "Bard" "Monk" "Vampire" "Mercenary" "Spare1" "Spare2" "Spare3" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "Mobile" "Bird" "Predator" "Snake" "Horse" "Small" "Medium" "Large" "Scientist" "ILL" "Skeleton" "Ghoul" "Shadow" "Wight" "Wraith" "Mummy" "Spectre" "Vampire" "Ghost" "Lich" "Zombie" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "Earth" "Fire" "Water" "Air" "Lighting" "ILL" "ILL" "ILL" "ILL" "ILL" "Green" "White" "Black" "Blue" "Red" "Silver" "Shadow_D" "Deep" "Turtle" "ILL" "Least" "Lesser" "Greater" "Duke" "Arch" "ILL" "ILL" "ILL" "ILL" "ILL" "Hill" "Stone" "Frost" "Fire" "Cloud" "Storm" "ILL" "ILL" "ILL" "Red" "Blue" "Green" "Grey" "Death" "Lord" "ILL" "ILL" "ILL" "ILL" "Type I" "Type II" "Type III" "Type IV" "Type V" "Type VI" "Semi" "Minor" "Major" "Lord" "Prince" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "Astral" "Monadic" "Movanic" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "Fire" "Lava" "Smoke" "Steam" "ILL" "ILL" "ILL" "ILL" "ILL" "ILL" "Arcana" "Charona" "Dergho" "Hydro" "Pisco" "Ultro" "Yagno" "Pyro" "Godling" "Deity"))

(defparameter +borg-subchar-class-names+ #("power" "speed" "mentat"))

(defparameter +player-races+
  #("Human"
    "Elf"
    "Dwarf"
    "Half Orc"
    "Klingon"
    "Halfling"                          ; 5
    "Tabaxi"
    "Drow"
    "ILL" "ILL"
    "Mobile"                            ; 10
    "Undead"
    "Humanoid"
    "Animal"
    "Dragon"
    "Giant"                             ; 15
    "Orc"
    "Goblin"
    "Hafling"
    "Minotaur"
    "Troll"                             ; 20
    "Golem"
    "Elemental"
    "Ogre"
    "Devil"
    "Trog"
    "Manticore"
    "Bugbear"
    "Draconian"
    "Duergar"
    "Slaad"
    "Robot"
    "Demon"
    "Deva"
    "Plant"
    "Archon"
    "Pudding"
    "Alien 1"
    "Predator Alien"
    "Slime"
    "Illithid"                          ; 40
    "Fish"
    "Beholder"
    "Gaseous"
    "Githyanki"
    "Insect"
    "Daemon"
    "Mephit"
    "Kobold"
    "Umber Hulk"
    "Wemic"
    "Rakshasa"
    "Spider"                            ; 52
    "Griffin"
    "Rotarian"
    "Half Elf"
    "Celestial"
    "Guardinal"
    "Olympian"
    "Yugoloth"
    "Rowlahr"
    "Githzerai"
    "Faerie"
    "\n"))

(defparameter +reputation-msg+
  #("Innocent"                          ; 0 reputation
    "Mostly Harmless"                   ; 1-99
    "Unfriendly"                        ; 100-199
    "Unkind"                            ; 200-299
    "Cold"                              ; 300-399
    "Daunting"                          ; 400-499
    "Feared"                            ; 500-599
    "Frightening"                       ; 600-699
    "Dreaded"                           ; 700-799
    "Terrifying"                        ; 800-899
    "Monstrous"                         ; 900-999
    "True Killer"))                     ; 1000 reputation

(defparameter +month-name+
  #("Month of Winter"
    "Month of the Dire Wolf"
    "Month of the Frost Giant"
    "Month of the Old Forces"
    "Month of the Grand Struggle"
    "Month of Conception"
    "Month of Nature"
    "Month of Futility"
    "Month of the Dragon"
    "Month of the Sun"
    "Month of Madness"
    "Month of the Battle"
    "Month of the Dark Shades"
    "Month of Long Shadows"
    "Month of the Ancient Darkness"
    "Month of the Great Evil"))

(defparameter +str-app+
  #((:to-hit -5 :to-dam -4 :carry-w 0 :wield-w 0)
    (:to-hit -5 :to-dam -4 :carry-w 3 :wield-w 1)
    (:to-hit -3 :to-dam -2 :carry-w 3 :wield-w 2)
    (:to-hit -3 :to-dam -1 :carry-w 10 :wield-w 3)
    (:to-hit -2 :to-dam -1 :carry-w 25 :wield-w 4)
    (:to-hit -2 :to-dam -1 :carry-w 55 :wield-w 5) ; 5
    (:to-hit -1 :to-dam 0 :carry-w 80 :wield-w 6)
    (:to-hit -1 :to-dam 0 :carry-w 90 :wield-w 7)
    (:to-hit 0 :to-dam 0 :carry-w 100 :wield-w 8)
    (:to-hit 0 :to-dam 0 :carry-w 110 :wield-w 9)
    (:to-hit 0 :to-dam 0 :carry-w 120 :wield-w 10) ; 10
    (:to-hit 0 :to-dam 0 :carry-w 130 :wield-w 11)
    (:to-hit 0 :to-dam 0 :carry-w 140 :wield-w 12)
    (:to-hit 1 :to-dam 0 :carry-w 150 :wield-w 13)
    (:to-hit 1 :to-dam 1 :carry-w 160 :wield-w 14)
    (:to-hit 1 :to-dam 1 :carry-w 180 :wield-w 15) ; 15
    (:to-hit 1 :to-dam 2 :carry-w 200 :wield-w 16)
    (:to-hit 1 :to-dam 2 :carry-w 220 :wield-w 18)
    (:to-hit 1 :to-dam 3 :carry-w 245 :wield-w 20) ; 18
    (:to-hit 3 :to-dam 13 :carry-w 650 :wield-w 33)
    (:to-hit 3 :to-dam 14 :carry-w 700 :wield-w 35) ; 20
    (:to-hit 4 :to-dam 15 :carry-w 750 :wield-w 36)
    (:to-hit 4 :to-dam 16 :carry-w 800 :wield-w 37)
    (:to-hit 5 :to-dam 17 :carry-w 850 :wield-w 38)
    (:to-hit 6 :to-dam 18 :carry-w 955 :wield-w 39)
    (:to-hit 7 :to-dam 20 :carry-w 1000 :wield-w 40) ; 25
    (:to-hit 1 :to-dam 4 :carry-w 270 :wield-w 21)   ; 18/10
    (:to-hit 1 :to-dam 4 :carry-w 295 :wield-w 22)   ; 18/20
    (:to-hit 1 :to-dam 5 :carry-w 320 :wield-w 23)   ; 18/30
    (:to-hit 1 :to-dam 5 :carry-w 350 :wield-w 24)   ; 18/40
    (:to-hit 2 :to-dam 6 :carry-w 380 :wield-w 25)   ; 18/50
    (:to-hit 2 :to-dam 7 :carry-w 410 :wield-w 26)   ; 18/60
    (:to-hit 2 :to-dam 8 :carry-w 440 :wield-w 27)   ; 18/70
    (:to-hit 2 :to-dam 9 :carry-w 480 :wield-w 28)   ; 18/80
    (:to-hit 2 :to-dam 10 :carry-w 520 :wield-w 29)  ; 18/90
    (:to-hit 3 :to-dam 11 :carry-w 560 :wield-w 30)  ; 18/99
    (:to-hit 3 :to-dam 12 :carry-w 600 :wield-w 31))) ; 18/00

(defun str-app-type-to-hit (plist)
  (getf plist :to-hit))

(defun str-app-type-to-dam (plist)
  (getf plist :to-dam))

(defparameter +con-app+
  #((:hitp -4 :shock 20)
    (:hitp -3 :shock 25)
    (:hitp -2 :shock 30)
    (:hitp -2 :shock 35)
    (:hitp -1 :shock 40)
    (:hitp -1 :shock 45)
    (:hitp -1 :shock 50)
    (:hitp 0 :shock 55)
    (:hitp 0 :shock 60)
    (:hitp 0 :shock 65)
    (:hitp 0 :shock 70)
    (:hitp 0 :shock 75)
    (:hitp 0 :shock 80)
    (:hitp 1 :shock 85)
    (:hitp 2 :shock 88)
    (:hitp 3 :shock 90)
    (:hitp 5 :shock 95)
    (:hitp 6 :shock 97)
    (:hitp 7 :shock 99)
    (:hitp 8 :shock 99)
    (:hitp 9 :shock 99)
    (:hitp 10 :shock 99)
    (:hitp 11 :shock 99)
    (:hitp 12 :shock 99)
    (:hitp 13 :shock 99)
    (:hitp 14 :shock 100)))

(defparameter +dex-app+
  #((:reaction -7 :miss-att -7 :defensive 6 :tohit -5 :todam -4)
    (:reaction -6 :miss-att -6 :defensive 5 :tohit -5 :todam -4)
    (:reaction -4 :miss-att -4 :defensive 5 :tohit -3 :todam -2)
    (:reaction -3 :miss-att -3 :defensive 4 :tohit -3 :todam -1)
    (:reaction -2 :miss-att -2 :defensive 3 :tohit -2 :todam -1)
    (:reaction -1 :miss-att -1 :defensive 2 :tohit -1 :todam 0)
    (:reaction 0 :miss-att 0 :defensive 1 :tohit -1 :todam 0)
    (:reaction 0 :miss-att 0 :defensive 0 :tohit 0 :todam 1)
    (:reaction 0 :miss-att 0 :defensive 0 :tohit 0 :todam 1)
    (:reaction 0 :miss-att 0 :defensive 0 :tohit 0 :todam 3)
    (:reaction 0 :miss-att 0 :defensive 0 :tohit 0 :todam 3)
    (:reaction 0 :miss-att 0 :defensive 0 :tohit 0 :todam 4)
    (:reaction 0 :miss-att 0 :defensive 0 :tohit 1 :todam 5)
    (:reaction 0 :miss-att 0 :defensive 0 :tohit 1 :todam 6)
    (:reaction 0 :miss-att 0 :defensive 0 :tohit 1 :todam 7)
    (:reaction 1 :miss-att 0 :defensive -1 :tohit 1 :todam 8)
    (:reaction 1 :miss-att 1 :defensive -2 :tohit 2 :todam 9)
    (:reaction 2 :miss-att 2 :defensive -3 :tohit 2 :todam 10)
    (:reaction 2 :miss-att 2 :defensive -4 :tohit 3 :todam 13)
    (:reaction 3 :miss-att 3 :defensive -5 :tohit 3 :todam 14)
    (:reaction 3 :miss-att 3 :defensive -5 :tohit 4 :todam 15)
    (:reaction 4 :miss-att 4 :defensive -6 :tohit 4 :todam 16)
    (:reaction 4 :miss-att 4 :defensive -6 :tohit 5 :todam 17)
    (:reaction 5 :miss-att 5 :defensive -7 :tohit 6 :todam 18)
    (:reaction 6 :miss-att 6 :defensive -7 :tohit 7 :todam 19)
    (:reaction 7 :miss-att 7 :defensive -8 :tohit 8 :todam 20)))

(defparameter +trail-flags+ #("BLOODPRINTS" "BLOOD_DROPS"))

(defparameter +spell-bit-keywords+
  #("damage" "affects" "unaffects" "points" "alterations" "groups"
    "masses" "areas" "summons" "creations" "manual" "objects"
    "touch" "magic" "divine" "physics" "psionic" "biologic"
    "cybernetic" "evil" "good" "exits" "outdoors" "nowater"
    "waterzap" "nosun" "zen" "mercenary" "song"))

(defparameter +player-bits+
  #("KILLER" "THIEF" "FROZEN" "DONTSET" "WRITING" "MAILING"
    "CSH" "SITEOK" "!SHOUT" "!TITLE" "*DELETED*" "LODRM" "!WIZL"
    "!DEL" "INVST" "CRYO" "AFK" "C-LEADR" "TOUGHGUY" "OLC"
    "HALTED" "OLCGOD" "TESTER" "Q-GOD" "MORTALIZED" "REMORT-TOUGH"
    "!UNUSED!" "NOPOST" "KRN" "!UNUSED!" "NOPK" "SOULLESS"))

(defparameter +player2-bits+
  #("SOULLESS" "BURIED" "IN-COMBAT"))

(defparameter +action-bits+
  #("SPEC" "SENTINEL" "SCAVENGER" "ISNPC" "AWARE" "AGGR"
    "STAY-ZONE" "WIMPY" "AGGR-EVIL" "AGGR-GOOD" "AGGR-NEUTRAL"
    "MEMORY" "HELPER" "!CHARM" "!SUMMN" "!SLEEP" "!BASH" "!BLIND"
    "!TURN" "!PETRI" "PET" "SOULLESS" "SPRT-HNTR" "UTILITY"))

(defparameter +action-bits-desc+
  #("Special" "Sentinel" "Scavenger" "NPC" "Aware" "Aggressive"
    "Stay-Zone" "Wimpy" "Aggro-Evil" "Aggro-Good" "Aggro-Neutral"
    "Memory" "Helper" "NoCharm" "NoSummon" "NoSleep" "NoBash"
    "NoBlind" "NoTurn" "nopetri" "pet" "soulless" "Spirit-Hunter"
    "Utility"))

(defparameter +action2-bits+
  #("SCRIPT" "MOUNT" "STAY-SECT" "ATK-MOBS" "HUNT" "LOOT" "NOSTUN"
    "SELLR" "!WEAR" "SILENT" "FAMILIAR" "NO-FLOW" "!APPROVE"
    "RENAMED" "!AGGR-RACE" "15" "16" "17" "18" "19"))

(defparameter +action2-bits-desc+
  #("script" "Mount" "Stay-Sector" "Attack-Mobs" "Hunt" "Loot"
    "NoStun" "Seller" "NoWear" "Silent-Hunter" "Familiar" "NoFlow"
    "Unnapproved" "Renamed" "Noaggro-Race" "mugger"))

(defparameter +preference-bits+
  #("BRieF" "CMPCT" "DEAF" "!TELL" "D-HP" "D-MAN" "D-MV" "AEX"
    "!HaSS" "NASTY" "SUMN" "!REP" "HLGHT" "C1" "C2" "!WiZ" "L1"
    "L2" "!AuC" "!GoS" "!GTZ" "RmFLG" "!SNooP" "!MuS" "!SpW"
    "!MiS" "!PROJ" "!INTWZ" "!CLN-SAY" "!IDENT" "!DREAM"
    "DEBUG" "Newbie-HLPR" "DIAG" "BEEP" "!AFF" "!HOLR" "!IMM"
    "C-TIT" "C-HID" "L-READ" "AUTOPRMPT" "NOWHO" "ANON" "!TRAIL"
    "VT100" "AUTOSPLIT" "AUTOLOOT" "PK" "NOGECHO" "NOWRAP"
    "DSPALGN" "WLDWRIT"))

(defparameter +affected-bits+
  #("BLiND" "INViS" "DT-ALN" "DT-INV" "DT-MAG" "SNSE-L" "WaTWLK"
    "SNCT" "GRP" "CRSE" "NFRa" "PoIS" "PRT-E" "PRT-G" "SLeP"
    "!TRK" "NFLT" "TiM_W" "SNK" "HiD" "H2O-BR" "CHRM" "CoNFu"
    "!PaIN" "ReTNa" "ADReN" "CoNFi" "ReJV" "ReGN" "GlWL" "BlR"
    "31"))

(defparameter +affected-bits-desc+
  #("Blind" "Invisible" "Det_Alignment" "Det_Invisible" "Det_Magic" "Sense_Life" "Waterwalk" "Sanctuary" "Group" "Curse" "Infravision" "Poison" "Prot_Evil" "Prot_Good" "Sleep" "NoTrack" "inflight" "Time_Warp" "Sneak" "Hide" "WaterBreath" "Charm" "Confusion" "NoPain" "Retina" "Adrenaline" "Confidence" "Rejuvenation" "Regeneration" "Glowlight" "Blur"))

(defparameter +affected2-bits+
  #("FLUOR" "TRANSP" "SLOW" "HASTE" "MOUNTED" "FIRESHLD" "BERSERK"
    "INTIMD" "TRUE_SEE" "DIV_ILL" "PROT_UND" "INVS_UND" "ANML_KIN"
    "END_COLD" "PARALYZ" "PROT_LGTN" "PROT_FIRE" "TELEK" "PROT_RAD"
    "BURNING!" "!BEHEAD" "DISPLACR" "PROT_DEVILS" "MEDITATE" "EVADE"
    "BLADE BARRIER" "OBLIVITY" "NRG_FIELD" "PETRI" "VERTIGO"
    "PROT_DEMONS"))

(defparameter +affected2-bits-desc+
  #("Fluorescent" "Transparent" "Slow" "Haste" "Mounted" "Fireshield" "Berserk" "Intimidation" "True_See" "Divine_Ill" "Prot_Undead" "Invis_Undead" "Invis_Animals" "End_Cold" "Paralyze" "Prot_Lightning" "Prot_Fire" "Telekinesis" "Prot_Radiation" "Burning" "No_BeHead" "Displacer" "Prot_Devils" "Meditate" "Evade" "Blade_Barrier" "Oblivity" "Energy_Field" "Petri" "Vertigo" "Prot_Demons"))

(defparameter +affected3-bits+
  #("ATTR-FIELD" "ENERGY_LEAK" "POISON-2" "POISON-3" "SICK"
    "S-DEST!" "DAMCON" "STASIS" "P-SPHERE" "RAD" "DET_POISON"
    "MANA_TAP" "ENERGY_TAP" "SONIC_IMAGERY" "SHR_OBSC" "!BREATHE"
    "PROT_HEAT" "PSISHIELD" "PSY CRUSH" "2xDAM" "ACID" "HAMSTRING"
    "GRV WELL" "SMBL PAIN" "EMP_SHLD" "IAFF" "!UNUSED!" "TAINTED"
    "INFIL" "DivPwR" "MANA_LEAK"))

(defparameter +affected3-bits-desc+
  #("attraction_field" "Energy_Leak" "Poison_2" "Poison_3" "Sick" "Self_Destruct" "Damage_Control" "Stasis" "Prismatic_Sphere" "Radioactive" "Detect Poison" "Mana_Tap" "Energy_Tap" "Sonic_Imagery" "Shroud_Obscurement" "Nobreathe" "Prot_Heat" "psishield" "psychic_crush" "double_damage" "acidity" "hamstring" "gravity well" "symbol of pain" "emp shielding" "!instant affect!" "Sniped" "Tainted" "Infiltrating" "Divine Power" "Mana_Leak"))


(defparameter +extra-bits+
  #("GLOW" "HUM" "!RENT" "!DON" "!INVIS" "INVIS" "MAGIC" "!DROP" "BLESS" "!GOOD" "!EVIL" "!NEU" "!MAGE" "!CLE" "!THI" "!WAR" "!SELL" "!BAR" "!PSI" "!PHY" "!CYB" "!KNI" "!RAN" "!BARD" "!MONK" "BLUR" "!DISP_MAG" "unused138" "REP_FLD" "TRANSP" "DAMNED"))

(defparameter +extra-names+
  #("glow" "hum" "norent" "nodonate" "noinvis" "invisible" "magic" "nodrop" "bless" "nogood" "noevil" "noneutral" "nomage" "nocleric" "nothief" "nowarrior" "nosell" "nobarb" "nopsionic" "nophysic" "nocyborg" "noknight" "noranger" "nobard" "nomonk" "blur" "nodispel_magic" "unused138" "repulsion_field" "transparent" "evil_bless"))

(defparameter +extra2-bits+
  #("RADACT" "!MERC" "!SPR1" "!SPR2" "!SPR3" "HIDDEN" "TRAPPED" "SINGULAR" "!LOC" "!SOIL" "CAST_W" "2_HAND" "BODY PART" "ABLAZE" "PERM_CURSE" "!REMOVE" "THROWN" "GODEQ" "!MORT" "BROKEN" "IMPLANT" "REINF" "ENHAN" "MORT" "*" "*" "*" "*" "PROT_HUNT" "RENAMED" "_!APPROVE_"))

(defparameter +extra2-names+
  #("radioactive" "nomerc" "nospare1" "nospare2" "nospare3" "hidden" "trapped" "singular" "nolocate" "nosoilage" "casting_weapon" "two_handed" "body_part" "ablaze" "perm_curse" "noremove" "throwable" "godeq" "nomort" "broken" "implant" "reinforced" "enhanced" "mort" "*" "*" "*" "*" "prot_hunt" "renamed" "unapproved"))

(defparameter +extra3-bits+
  #("MAGE" "CLE" "THI" "WAR" "BAR" "PSI" "PHY" "CYB" "KNI" "RAN" "BARD" "MONK" "VAMP" "MER" "SPR1" "SPR2" "SPR3" "HARD" "STAY" "HUNT" "!MAG" "!SCI"))

(defparameter +extra3-names+
  #("mage" "cleric" "thief" "warrior" "barbarian" "psionic" "physic" "cyborg" "knight" "ranger" "bard" "monk" "vampire" "mercenary" "spare1" "spare2" "spare3" "hardened" "stayzone" "hunted" "nomagic" "noscience"))

(defparameter +apply-types+
  #("NONE" "STR" "DEX" "INT" "WIS" "CON" "CHA" "CLASS" "LEVEL" "AGE" "WEIGHT" "HEIGHT" "MAXMANA" "MAXHIT" "MAXMOVE" "GOLD" "EXP" "ARMOR" "HITROLL" "DAMROLL" "SAV_PARA" "SAV_ROD" "SAV_PETRI" "SAV_BREATH" "SAV_SPELL" "SNEAK" "HIDE" "RACE" "SEX" "BACKST" "PICK" "PUNCH" "SHOOT" "KICK" "TRACK" "IMPALE" "BEHEAD" "THROW" "RIDING" "TURN" "SAV_CHEM" "SAV_PSI" "ALIGN" "SAV_PHY" "CASTER" "WEAP_SPEED" "DISGUISE" "NOTHIRST" "NOHUNGER" "NODRUNK" "SPEED"))

;;; Take/Wear flags: used by obj_data.obj_flags.wear_flags
(defparameter +item-wear-take+ (ash 1 0))   ; Item can be takes
(defparameter +item-wear-finger+ (ash 1 1)) ; Can be worn on finger
(defparameter +item-wear-neck+ (ash 1 2))   ; Can be worn around neck
(defparameter +item-wear-body+ (ash 1 3))   ; Can be worn on body
(defparameter +item-wear-head+ (ash 1 4))   ; Can be worn on head
(defparameter +item-wear-legs+ (ash 1 5))   ; Can be worn on legs
(defparameter +item-wear-feet+ (ash 1 6))   ; Can be worn on feet
(defparameter +item-wear-hands+ (ash 1 7))  ; Can be worn on hands
(defparameter +item-wear-arms+ (ash 1 8))   ; Can be worn on arms
(defparameter +item-wear-shield+ (ash 1 9)) ; Can be used as a shield
(defparameter +item-wear-about+ (ash 1 10)) ; Can be worn about body
(defparameter +item-wear-waist+ (ash 1 11)) ; Can be worn around waist
(defparameter +item-wear-wrist+ (ash 1 12)) ; Can be worn on wrist
(defparameter +item-wear-wield+ (ash 1 13)) ; Can be wielded
(defparameter +item-wear-hold+ (ash 1 14))  ; Can be held
(defparameter +item-wear-crotch+ (ash 1 15)) ; guess where
(defparameter +item-wear-eyes+ (ash 1 16))   ; eyes
(defparameter +item-wear-back+ (ash 1 17))   ;Worn on back
(defparameter +item-wear-belt+ (ash 1 18)) ; Worn on a belt(ie, pouch)
(defparameter +item-wear-face+ (ash 1 19))
(defparameter +item-wear-ear+ (ash 1 20))
(defparameter +item-wear-ass+ (ash 1 21)) ;Can be RAMMED up an asshole
(defparameter +num-wear-flags+ 22)

(defparameter +wear-bitvectors+
  (coerce (list +item-wear-take+ +item-wear-finger+ +item-wear-finger+
                +item-wear-neck+ +item-wear-neck+ +item-wear-body+
                +item-wear-head+ +item-wear-legs+ +item-wear-feet+
                +item-wear-hands+ +item-wear-arms+ +item-wear-shield+
                +item-wear-about+ +item-wear-waist+ +item-wear-wrist+
                +item-wear-wrist+ +item-wear-wield+ +item-wear-hold+
                +item-wear-crotch+ +item-wear-eyes+ +item-wear-back+
                +item-wear-belt+ +item-wear-face+ +item-wear-ear+
                +item-wear-ear+ +item-wear-wield+ +item-wear-ass+)
          'vector))

(defparameter +wear-bits-desc+
  #("TAKE" "FINGER" "NECK" "BODY" "HEAD" "LEGS" "FEET" "HANDS"
    "ARMS" "SHIELD" "ABOUT" "WAIST" "WRIST" "WIELD" "HOLD" "CROTCH"
    "EYES" "BACK" "BELT" "FACE" "EAR" "ASS"))

(defparameter +wear-eq-positions+
  (list (cons +item-wear-finger+ +wear-finger-r+)
        (cons +item-wear-neck+ +wear-neck-1+)
        (cons +item-wear-body+ +wear-body+)
        (cons +item-wear-head+ +wear-head+)
        (cons +item-wear-legs+ +wear-legs+)
        (cons +item-wear-feet+ +wear-feet+)
        (cons +item-wear-hands+ +wear-hands+)
        (cons +item-wear-arms+ +wear-arms+)
        (cons +item-wear-shield+ +wear-shield+)
        (cons +item-wear-about+ +wear-about+)
        (cons +item-wear-waist+ +wear-waist+)
        (cons +item-wear-wrist+ +wear-wrist-r+)
        (cons +item-wear-crotch+ +wear-crotch+)
        (cons +item-wear-eyes+ +wear-eyes+)
        (cons +item-wear-back+ +wear-back+)
        (cons +item-wear-belt+ +wear-belt+)
        (cons +item-wear-face+ +wear-face+)
        (cons +item-wear-ear+ +wear-ear-l+)
        (cons +item-wear-ass+ +wear-ass+)))

(defparameter +wear-keywords+
  #("!RESERVED! (light)"
    "finger"
    "!RESERVED! (finger)"
    "neck"
    "!RESERVED! (neck)"
    "body"
    "head"
    "legs"
    "feet"
    "hands"
    "arms"
    "shield"
    "about"
    "waist"
    "wrist"
    "!RESERVED! (wrist)"
    "!RESERVED! (wield)"
    "!RESERVED! (hold)"
    "crotch"
    "eyes"
    "back"
    "belt"
    "face"
    "ear"
    "!RESERVED! (ear)"
    "WIELD 2"
    "ass"))

(defparameter +wear-translator+
  (vector +wear-light+
          +wear-finger-r+
          +wear-finger-r+
          +wear-neck-1+
          +wear-neck-1+
          +wear-body+
          +wear-head+
          +wear-legs+
          +wear-feet+
          +wear-hands+
          +wear-arms+
          +wear-shield+
          +wear-back+
          +wear-waist+
          +wear-wrist-r+
          +wear-wrist-r+
          +wear-wield+
          +wear-hold+
          +wear-crotch+
          +wear-eyes+
          +wear-back+
          +wear-waist+
          +wear-face+
          +wear-ear-l+
          +wear-ear-l+
          +wear-wield-2+
          +wear-ass+))

(defparameter +eq-pos-descs+
  #("<as light>       "
    "<on finger>      "
    "<on finger>      "
    "<around neck>    "
    "<around neck>    "
    "<on body>        "
    "<on head>        "
    "<on legs>        "
    "<on feet>        "
    "<on hands>       "
    "<on arms>        "
    "<as shield>      "
    "<about body>     "
    "<about waist>    "
    "<around wrist>   "
    "<around wrist>   "
    "<wielded>        "
    "<held>           "
    "<on crotch>      "
    "<on eyes>        "
    "<on back>        "
    "<on belt>        "
    "<on face>        "
    "<on left ear>    "
    "<on right ear>   "
    "<second wielded> "
    "<stuck up ass>   "))

(defparameter +tattoo-pos-descs+
  #("<INVALID>        "
    "<INVALID>        "
    "<INVALID>        "
    "<on neck>        "
    "<INVALID>        "
    "<on chest>       "
    "<on scalp>       "
    "<on legs>        "
    "<on feet>        "
    "<on hands>       "
    "<on arms>        "
    "<INVALID>        "
    "<INVALID>        "
    "<about waist>    "
    "<around wrist>   "
    "<around wrist>   "
    "<INVALID>        "
    "<INVALID>        "
    "<around crotch>  "
    "<INVALID>        "
    "<on back>        "
    "<INVALID>        "
    "<on face>        "
    "<on left ear>    "
    "<on right ear>   "
    "<INVALID>        "
    "<on buttocks>    "))

(defparameter +already-wearing+
  #("You're already using a light."
    "YOU SHOULD NEVER SEE THIS MESSAGE.  PLEASE REPORT."
    "You're already wearing something on both of your ring fingers."
    "YOU SHOULD NEVER SEE THIS MESSAGE.  PLEASE REPORT."
    "You can't wear anything else around your neck."
    "You're already wearing something on your body."
    "You're already wearing something on your head."
    "You're already wearing something on your legs."
    "You're already wearing something on your feet."
    "You're already wearing something on your hands."
    "You're already wearing something on your arms."
    "You're already using a shield."
    "You're already wearing something about your body."
    "You already have something around your waist."
    "YOU SHOULD NEVER SEE THIS MESSAGE.  PLEASE REPORT."
    "You're already wearing something around both of your wrists."
    "You're already wielding a weapon."
    "You're already holding something."
    "You've already got something on your crotch."
    "You already have something on your eyes."
    "You already have something on your back."
    "There is already something attached to your belt."
    "You are already wearing something on your face."
    "YOU SHOULD NEVER SEE THIS MESSAGE.  PLEASE REPORT."
    "You are wearing something in both ears already."
    "You are already wielding something in your off hand."
    "You already have something stuck up yer butt!"))

(defparameter +wear-messages+
  #2A(("$n lights $p and holds it."
       "You light $p and hold it.")
      ("$n slides $p on to $s right ring finger."
       "You slide $p on to your right ring finger.")
      ("$n slides $p on to $s left ring finger."
       "You slide $p on to your left ring finger.")
      ("$n wears $p around $s neck."
       "You wear $p around your neck.")
      ("$n wears $p around $s neck."
       "You wear $p around your neck.")
      ("$n wears $p on $s body."
       "You wear $p on your body.")
      ("$n wears $p on $s head."
       "You wear $p on your head.")
      ("$n puts $p on $s legs."
       "You put $p on your legs.")
      ("$n wears $p on $s feet."
       "You wear $p on your feet.")
      ("$n puts $p on $s hands."
       "You put $p on your hands.")
      ("$n wears $p on $s arms."
       "You wear $p on your arms.")
      ("$n straps $p around $s arm as a shield."
       "You start to use $p as a shield.")
      ("$n wears $p about $s body."
       "You wear $p around your body.")
      ("$n wears $p around $s waist."
       "You wear $p around your waist.")
      ("$n puts $p around $s right wrist."
       "You put $p around your right wrist.")
      ("$n puts $p around $s left wrist."
       "You put $p around your left wrist.")
      ("$n wields $p."
       "You wield $p.")
      ("$n grabs $p."
       "You grab $p.")
      ("$n puts $p on $s crotch."
       "You put $p on your crotch.")
      ("$n puts $p on $s eyes."
       "You put $p on your eyes.")
      ("$n wears $p on $s back."
       "You wear $p on your back.")
      ("$n attaches $p to $s belt."
       "You attach $p to your belt.")
      ("$n wears $p on $s face."
       "You wear $p on your face.")
      ("$n puts $p in $s left ear lobe."
       "You wear $p in your left ear.")
      ("$n puts $p in $s right ear lobe."
       "You wear $p in your right ear.")
      ("$n wields $p in $s off hand."
       "You wield $p in your off hand.")
      ("$n RAMS $p up $s ass!!!"
       "You RAM $p up your ass!!!")))

(defparameter +item-kinds+
  #("undefined" "light" "scroll" "wand" "staff" "weapon" "camera" "missile"
    "treasure" "armor" "potion" "worn" "other" "trash" "trap" "container" "note"
    "liq cont" "key" "food" "money" "pen" "boat" "fountain" "wings" "vr_interface"
    "scuba_mask" "device" "interface" "holy symbol" "vehicle" "engine" "battery"
    "energy_gun" "window" "portal" "tobacco" "cigarette" "metal" "v-stone" "pipe"
    "transporter" "syringe" "chit" "scuba_tank" "tattoo" "tool" "bomb" "detonator"
    "fuse" "podium" "pill" "energy_cell" "v_window" "v_door" "v_console" "gun"
    "bullet" "clip" "microchip" "communicator" "script" "instrument" "book"))

(defparameter +item-kind-values+
  #2a(("UNDEFINED" "UNDEFINED" "UNDEFINED" "UNDEFINED")
      ("Color" "Type" "Hours" "UNDEFINED")  ; Light
      ("Level" "Spell1" "Spell2" "Spell3")  ; Scroll
      ("Level" "Max Charg" "Cur Chrg" "Spell")  ; wand
      ("Level" "Max Charg" "Cur Chrg" "Spell")  ; staff
      ("Spell" "Dam dice1" "Dam dice " "Atck type") ; weapon
      ("Targ room" "UNDEFINED" "UNDEFINED" "UNDEFINED") ; camera
      ("UNDEFINED" "UNDEFINED" "UNDEFINED" "UNDEFINED") ; missile
      ("UNDEFINED" "UNDEFINED" "UNDEFINED" "UNDEFINED") ; treasure
      ("AC-Apply" "UNDEFINED" "UNDEFINED" "UNDEFINED")  ; armor
      ("Level" "Spell1" "Spell2" "Spell3")  ; Potion
      ("UNDEFINED" "UNDEFINED" "UNDEFINED" "UNDEFINED") ; worn
      ("UNDEFINED" "UNDEFINED" "UNDEFINED" "UNDEFINED") ; other
      ("UNDEFINED" "UNDEFINED" "UNDEFINED" "UNDEFINED") ; trash
      ("UNDEFINED" "UNDEFINED" "UNDEFINED" "UNDEFINED") ; trap
      ("Max Capac" "Flags" "Keynum" "DONT SET") ; container
      ("UNDEFINED" "UNDEFINED" "UNDEFINED" "UNDEFINED") ; note
      ("Max units" "Cur Unit" "Liq.type" "Poison")  ; liq cont
      ("Keytype" "Rentflag" "UNDEFINED" "UNDEFINED")    ; key
      ("Hours" "Spell lev" "Spellnum" "Poison") ; food
      ("Num Coins" "Type" "UNDEFINED" "UNDEFINED")  ; money
      ("UNDEFINED" "UNDEFINED" "UNDEFINED" "UNDEFINED") ; pen
      ("UNDEFINED" "UNDEFINED" "UNDEFINED" "UNDEFINED") ; boat
      ("Max units" "Cur Units" "Liq.type" "Poison") ; fountain
      ("UNDEFINED" "UNDEFINED" "UNDEFINED" "UNDEFINED") ; wings
      ("Startroom" "Cost" "Max Lev." "Hometown")    ; vr interface
      ("State" "UNDEFINED" "UNDEFINED" "UNDEFINED") ; scuba
      ("Max energ" "Cur Energ" "State" "Rate     ") ; device
      ("Type" "UNDEFINED" "Max" "UNDEFINED")    ; interface
      ("Align" "Class" "Min level" "Max level") ; holy symb
      ("Room/KeyNum" "Doorstate" "Flags" "Special") ; vehicle
      ("Max energ" "Cur Energ" "Enginstat" "Rate     ") ; engine
      ("Max charg" "Cur charg" "Rate" "Cost/unit")  ; battery
      ("Drain Rate" "Dam dice1" "Dam dice2" "gun type") ; raygun
      ("Targ room" "Doorstate" "UNDEFINED" "UNDEFINED") ; window
      ("Targ room" "Doorstate" "Keynum?" "Charges") ; portal
      ("Type" "Max Drags" "UNDEFINED" "UNDEFINED")  ; tobacco
      ("Drags lft" "UNDEFINED" "Tobac typ" "Lit?")  ; joint
      ("UNDEFINED" "UNDEFINED" "UNDEFINED" "UNDEFINED") ; metal
      ("Targ room" "Idnum" "Charges" "UNDEFINED")   ; vstone
      ("Drags lft" "Max drags" "Tobac typ" "Lit?   ")   ; pipe
      ("Max Energy" "Cur Energ" "To-room" "tunable?")   ; transporter
      ("Level" "Spell1" "Spell2" "Spell3")  ; Syringe
      ("Credits  " "UNDEFINED" "UNDEFINED" "UNDEFINED") ; chit
      ("Max Units" "Cur Unit" "UNDEFINED" "UNDEFINED")  ; scuba tank
      ("UNDEFINED" "UNDEFINED" "UNDEFINED" "UNDEFINED")   ; tattoo
      ("skillnum" "modifier" "UNDEFINED" "UNDEFINED")   ; tool *
      ("Type" "Power" "UND" "Idnum")    ; bomb *
      ("UNDEFINED" "UNDEFINED" "UNDEFINED" "UNDEFINED") ; detonator
      ("Type" "State" "Timer" "UNDEFINED")  ; fuse
      ("UNDEFINED" "UNDEFINED" "UNDEFINED" "UNDEFINED") ; podium
      ("Level" "Spell1" "Spell2" "Spell3")  ; Pill
      ("Max charg" "Cur charg" "Recharge rate" "UNDEF") ; energy cell
      ("Room" "Doorflags" "Car Vnum" "UNDEFINED")   ; v window
      ("Room" "Unused" "Car Vnum" "UNDEFINED")  ; v door
      ("Room" "Unused" "Car Vnum" "Driver Idnum")   ; v console
      ("MAX ROF" "CUR ROF" "Max Load" "Gun Type")   ; gun
      ("UNDEFINED" "UNDEFINED" "Dam Mod" "Gun Type")    ; bullet
      ("UNDEFINED" "UNDEFINED" "Max Load" "Gun Type")   ; clip
      ("Type" "Data" "Max" "UNDEFINED") ; chip
      ("Max Charge" "Cur Charge" "State" "Channel") ; Communicator
      ("Top Message" "Mode" "Wait Time" "Counter")  ; Script
      ("UNDEFINED" "UNDEFINED" "UNDEFINED" "UNDEFINED") ; Instrument
      ("Level" "Spell1" "Spell2" "Spell3")  ; Book
      ("UNDEFINED" "UNDEFINED" "UNDEFINED" "UNDEFINED")))

(defparameter +drinks+
  #("water"
    "beer"
    "wine"
    "ale"
    "dark ale"
    "whiskey"
    "lemonade"
    "firebreather"
    "local specialty"
    "slime"
    "milk"
    "tea"
    "coffee"
    "blood"
    "salt water"
    "clear water"
    "coke"
    "firetalon"
    "soup"
    "mud"
    "holy water"
    "orange juice"
    "goatsmilk"
    "mucus"
    "pus"
    "sprite"
    "diet coke"
    "root beer"
    "vodka"
    "beer"
    "urine"
    "stout"
    "souls"
    "champagne"
    "cappucino"
    "rum"
    "sake"
    "chocolate milk"
    "juice"
    "mead"))

(defparameter +drink-names+
  #("water"
    "beer"
    "wine"
    "ale"
    "ale"
    "whiskey"
    "lemonade"
    "firebreather"
    "local"
    "slime mold juice"
    "milk"
    "tea"
    "coffee"
    "blood"
    "salt water"
    "water clear"
    "coke"
    "firetalon"
    "soup"
    "mud"
    "holy water"
    "orange juice"
    "goatsmilk"
    "mucus"
    "pus"
    "sprite"
    "diet"
    "root"
    "vodka"
    "beer"
    "urine"
    "stout"
    "souls"
    "champagne bubbly"
    "cappucino coffee"
    "rum"
    "sake"
    "chocolate milk"
    "juice"
    "mead"))

(defparameter +drink-affects+
  #2A((0 1 10)                          ;water
      (3 2 5)                           ; beer
      (5 2 5)                           ; wine
      (2 2 5)                           ; ale
      (1 2 5)                           ; darkale
      (6 1 4)                           ; whiskey; 5
      (0 1 8)                           ; lemonade
      (10 0 0)                          ; firebreather
      (3 3 3)                           ; local specialty
      (0 4 -8)                          ; slime
      (0 3 6)                           ; milk; 10
      (0 1 6)                           ; tea
      (0 1 6)                           ; coffee
      (0 2 -1)                          ; blood
      (0 1 -2)                          ; saltwater
      (0 0 13)                          ; clear water; 15
      (0 1 10)                          ; coke
      (10 0 1)                          ; fire talon
      (0 5 5)                           ; soup
      (0 3 -2)                          ; mud
      (0 1 10)                          ; holy water; 20
      (0 2 8)                           ; orange juice
      (0 3 8)                           ; goatsmilk
      (0 3 -5)                          ; mucus
      (0 1 0)                           ; pus
      (0 1 10)                          ; sprite 25
      (0 1 10)                          ; diet coke
      (0 1 10)                          ; root beer
      (12 0 2)                          ; vodka
      (1 1 6)                           ; city beer
      (0 0 -2)                          ; urine 30
      (5 3 6)                           ; stout
      (2 1 8)                           ; souls
      (1 2 2)                           ; champagne
      (0 1 6)                           ; cappucino
      (10 0 2)                          ; rum
      (12 0 2)                          ; sake
      (0 3 6)                           ; chocolate milk
      ))

(defparameter +flow-types+
  #("None" "Wind" "Falling" "River" "Water Vortex" "Underwater"
    "Conduit" "Conveyor" "Lava Flow" "River of Fire"
    "Volcanic Updraft" "Rotating Disc" "Escalator"
    "Sinking_Swamp" "Unseen_Force" "Elemental_Wind" "Quicksand"
    "Crowds"))

(defparameter +material-names+
  #("indeterminate"
    "water"
    "fire"
    "shadow"
    "gelatin"
    "light"
    "dreams"
    "*"
    "*"
    "*"
    "paper"             ;; 10
    "papyrus"
    "cardboard"
    "hemp"
    "parchment"
    "*"
    "*"
    "*"
    "*"
    "*"
    "cloth"          ;; 20
    "silk"
    "cotton"
    "polyester"
    "vinyl"
    "wool"
    "satin"
    "denim"
    "carpet"
    "velvet"
    "nylon"            ;; 30
    "canvas"
    "sponge"
    "*"
    "*"
    "*"
    "*"
    "*"
    "*"
    "*"
    "leather"            ;; 40
    "suede"
    "hard leather"
    "skin"
    "fur"
    "scales"
    "hair"
    "ivory"
    "*"
    "*"
    "flesh"          ;; 50
    "bone"
    "tissue"
    "cooked meat"
    "raw meat"
    "cheese"
    "egg"
    "*"
    "*"
    "*"
    "vegetable"      ;; 60
    "leaf"
    "grain"
    "bread"
    "fruit"
    "nut"
    "flower petal"
    "fungus"
    "slime"
    "*"
    "candy"          ;; 70
    "chocolate"
    "pudding"
    "*"
    "*"
    "*"
    "*"
    "*"
    "*"
    "*"
    "wood"               ;; 80
    "oak"
    "pine"
    "maple"
    "birch"
    "mahogony"
    "teak"
    "rattan"
    "ebony"
    "bamboo"
    "*"             ;; 90
    "*"
    "*"
    "*"
    "*"
    "*"
    "*"
    "*"
    "*"
    "*"
    "metal"          ;; 100
    "iron"
    "bronze"
    "steel"
    "copper"
    "brass"
    "silver"
    "gold"
    "platinum"
    "electrum"
    "lead"               ;; 110
    "tin"
    "chrome"
    "aluminum"
    "silicon"
    "titanium"
    "adamantium"
    "cadmium"
    "nickel"
    "mithril"
    "pewter"                  ;; 120
    "plutonium"
    "uranium"
    "rust"
    "orichalcum"
    "*"
    "*"
    "*"
    "*"
    "*"
    "*"              ;; 130
    "*"
    "*"
    "*"
    "*"
    "*"
    "*"
    "*"
    "*"
    "*"
    "plastic"            ;; 140
    "kevlar"
    "rubber"
    "fiberglass"
    "asphalt"
    "concrete"
    "wax"
    "phenolic"
    "latex"
    "enamel"
    "*"              ;; 150
    "*"
    "*"
    "*"
    "*"
    "*"
    "*"
    "*"
    "*"
    "*"
    "glass"          ;; 160
    "crystal"
    "lucite"
    "porcelain"
    "ice"
    "shell"
    "earthenware"
    "pottery"
    "ceramic"
    "*"
    "*"             ;; 170
    "*"
    "*"
    "*"
    "*"
    "*"
    "*"
    "*"
    "*"
    "*"
    "stone"          ;; 180
    "azurite"
    "agate"
    "moss Agate"
    "banded Agate"
    "eye Agate"
    "tiger Eye Agate"
    "quartz"
    "rose Quartz"
    "smoky Quartz"
    "quartz 2"
    "hematite"          ;; 190
    "lapis lazuli"
    "malachite"
    "obsidian"
    "rhodochorsite"
    "tiger eye"
    "turquoise"
    "bloodstone"
    "carnelian"
    "chalcedony"        ;; 200
    "chysoprase"
    "citrine"
    "jasper"
    "moonstone"
    "onyx"
    "zircon"
    "amber"
    "alexandrite"
    "amethyst"
    "oriental amethyst";; 210
    "aquamarine"
    "chrysoberyl"
    "coral"
    "garnet"
    "jade"
    "jet"
    "pearl"
    "peridot"
    "spinel"
    "topaz"         ;; 220
    "oriental topaz"
    "tourmaline"
    "sapphire"
    "black sapphire"
    "star sapphire"
    "ruby"
    "star ruby"
    "opal"
    "fire opal"
    "diamond"           ;; 230
    "sandstone"
    "marble"
    "emerald"
    "mud"
    "clay"
    "labradorite"
    "iolite"
    "spectrolite"
    "charolite"
    "basalt"
    "ash"
    "ink"))

(defparameter +weapon-proficiencies+
  (coerce (list 0
                +skill-prof-whip+
                +skill-prof-whip+
                +skill-prof-slash+
                0
                +skill-prof-pound+
                +skill-prof-crush+
                +skill-prof-pound+
                +skill-prof-slash+
                +skill-prof-crush+
                +skill-prof-whip+
                +skill-prof-pierce+
                +skill-prof-blast+
                0
                +skill-prof-pierce+
                +skill-energy-weapons+
                +skill-prof-slash+
                +skill-prof-slash+
                0 0 0)
          'simple-vector))

(defconstant +mat-none+ 0)
(defconstant +mat-paper+ 10)
(defconstant +mat-cloth+ 20)
(defconstant +mat-leather+ 40)
(defconstant +mat-flesh+ 50)
(defconstant +mat-bone+ 51)
(defconstant +mat-vegetable+ 60)
(defconstant +mat-slime+ 68)
(defconstant +mat-pudding+ 72)
(defconstant +mat-wood+ 80)
(defconstant +mat-metal+ 100)
(defconstant +mat-iron+ 101)
(defconstant +mat-bronze+ 102)
(defconstant +mat-steel+ 103)
(defconstant +mat-tin+ 111)
(defconstant +mat-adamantium+ 116)
(defconstant +mat-mithril+ 119)
(defconstant +mat-rust+ 123)
(defconstant +mat-plastic+ 140)
(defconstant +mat-gold+ 107)
(defconstant +mat-glass+ 160)
(defconstant +mat-stone+ 180)
(defconstant +mat-ash+ 241)
(defconstant +top-material+ 243)

(defparameter +reputation-msg+
  #("Innocent"              ; 0 reputation
    "Mostly Harmless"       ; 1-99
    "Unfriendly"            ; 100-199
    "Unkind"                ; 200-299
    "Cold"                  ; 300-399
    "Daunting"              ; 400-499
    "Feared"                ; 500-599
    "Frightening"           ; 600-699
    "Dreaded"               ; 700-799
    "Terrifying"            ; 800-899
    "Monstrous"             ; 900-999
    "True Killer"))         ; 1000 reputation


(defparameter +exp-scale+
  #(0
    1
    2500
    6150
    11450
    19150        ; 5
    30150
    45650
    67600
    98100
    140500        ; 10
    199500
    281500
    391500
    541000
    746000        ; 15
    1025000
    1400000
    1900000
    2550000
    3400000    ; 20
    4500000
    5900000
    7700000
    10050000
    12950000        ; 25
    16550000
    21050000
    26650000
    33650000
    42350000        ; 30
    52800000
    65300000
    79800000
    96800000
    116500000        ; 35
    140000000
    167000000
    198000000
    233500000
    274500000        ; 40
    320500000
    371500000
    426500000
    486500000
    551000000        ; 45
    622000000
    699000000
    783000000
    869000000
    1000000000        ; 50
    1100000000
    1200000000
    1300000000
    1400000000
    1500000000        ; 55
    1600000000
    1700000000
    1800000000
    1900000000
    2000000000       ; 60
    2000000001
    2000000002
    2000000003
    2000000004
    2000000005
    2000000006
    2000000007
    2000000008
    2000000009
    2000000010
    2000000011      ; 71
    2000000012
    2000000013))

(defparameter +thaco-factor+
  #(0.15                                ; mage
    0.20                                ; cleric
    0.25                                ; thief
    0.30                                ; warrior
    0.40                                ; barb
    0.20                                ; psionic
    0.15                                ; physic
    0.30                                ; cyborg
    0.35                                ; knight
    0.35                                ; ranger
    0.30                                ; bard
    0.40                                ; monk
    0.40                                ; vampire
    0.35                                ; merc
    0.30                                ; spare1
    0.30                                ; spare2
    0.30))                              ; spare3

(defparameter +racial-lifespans+
  #(80                                  ; human
    400                                 ; elf
    160                                 ; dwarf
    55                                  ; half orc
    100                                 ; klingon
    110                                 ; halfling
    80                                  ; tabaxi
    200                                 ; drow
    0                                   ;
    0                                   ;
    0                                   ; mobile
    10000                               ; undead
    100                                 ; humanoid
    50                                  ; animal
    10000                               ; dragon
    500                                 ; giant
    65                                  ; orc
    50                                  ; goblin
    50                                  ; halfling
    200                                 ; minotaur
    500))                               ; troll

(defparameter +attack-hit-text+
  #2A(("hit" "hits")                    ; 0
      ("sting" "stings")
      ("whip" "whips")
      ("slash" "slashes")
      ("bite" "bites")
      ("bludgeon" "bludgeons")          ; 5
      ("crush" "crushes")
      ("pound" "pounds")
      ("claw" "claws")
      ("maul" "mauls")
      ("thrash" "thrashes")             ; 10
      ("pierce" "pierces")
      ("blast" "blasts")
      ("punch" "punches")
      ("stab" "stabs")
      ("zap" "zaps")                    ; Raygun blasting
      ("rip" "rips")
      ("chop" "chops")
      ("shoot" "shoots")))

(defparameter +gun-hit-text+
  #2A(("fire" "fires" "laser beam")
      ("ignite" "ignites" "plasma eruption")
      ("discharge" "discharges" "ionic pulse")
      ("energize" "energizes" "photon burst")
      ("trigger" "triggers" "sonic shock wave")
      ("fire" "fires" "particle stream")
      ("activate" "activates" "gamma ray")
      ("discharge" "discharges" "lightning bolt")
      ("shoot" "shoots" "unknown force")))

(defparameter +attack-types+
  #("hit"                                ; 0
    "sting"
    "whip"
    "slash"
    "bite"
    "bludgeon"                           ; 5
    "crush"
    "pound"
    "claw"
    "maul"
    "thrash"                             ; 10
    "pierce"
    "blast"
    "punch"
    "stab"
    "zap"                                ; Raygun blasting
    "rip"
    "chop"
    "shoot"))


(defparameter +component-names+
  #2A(("NONE" "NONE" "NONE")
      ("primary transformer" "analog converter" "neural net")
      ("spinal cable" "internal gyroscope" "cerebral servo")
      ("hydraulic line" "high speed controller" "interface chip")
      ("receptor enhancer" "receptor enhancer" "receptor enhancer")
      ("primary capacitor" "primary capacitor" "math coprocessor")
      ("cooling pump" "ventilator fan" "ventilation unit")
      ("system coordinator" "parallel processor" "parallel processor")
      ("kinetic drive unit" "kinetic drive unit" "power converter")
      ("hard disk" "hard disk" "hard disk")))