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

(defparameter +dirs+ #("north" "east" "south" "west" "up" "down" "future" "past"))
(defparameter +to-dirs+ #("north" "east" "south" "west" "up" "down" "into the future" "into the past"))
(defparameter +from-dirs+ #("the south" "the west" "the north" "the east" "below" "above" "the past" "the future"))
(defparameter +num-of-dirs+ (length +dirs+))

(defparameter +room-bits+
  #("DRK" "DTH" "!MOB" "IND" "NV" "SDPF" "!TRK" "!MAG" "TNL" "!TEL"
    "GDR" "HAS" "HCR" "COMFORT" "SMOKE" "!FLEE" "!PSI" "!SCI" "!RCL"
    "CLAN" "ARENA" "DOCK" "BURN" "FREEZ" "NULLMAG" "HOLYO" "RAD"
    "SLEEP" "EXPLOD" "POISON" "VACUUM" "\n"))

(defparameter +sector-types+
  #("Inside" "City" "Field" "Forest" "Hills" "Mountains"
    "Water (Swim)" "Water (No Swim)" "Underwater" "Open Air" "Notime"
    "Climbing" "Outer Space" "Road" "Vehicle" "Farmland" "Swamp"
    "Desert" "Fire River" "Jungle" "Pitch Surface" "Pitch Submerged"
    "Beach" "Astral" "Elemental Fire" "Elemental Earth" "Elemental Air"
    "Elemental Water" "Elemental Positive" "Elemental Negative"
    "Elemental Smoke" "Elemental Ice" "Elemental Ooze"
    "Elemental Magma" "Elemental Lightning" "Elemental Steam"
    "Elemental Radiance" "Elemental Minerals" "Elemental Vacuum"
    "Elemental Salt" "Elemental Ash" "Elemental Dust" "Blood" "Rock"
    "Muddy" "Trail" "Tundra" "Catacombs" "Cracked Road" "Deep Ocean"))

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
(defparameter +spell-reserved-dbc+ 0)	; SKILL NUMBER ZERO -- RESERVED

;; Special mobile vnums
(defparameter +unholy-stalker-vnum+ 1513)
(defparameter +zombie-vnum+ 1512)

;; Special object vnums
(defparameter +quad-vnum+ 1578)

;;; PLAYER SPELLS -- Numbered from 1 to MAX_SPELLS

(defparameter +spell-armor+ 1)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-teleport+ 2)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-bless+ 3)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-blindness+ 4)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-burning-hands+ 5)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-call-lightning+ 6)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-charm+ 7)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-chill-touch+ 8)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-clone+ 9)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-color-spray+ 10)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-control-weather+ 11)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-create-food+ 12)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-create-water+ 13)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-cure-blind+ 14)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-cure-critic+ 15)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-cure-light+ 16)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-curse+ 17)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-detect-align+ 18)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-detect-invis+ 19)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-detect-magic+ 20)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-detect-poison+ 21)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-dispel-evil+ 22)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-earthquake+ 23)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-enchant-weapon+ 24)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-energy-drain+ 25)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-fireball+ 26)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-harm+ 27)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-heal+ 28)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-invisible+ 29)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-lightning-bolt+ 30)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-locate-object+ 31)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-magic-missile+ 32)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-poison+ 33)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-prot-from-evil+ 34)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-remove-curse+ 35)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-sanctuary+ 36)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-shocking-grasp+ 37)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-sleep+ 38)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-strength+ 39)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-summon+ 40)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-ventriloquate+ 41)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-word-of-recall+ 42)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-remove-poison+ 43)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-sense-life+ 44)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-animate-dead+ 45)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-dispel-good+ 46)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-group-armor+ 47)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-group-heal+ 48)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-group-recall+ 49)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-infravision+ 50)	; Reserved Skill[] DO NOT CHANGE
(defparameter +spell-waterwalk+ 51)	; Reserved Skill[] DO NOT CHANGE
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
(defparameter +spell-shield-of-righteousness+ 129)	; group protection
(defparameter +spell-blackmantle+ 130)	; blocks healing spells
(defparameter +spell-sanctification+ 131)	; 2x dam vs. evil
(defparameter +spell-stigmata+ 132)	; causes a bleeding wound
(defparameter +spell-summon-legion+ 133)	; knights summon devils
(defparameter +spell-entangle+ 134)	; rangers entangle in veg.
(defparameter +spell-anti-magic-shell+ 135)
(defparameter +spell-divine-intervention+ 136)
(defparameter +spell-sphere-of-desecration+ 137)
(defparameter +spell-malefic-violation+ 138)	; cuts thru good sanct
(defparameter +spell-righteous-penetration+ 139)	; cuts thru evil sanct
(defparameter +spell-unholy-stalker+ 140)	; evil cleric hunter mob
(defparameter +spell-inferno+ 141)	; evil cleric room affect
(defparameter +spell-vampiric-regeneration+ 142)	; evil cleric vamp. regen
(defparameter +spell-banishment+ 143)	; evil cleric sends devils away
(defparameter +spell-control-undead+ 144)	; evil clerics charm undead
(defparameter +spell-stoneskin+ 145)	; remort rangers stone skin
(defparameter +spell-sun-ray+ 146)	; Good cleric remort,
											 ; destroys undead.
(defparameter +spell-taint+ 147)	; Evil knight remort spell, taint.
(defparameter +spell-locust-regeneration+ 148)	; Mage remort skill, drains mana
(defparameter +spell-divine-power+ 149)	; Good cleric remort skill.
(defparameter +spell-death-knell+ 150)	; Evil cleric remort skill.
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
(defparameter +spell-power+ 201)	; Strength
(defparameter +spell-intellect+ 202)
(defparameter +spell-confusion+ 203)
(defparameter +spell-fear+ 204)
(defparameter +spell-satiation+ 205)	; fills hunger
(defparameter +spell-quench+ 206)	; fills thirst
(defparameter +spell-confidence+ 207)	; sets nopain
(defparameter +spell-nopain+ 208)
(defparameter +spell-dermal-hardening+ 209)
(defparameter +spell-wound-closure+ 210)
(defparameter +spell-antibody+ 211)
(defparameter +spell-retina+ 212)
(defparameter +spell-adrenaline+ 213)
(defparameter +spell-breathing-stasis+ 214)
(defparameter +spell-vertigo+ 215)
(defparameter +spell-metabolism+ 216)	; Increased healing, hunger, thirst
(defparameter +spell-ego-whip+ 217)
(defparameter +spell-psychic-crush+ 218)
(defparameter +spell-relaxation+ 219)	; speeds mana regen, weakens char
(defparameter +spell-weakness+ 220)	; minus str
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
(defparameter +spell-amnesia+ 235)	; psi remorts
(defparameter +spell-nullpsi+ 236)	; remove psi affects
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
(defparameter +spell-random-coordinates+ 309)	; random teleport
(defparameter +spell-repulsion-field+ 310)
(defparameter +spell-transmittance+ 311)	; transparency
(defparameter +spell-spacetime-imprint+ 312)	; sets room as teleport spot
(defparameter +spell-spacetime-recall+ 313)	; teleports to imprint telep spot
(defparameter +spell-time-warp+ 314)	; random teleport into other time
(defparameter +spell-tidal-spacewarp+ 315)	; fly
(defparameter +spell-fission-blast+ 316)	; full-room damage
(defparameter +spell-refraction+ 317)	; like displacement
(defparameter +spell-electroshield+ 318)	; prot_lightning
(defparameter +spell-vacuum-shroud+ 319)	; eliminates breathing and fire
(defparameter +spell-densify+ 320)	; increase weight of obj & char
(defparameter +spell-chemical-stability+ 321)	; prevent/stop acidity
(defparameter +spell-entropy-field+ 322)	; drains move on victim (time effect)
(defparameter +spell-gravity-well+ 323)	; time effect crushing damage
(defparameter +spell-capacitance-boost+ 324)	; increase maxmv
(defparameter +spell-electric-arc+ 325)	; lightning bolt
(defparameter +spell-sonic-boom+ 326)	; area damage + wait state
(defparameter +spell-lattice-hardening+ 327)	; dermal hard or increase obj maxdam
(defparameter +spell-nullify+ 328)	; like dispel magic
(defparameter +spell-force-wall+ 329)	; sets up an exit blocker
(defparameter +spell-unused-330+ 330)
(defparameter +spell-phasing+ 331)	; invuln.
(defparameter +spell-absorption-shield+ 332)	; works like mana shield
(defparameter +spell-temporal-compression+ 333)	; works like haste
(defparameter +spell-temporal-dilation+ 334)	; works like slow
(defparameter +spell-gauss-shield+ 335)	; half damage from metal
(defparameter +spell-albedo-shield+ 336)	; reflects e/m attacks
(defparameter +spell-thermostatic-field+ 337)	; sets prot_heat + end_cold
(defparameter +spell-radioimmunity+ 338)	; sets prot_rad
(defparameter +spell-transdimensionality+ 339)	; randomly teleport to another plane
(defparameter +spell-area-stasis+ 340)	; sets !phy room flag
(defparameter +spell-electrostatic-field+ 341)	; protective static field does damage to attackers
(defparameter +spell-emp-pulse+ 342)	; Shuts off devices, communicators
										; deactivats all cyborg programs
										; blocked by emp shield
(defparameter +spell-quantum-rift+ 343)	; Shuts off devices, communicators
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
(defparameter +song-lullaby+ 352) ; puts a room to sleep
(defparameter +song-verse-of-vulnerability+ 353) ; lowers AC of target
(defparameter +song-exposure-overture+ 354) ; Area affect, causes targets to vis
(defparameter +song-verse-of-vibration+ 355) ; motor spasm++
(defparameter +song-regalers-rhapsody+ 356) ; caster and groupies get satiated
(defparameter +song-melody-of-mettle+ 357) ; caster and group get con and maxhit
(defparameter +song-lustration-melisma+ 358) ; caster and group cleansed of blindness, poison, sickness
(defparameter +song-defense-ditty+ 359) ; save spell, psi, psy based on gen
(defparameter +song-alrons-aria+ 360) ; singer/group confidence
(defparameter +song-song-shield+ 361) ; self only, like psi block
(defparameter +song-verse-of-valor+ 362) ; self/group increase hitroll
(defparameter +song-hymn-of-peace+ 363) ; stops fighting in room, counters req of rage
(defparameter +song-song-of-silence+ 364) ; Area, disallow speaking, casting. singing
(defparameter +song-drifters-ditty+ 365) ; self/group increases move
(defparameter +song-unravelling-diapason+ 366) ;dispel magic
(defparameter +song-rhapsody-of-depression+ 367) ; Area, slow all but grouped
(defparameter +song-chant-of-light+ 368) ; group, light and prot_cold
(defparameter +song-aria-of-asylum+ 369) ; self/group/target up to 25 percent dam reduction
(defparameter +song-white-noise+ 370) ; single target, confusion
(defparameter +song-rhythm-of-rage+ 371) ; self only, berserk, counter = hymn of peace
(defparameter +song-power-overture+ 372) ; self only, increase strength and hitroll
(defparameter +song-guiharias-glory+ 373) ; self/group, + damroll
(defparameter +song-sirens-song+ 374) ; single target, charm
(defparameter +song-sonic-disruption+ 375) ; area, medium damage
(defparameter +song-mirror-image-melody+ 376) ; causes multiple images of the singer
(defparameter +song-clarifying-harmonies+ 377) ; identify
(defparameter +song-unladen-swallow-song+ 378) ; group flight
(defparameter +song-irresistable-dance+ 379) ; Target, -hitroll
(defparameter +song-rhythm-of-alarm+ 380) ; room affect, notifies the bard of person entering room
(defparameter +song-rhapsody-of-remedy+ 381) ; self/target, heal
(defparameter +song-shatter+ 382) ; target; damage persons/objects, penetrate WALL O SOUND
(defparameter +song-home-sweet-home+ 383) ; recall
(defparameter +song-weight-of-the-world+ 384) ; self/group/target, like telekinesis
(defparameter +song-purple-haze+ 385) ; area, pauses fighting for short time
(defparameter +song-wounding-whispers+ 386) ; self, like blade barrier
(defparameter +song-dirge+ 387) ; area, high damage
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
(defparameter +skill-backstab+ 501)	; Reserved Skill[] DO NOT CHANGE
(defparameter +skill-bash+ 502)	; Reserved Skill[] DO NOT CHANGE
(defparameter +skill-hide+ 503)	; Reserved Skill[] DO NOT CHANGE
(defparameter +skill-kick+ 504)	; Reserved Skill[] DO NOT CHANGE
(defparameter +skill-pick-lock+ 505)	; Reserved Skill[] DO NOT CHANGE
(defparameter +skill-punch+ 506)	; Reserved Skill[] DO NOT CHANGE
(defparameter +skill-rescue+ 507)	; Reserved Skill[] DO NOT CHANGE
(defparameter +skill-sneak+ 508)	; Reserved Skill[] DO NOT CHANGE
(defparameter +skill-steal+ 509)	; Reserved Skill[] DO NOT CHANGE
(defparameter +skill-track+ 510)	; Reserved Skill[] DO NOT CHANGE
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
(defparameter +skill-transmute+ 585)	; physic transmute objs
(defparameter +skill-metalworking+ 586)
(defparameter +skill-leatherworking+ 587)
(defparameter +skill-demolitions+ 588)
(defparameter +skill-psiblast+ 589)
(defparameter +skill-psilocate+ 590)
(defparameter +skill-psidrain+ 591)	; drain mana from vict
(defparameter +skill-gunsmithing+ 592)	; repair gunz
(defparameter +skill-elusion+ 593)	; !track
(defparameter +skill-pistolwhip+ 594)
(defparameter +skill-crossface+ 595)	; rifle whip
(defparameter +skill-wrench+ 596)	; break neck
(defparameter +skill-cry-from-beyond+ 597)
(defparameter +skill-kia+ 598)
(defparameter +skill-wormhole+ 599)	; physic's wormhole
(defparameter +skill-lecture+ 600)	; physic's boring-ass lecture

(defparameter +skill-turn+ 601)	; Cleric's turn
(defparameter +skill-analyze+ 602)	; Physic's analysis
(defparameter +skill-evaluate+ 603)	; Physic's evaluation
(defparameter +skill-holy-touch+ 604)	; Knight's skill
(defparameter +skill-night-vision+ 605)
(defparameter +skill-empower+ 606)
(defparameter +skill-swimming+ 607)
(defparameter +skill-throwing+ 608)
(defparameter +skill-riding+ 609)
(defparameter +skill-pipemaking+ 610)	;Make a pipe!
(defparameter +skill-charge+ 611)	; BANG!
(defparameter +skill-counter-attack+ 612)


;;; ****************  CYBORG SKILLS  *******************
(defparameter +skill-reconfigure+ 613)	; Re-allocate stats
(defparameter +skill-reboot+ 614)	; Start over from scratch
(defparameter +skill-motion-sensor+ 615)	; Detect Entries into room
(defparameter +skill-stasis+ 616)	; State of rapid healing
(defparameter +skill-energy-field+ 617)	; Protective field
(defparameter +skill-reflex-boost+ 618)	; Speeds up processes
(defparameter +skill-power-boost+ 619)	; Increases Strength
(defparameter +skill-unused-1+ 620)	;
(defparameter +skill-fastboot+ 621)	; Reboots are faster
(defparameter +skill-self-destruct+ 622)	; Effective self destructs
(defparameter +skill-unused-2+ 623)	;
(defparameter +skill-bioscan+ 624)	; Sense Life scan
(defparameter +skill-discharge+ 625)	; Discharge attack
(defparameter +skill-selfrepair+ 626)	; Repair hit points
(defparameter +skill-cyborepair+ 627)	; Repair other borgs
(defparameter +skill-overhaul+ 628)	; Overhaul other borgs
(defparameter +skill-damage-control+ 629)	; Damage Control System
(defparameter +skill-electronics+ 630)	; Operation of Electronics
(defparameter +skill-hacking+ 631)	; hack electronic systems
(defparameter +skill-cyberscan+ 632)	; scan others for implants
(defparameter +skill-cybo-surgery+ 633)	; implant objects
(defparameter +skill-energy-weapons+ 634)	; energy weapon use
(defparameter +skill-proj-weapons+ 635)	; projectile weapon use
(defparameter +skill-speed-loading+ 636)	; speed load weapons
(defparameter +skill-hyperscan+ 637)	; aware of hidden objs and traps
(defparameter +skill-overdrain+ 638)	; overdrain batteries
(defparameter +skill-de-energize+ 639)	; drain energy from chars
(defparameter +skill-assimilate+ 640)	; assimilate objects
(defparameter +skill-radionegation+ 641)	; immunity to radiation
(defparameter +skill-implant-w+ 642)	; Extra attacks with implant weapons.
(defparameter +skill-adv-implant-w+ 643)	; ""
(defparameter +skill-offensive-pos+ 644)	; Offensive Posturing
(defparameter +skill-defensive-pos+ 645)	; Defensive Posturing
(defparameter +skill-melee-combat-tac+ 646)	; Melee combat tactics
(defparameter +skill-neural-bridging+ 647)	; Cogenic Neural Bridging
										; (Ambidextarity)
;;; Cyborg skills continue around 675

(defparameter +skill-retreat+ 648)	; controlled flee
(defparameter +skill-disguise+ 649)	; look like a mob
(defparameter +skill-ambush+ 650)	; surprise victim

(defparameter +skill-chemistry+ 651) ; merc skill
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
(defparameter +skill-snipe+ 669)	; sniper skill for mercs
(defparameter +skill-infiltrate+ 670)	; merc skill, improvement on sneak
(defparameter +skill-shoulder-throw+ 671)	; grounding skill between hiptoss
										 ; and sweepkick

;;; Bard Skills
(defparameter +skill-scream+ 672) ; damage like psiblast, chance to stun
(defparameter +skill-ventriloquism+ 673) ; makes objects talk
(defparameter +skill-tumbling+ 674) ; like uncanny dodge
(defparameter +skill-lingering-song+ 676) ; increases duration of song affects

;;; Overflow Cyborg
(defparameter +skill-nanite-reconstruction+ 675)	; repairs implants
;;; static const int SKILL_ARTERIAL_FLOW = 676;	; Arterial Flow Enhancements
(defparameter +skill-optimmunal-resp+ 677)	; Genetek Optimmunal Node
(defparameter +skill-adrenal-maximizer+ 678)	; Shukutei Adrenal Maximizer

(defparameter +skill-energy-conversion+ 679)	; physic's energy conversion

  ;*****************  PROFICENCIES  ******************
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
(defparameter +skill-use-wands+ 691)


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
(defparameter +type-malovent-holytouch+ 767)	; When holytouch wears off.
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


(defparameter +type-crushing-depth+ 892)	; in deep ocean without vehicle
(defparameter +type-taint-burn+ 893)	; casting while tainted
(defparameter +type-pressure+ 894)
(defparameter +type-suffocating+ 895)
(defparameter +type-anguish+ 896)	; Soulless and good aligned. dumbass.
(defparameter +type-bleed+ 897)	; Open wound
(defparameter +type-overload+ 898)	; cyborg overloading systems.
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
(defparameter +tar-self-only+ 32)	; Only a check, use with i.e. TAR_CHAR_ROOM
(defparameter +tar-not-self+ 64)	; Only a check, use with i.e. TAR_CHAR_ROOM
(defparameter +tar-obj-inv+ 128)
(defparameter +tar-obj-room+ 256)
(defparameter +tar-obj-world+ 512)
(defparameter +tar-obj-equip+ 1024)
(defparameter +tar-door+ 2048)
(defparameter +tar-unpleasant+ 4096)
(defparameter +tar-dir+ 8192)

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
(defparameter +num-wears+ 27)	; This must be the # of eq positions!!
(defparameter +wear-random+ 28)
(defparameter +wear-mshield+ 29) ; This is for mana shield messages just increase it if new wear positions are added

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



(defparameter +player-races+
  #("Human"
	"Elf"
	"Dwarf"
	"Half Orc"
	"Klingon"
	"Halfling"					; 5
	"Tabaxi"
	"Drow"
	"ILL" "ILL"
	"Mobile"					; 10
	"Undead"
	"Humanoid"
	"Animal"
	"Dragon"
	"Giant"					; 15
	"Orc"
	"Goblin"
	"Hafling"
	"Minotaur"
	"Troll"					; 20
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
	"Illithid"					; 40
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
	"Spider"                  ; 52
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

;;; Take/Wear flags: used by obj_data.obj_flags.wear_flags
(defparameter +item-wear-take+ (ash 1 0))	; Item can be takes
(defparameter +item-wear-finger+ (ash 1 1))	; Can be worn on finger
(defparameter +item-wear-neck+ (ash 1 2))	; Can be worn around neck
(defparameter +item-wear-body+ (ash 1 3))	; Can be worn on body
(defparameter +item-wear-head+ (ash 1 4))	; Can be worn on head
(defparameter +item-wear-legs+ (ash 1 5))	; Can be worn on legs
(defparameter +item-wear-feet+ (ash 1 6))	; Can be worn on feet
(defparameter +item-wear-hands+ (ash 1 7))	; Can be worn on hands
(defparameter +item-wear-arms+ (ash 1 8))	; Can be worn on arms
(defparameter +item-wear-shield+ (ash 1 9))	; Can be used as a shield
(defparameter +item-wear-about+ (ash 1 10))	; Can be worn about body
(defparameter +item-wear-waist+ (ash 1 11))	; Can be worn around waist
(defparameter +item-wear-wrist+ (ash 1 12))	; Can be worn on wrist
(defparameter +item-wear-wield+ (ash 1 13))	; Can be wielded
(defparameter +item-wear-hold+ (ash 1 14))	; Can be held
(defparameter +item-wear-crotch+ (ash 1 15))	; guess where
(defparameter +item-wear-eyes+ (ash 1 16))	; eyes
(defparameter +item-wear-back+ (ash 1 17))	;Worn on back
(defparameter +item-wear-belt+ (ash 1 18))	; Worn on a belt(ie, pouch)
(defparameter +item-wear-face+ (ash 1 19))
(defparameter +item-wear-ear+ (ash 1 20))
(defparameter +item-wear-ass+ (ash 1 21))	;Can be RAMMED up an asshole
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