(in-package :tempus)

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
(defparameter +class-psionic+ 5)	; F
(defparameter +class-physic+ 6)	; F
(defparameter +class-cyborg+ 7)	; F
(defparameter +class-knight+ 8)
(defparameter +class-ranger+ 9)
(defparameter +class-bard+ 10)	; N
(defparameter +class-monk+ 11)
(defparameter +class-vampire+ 12)
(defparameter +class-mercenary+ 13)
(defparameter +class-spare1+ 14)
(defparameter +class-spare2+ 15)
(defparameter +class-spare3+ 16)

(defparameter +num-classes+ 17)	; This must be the number of char_classes!!
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

(defparameter +class-earth+ 81)	; Elementals
(defparameter +class-fire+ 82)
(defparameter +class-water+ 83)
(defparameter +class-air+ 84)
(defparameter +class-lightning+ 85)
(defparameter +class-green+ 91)	; Dragons
(defparameter +class-white+ 92)
(defparameter +class-black+ 93)
(defparameter +class-blue+ 94)
(defparameter +class-red+ 95)
(defparameter +class-silver+ 96)
(defparameter +class-shadow-d+ 97)
(defparameter +class-deep+ 98)
(defparameter +class-turtle+ 99)
(defparameter +class-least+ 101)	; Devils
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
(defparameter +class-slaad-red+ 120)	; Slaad
(defparameter +class-slaad-blue+ 121)
(defparameter +class-slaad-green+ 122)
(defparameter +class-slaad-grey+ 123)
(defparameter +class-slaad-death+ 124)
(defparameter +class-slaad-lord+ 125)
(defparameter +class-demon-i+ 130)	; Demons of the Abyss
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
(defparameter +class-daemon-arcana+ 170)	// daemons
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

;;; Positions
(defparameter +bottom-pos+ 0)
(defparameter +pos-dead+ 0)	; dead
(defparameter +pos-mortallyw+ 1)	; mortally wounded
(defparameter +pos-incap+ 2)	; incapacitated
(defparameter +pos-stunned+ 3)	; stunned
(defparameter +pos-sleeping+ 4)	; sleeping
(defparameter +pos-resting+ 5)	; resting
(defparameter +pos-sitting+ 6)	; sitting
(defparameter +pos-fighting+ 7)	; fighting
(defparameter +pos-standing+ 8)	; standing
(defparameter +pos-flying+ 9)	; flying around
(defparameter +pos-mounted+ 10)
(defparameter +pos-swimming+ 11)
(defparameter +top-pos+ 11)

;;; Player flags: used by Creature.char_specials.act
(defparameter +plr-killer+ (ash 1 0))	; Player is a player-killer
(defparameter +plr-thief+ (ash 1 1))	; Player is a player-thief
(defparameter +plr-frozen+ (ash 1 2))	; Player is frozen
(defparameter +plr-dontset+ (ash 1 3))	; Don't EVER set (ISNPC bit)
(defparameter +plr-writing+ (ash 1 4))	; Player writing (board/mail/olc)
(defparameter +plr-mailing+ (ash 1 5))	; Player is writing mail
(defparameter +plr-crash+ (ash 1 6))	; Player needs to be crash-saved
(defparameter +plr-siteok+ (ash 1 7))	; Player has been site-cleared
(defparameter +plr-noshout+ (ash 1 8))	; Player not allowed to shout/goss
(defparameter +plr-notitle+ (ash 1 9))	; Player not allowed to set title
(defparameter +plr-deleted+ (ash 1 10))	; Player deleted - space reusable
(defparameter +plr-loadroom+ (ash 1 11))	; Player uses nonstandard loadroom
(defparameter +plr-noclanmail+ (ash 1 12))	; Player doesn't get clanmail
(defparameter +plr-nodelete+ (ash 1 13))	; Player shouldn't be deleted
(defparameter +plr-invstart+ (ash 1 14))	; Player should enter game wizinvis
(defparameter +plr-cryo+ (ash 1 15))	; Player is cryo-saved (purge prog)
(defparameter +plr-afk+ (ash 1 16))	; Player is away from keyboard
(defparameter +plr-clan-leader+ (ash 1 17))	; The head of the respective clan
(defparameter +plr-unused2+ (ash 1 18))
(defparameter +plr-olc+ (ash 1 19))	; Player is descripting olc
(defparameter +plr-halt+ (ash 1 20))	; Player is halted
(defparameter +plr-olcgod+ (ash 1 21))	; Player can edit at will
(defparameter +plr-tester+ (ash 1 22))	; Player is a tester
(defparameter +plr-unused3+ (ash 1 23))	; Quest god
(defparameter +plr-mortalized+ (ash 1 24))	; God can be killed
(defparameter +plr-unused4+ (ash 1 25))
(defparameter +plr-unused6+ (ash 1 26))
(defparameter +plr-nopost+ (ash 1 27))
(defparameter +plr-log+ (ash 1 28))	; log all cmds
(defparameter +plr-unused5+ (ash 1 29))	; player approved for port olc
(defparameter +plr-nopk+ (ash 1 30))	; player cannot pk

;; Player Flags Mark II
(defparameter +plr2-soulless+ (ash 1 0))	; Signing the Unholy Compact.
(defparameter +plr2-buried+ (ash 1 1))	; Player has died way too many times.
(defparameter +plr2-in-combat+ (ash 1 2))

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

;;; Affect bits: used in Creature.char_specials.saved.affected_by
;;; WARNING: In the world files, NEVER set the bits marked "R" ("Reserved")
(defparameter +aff-blind+ (ash 1 0))	; (R) Char is blind
(defparameter +aff-invisible+ (ash 1 1))	; Char is invisible
(defparameter +aff-detect-align+ (ash 1 2))	; Char is sensitive to align
(defparameter +aff-detect-invis+ (ash 1 3))	; Char can see invis chars
(defparameter +aff-detect-magic+ (ash 1 4))	; Char is sensitive to magic
(defparameter +aff-sense-life+ (ash 1 5))	; Char can sense hidden life
(defparameter +aff-waterwalk+ (ash 1 6))	; Char can walk on water
(defparameter +aff-sanctuary+ (ash 1 7))	; Char protected by sanct.
(defparameter +aff-group+ (ash 1 8))	; (R) Char is grouped
(defparameter +aff-curse+ (ash 1 9))	; Char is cursed
(defparameter +aff-infravision+ (ash 1 10))	; Char can see in dark
(defparameter +aff-poison+ (ash 1 11))	; (R) Char is poisoned
(defparameter +aff-protect-evil+ (ash 1 12))	; Char protected from evil
(defparameter +aff-protect-good+ (ash 1 13))	; Char protected from good
(defparameter +aff-sleep+ (ash 1 14))	; (R) Char magically asleep
(defparameter +aff-notrack+ (ash 1 15))	; Char can't be tracked
(defparameter +aff-inflight+ (ash 1 16))	; Room for future expansion
(defparameter +aff-time-warp+ (ash 1 17))	; Room for future expansion
(defparameter +aff-sneak+ (ash 1 18))	; Char can move quietly
(defparameter +aff-hide+ (ash 1 19))	; Char is hidden
(defparameter +aff-waterbreath+ (ash 1 20))	; Room for future expansion
(defparameter +aff-charm+ (ash 1 21))	; Char is charmed
(defparameter +aff-confusion+ (ash 1 22))	; Char is confused
(defparameter +aff-nopain+ (ash 1 23))	; Char feels no pain
(defparameter +aff-retina+ (ash 1 24))	; Char's retina is stimulated
(defparameter +aff-adrenaline+ (ash 1 25))	; Char's adrenaline is pumping
(defparameter +aff-confidence+ (ash 1 26))	; Char is confident
(defparameter +aff-rejuv+ (ash 1 27))	; Char is rejuvenating
(defparameter +aff-regen+ (ash 1 28))	; Body is regenerating
(defparameter +aff-glowlight+ (ash 1 29))	; Light spell is operating
(defparameter +aff-blur+ (ash 1 30))	; Blurry image
(defparameter +num-aff-flags+ 31)

(defparameter +aff2-fluorescent+ (ash 1 0))
(defparameter +aff2-transparent+ (ash 1 1))
(defparameter +aff2-slow+ (ash 1 2))
(defparameter +aff2-haste+ (ash 1 3))
(defparameter +aff2-mounted+ (ash 1 4))	;DO NOT SET THIS IN MOB FILE
(defparameter +aff2-fire-shield+ (ash 1 5))	; affected by Fire Shield
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
(defparameter +aff2-telekinesis+ (ash 1 17))	; Char can carry more stuff
(defparameter +aff2-prot-rad+ (ash 1 18))	; Enables Autoexits ! :)
(defparameter +aff2-ablaze+ (ash 1 19))
(defparameter +aff2-neck-protected+ (ash 1 20))	; Can't be beheaded
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
(defparameter +aff3-self-destruct+ (ash 1 5))	; Self-destruct sequence init
(defparameter +aff3-damage-control+ (ash 1 6))	; Damage control for cyborgs
(defparameter +aff3-stasis+ (ash 1 7))	; Borg is in static state
(defparameter +aff3-prismatic-sphere+ (ash 1 8))	; Defensive
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
(defparameter +aff3-hamstrung+ (ash 1 21))	; Bleeding badly from the leg
(defparameter +aff3-gravity-well+ (ash 1 22))	; Pissed off a phyz and got hit by gravity well
(defparameter +aff3-symbol-of-pain+ (ash 1 23))	; Char's mind is burning with pain
(defparameter +aff3-emp-shield+ (ash 1 24))	; EMP SHIELDING
(defparameter +aff3-inst-aff+ (ash 1 25))	; Affected by an instant affect
(defparameter +aff3-tainted+ (ash 1 27))	; Knight spell, "taint"
(defparameter +aff3-infiltrate+ (ash 1 28))	; Merc skill infiltrate
(defparameter +aff3-divine-power+ (ash 1 29))
(defparameter +aff3-mana-leak+ (ash 1 30))
(defparameter +num-aff3-flags+ 31)

(defparameter +array-aff-1+ 1)
(defparameter +array-aff-2+ 2)
(defparameter +array-aff-3+ 3)

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


;;; Modifier constants used with obj affects ('A' fields)
(defparameter +apply-none+ 0)	; No effect
(defparameter +apply-str+ 1)	; Apply to strength
(defparameter +apply-dex+ 2)	; Apply to dexterity
(defparameter +apply-int+ 3)	; Apply to intellegence
(defparameter +apply-wis+ 4)	; Apply to wisdom
(defparameter +apply-con+ 5)	; Apply to constitution
(defparameter +apply-cha+ 6)	; Apply to charisma
(defparameter +apply-class+ 7)	; Reserved
(defparameter +apply-level+ 8)	; Reserved
(defparameter +apply-age+ 9)	; Apply to age
(defparameter +apply-char-weight+ 10)	; Apply to weight
(defparameter +apply-char-height+ 11)	; Apply to height
(defparameter +apply-mana+ 12)	; Apply to max mana
(defparameter +apply-hit+ 13)	; Apply to max hit points
(defparameter +apply-move+ 14)	; Apply to max move points
(defparameter +apply-gold+ 15)	; Reserved
(defparameter +apply-exp+ 16)	; Reserved
(defparameter +apply-ac+ 17)	; Apply to Armor Class
(defparameter +apply-hitroll+ 18)	; Apply to hitroll
(defparameter +apply-damroll+ 19)	; Apply to damage roll
(defparameter +apply-saving-para+ 20)	; Apply to save throw: paralz
(defparameter +apply-saving-rod+ 21)	; Apply to save throw: rods
(defparameter +apply-saving-petri+ 22)	; Apply to save throw: petrif
(defparameter +apply-saving-breath+ 23)	; Apply to save throw: breath
(defparameter +apply-saving-spell+ 24)	; Apply to save throw: spells
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
(defparameter +apply-caster+ 44)	; special usage
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
(defparameter +lvl-fishbone+ 71)
(defparameter +lvl-lucifer+ 70)
(defparameter +lvl-impl+ 69)
(defparameter +lvl-entity+ 68)
(defparameter +lvl-ancient+ +lvl-entity+)
(defparameter +lvl-creator+ 67)
(defparameter +lvl-grgod+ 66)
(defparameter +lvl-timegod+ 65)
(defparameter +lvl-deity+ 64)
(defparameter +lvl-god+ 63)	; Lesser God
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
   (number :accessor number-of :initarg :number :initform 0)
   (attack-type :accessor attack-type-of :initarg :attack-type :initform nil)
   (lair :accessor lair-of :initarg :lair :initform nil)
   (leader :accessor leader-of :initarg :leader :initform nil)
   (kills :accessor kills-of :initarg :kills :initform nil)
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
   (equipment :accessor equipment-of :initarg :equipment :initform nil)
   (implants :accessor implants-of :initarg :implants :initform nil)
   (tattoos :accessor tattoos-of :initarg :tattoos :initform nil)
   (carrying :accessor carrying-of :initarg :carrying :initform nil)
   (link :accessor link-of :initform nil)
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
   (aliases :accessor aliases-of :initarg :aliases :initform nil)
   (short-descr :accessor short-descr-of :initarg :short-descr :initform nil)
   (long-descr :accessor long-descr-of :initarg :long-descr :initform nil)
   (description :accessor description-of :initarg :description :initform nil)
   (char-class :accessor char-class-of :initarg :char-class :initform nil)
   (remort-char-class :accessor remort-char-class-of :initarg :remort-char-class :initform nil)
   (weight :accessor weight-of :initarg :weight :initform nil)
   (height :accessor height-of :initarg :height :initform nil)
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
   (skills :accessor skills-of :initarg :skills :initform (make-array 1000 :element-type '(integer 0 127)))
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
   (plr-bits :accessor plr-bits-of :initarg :plr-bits :initform nil)
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
                 :short-descr (short-descr-of proto)
                 :long-descr (long-descr-of proto)
                 :description (description-of proto)
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

(defun immortalp (ch)
  (>= (level-of ch) 50))

(defun noncorporealp (ch)
  nil)

(defun check-skill (ch skill)
  (aref (skills-of ch) skill))

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

(defun is-npc (ch) (typep ch 'mobile))
(defun is-cleric (ch) (or (eql (char-class-of ch) +class-cleric+)
                          (eql (remort-char-class-of ch) +class-cleric+)))

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
       (short-descr-of tch))
      ((can-detect-disguise ch tch (duration-of af))
       (format nil "~a (disguised as ~a)"
               (short-descr-of tch)
               (short-descr-of mob)))
      (t
       (short-descr-of mob)))))
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

(defun affected-by-spell (ch spell)
  nil)