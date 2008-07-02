(in-package #:tempus)

(defparameter +yes+ 1)
(defparameter +no+ 0)

;;; GAME PLAY OPTIONS

;; minimum level a player must be to shout/holler/gossip/auction
(defparameter +level-can-shout+ 6)

;; number of movement points it costs to holler
(defparameter +holler-move-cost+ 20)

;; exp change limits
(defparameter +max-exp-gain+ 10000000)  ;max gainable per kill
(defparameter +max-exp-loss+ 1000000)   ;max losable per death

;; number of tics (usually 75 seconds) before PC/NPC corpses decompose
(defparameter +max-npc-corpse-time+ 5)
(defparameter +max-pc-corpse-time+ 10)

;; should items in death traps automatically be junked?
(defparameter +dts-are-dumps+ nil)

;; "okay" etc.
(defparameter +ok+ "You got it.~%")
(defparameter +noperson+ "No-one by that name here.~%")
(defparameter +noeffect+ "Nothing seems to happen.~%")

;;;**************************************************************************
;;;**************************************************************************


;;; RENT/CRASHSAVE OPTIONS

;; Should the MUD allow you to 'rent' for free?  (i.e. if you just quit,
;; your objects are saved at no cost, as in Merc-type MUDs.

(defparameter +free-rent+ nil)

;; maximum number of items players are allowed to rent
(defparameter +max-obj-save+ 25)

;; receptionist's surcharge on top of item costs
(defparameter +min-rent-cost+ 5)

;; Should the game automatically save people?  (i.e., save player data
;; every 4 kills (on average), and Crash-save as defined below.

(defparameter +auto-save+ T)

;; if auto_save (above) is yes, how often (in minutes) should the MUD
;; Crash-save people's objects?   Also, this number indicates how often
;; the MUD will Crash-save players' houses.

(defparameter +autosave-time+ 4)

;; Lifetime of crashfiles and forced-rent (idlesave) files in days
(defparameter +crash-file-timeout+ 15)

;; Lifetime of normal rent files in days
(defparameter +rent-file-timeout+ 90)


;;; ROOM NUMBERS

;; vnum number of room that mortals should enter at
(defparameter +mortal-start-room+ 3001)
(defparameter +new-thalos-start-room+ 5505)
(defparameter +kromguard-start-room+ 39188)
(defparameter +electro-start-room+ 30001)
(defparameter +newbie-start-room+ 33800)
(defparameter +elven-start-room+ 19024)
(defparameter +istan-start-room+ 20444)
(defparameter +arena-start-room+ 40000)
(defparameter +tower-modrian-start-room+ 33800)
(defparameter +monk-start-room+ 21007)
(defparameter +solace-start-room+ 63000)
(defparameter +mavernal-start-room+ 59125)
(defparameter +dwarven-caverns-start-room+ 22809)
(defparameter +human-square-start-room+ 22898)
(defparameter +skullport-start-room+ 22873)
(defparameter +drow-isle-start-room+ 22727)
(defparameter +skullport-newbie-start-room+ 23100)
(defparameter +zul-dane-newbie-start-room+ 53306)
(defparameter +zul-dane-start-room+ 53172)
(defparameter +newbie-school-start-room+ 33800)

(defparameter +astral-manse-start-room+ 42500)

;; vnum number of room that immorts should enter at by default
(defparameter +immort-start-room+ 1204)

;; vnum number of room that frozen players should enter at
(defparameter +frozen-start-room+ 1202)

;; vnum numbers of donation rooms.  note: you must change code in
;; do_drop of act.obj1.c if you change the number of non-NOWHERE
;; donation rooms.

(defparameter +donation-room-1+ 3032)
(defparameter +donation-room-2+ 30032)
(defparameter +donation-room-3+ 5510)
(defparameter +donation-room-istan+ 20470)
(defparameter +donation-room-solace+ 63102)
(defparameter +donation-room-skullport-common+ 22942)
(defparameter +donation-room-skullport-dwarven+ 22807)

(defparameter +guild-donation-info+
  '((class-thief all home-skullport 22998)
	(class-thief all home-dwarven-caverns 22998)
	(class-thief all home-human-square 22998)
	(class-thief all home-drow-isle 22998)

	(class-cleric evil home-skullport 22929)
	(class-cleric evil home-dwarven-caverns 22929)
	(class-cleric evil home-human-square 22929)
	(class-cleric evil home-drow-isle 22929)

	(class-vampire evil home-skullport 22995)
	(class-vampire evil home-dwarven-caverns 22995)
	(class-vampire evil home-human-square 22995)
	(class-vampire evil home-drow-isle 22995)

	(class-barb all home-skullport 22993)
	(class-barb all home-dwarven-caverns 22993)
	(class-barb all home-human-square 22993)
	(class-barb all home-drow-isle 22993)

	(class-magic-user all home-skullport 22712)
	(class-magic-user all home-dwarven-caverns 22712)
	(class-magic-user all home-human-square 22712)
	(class-magic-user all home-drow-isle 22712)

	(class-knight evil home-skullport 22992)
	(class-knight evil home-dwarven-caverns 22992)
	(class-knight evil home-human-square 22992)
	(class-knight evil home-drow-isle 22992)

	(class-ranger all home-modrian 2701)
	(class-magic-user all home-modrian 2702)
	(class-thief all home-modrian 2703)
	(class-barb all home-modrian 2704)
	(class-cleric evil home-modrian 2705)
	(class-knight evil home-modrian 2706)
	(class-knight good home-modrian 2707)
	(class-cleric good home-modrian 2708)

	(class-monk all home-monk 21033)

	(class-cyborg all home-electro 30270)
	(class-physic all home-electro 30271)
	(class-monk all home-electro 30272)
	(class-psionic all home-electro 30273)
	(class-mercenary all home-electro 30274)
	(class-cleric evil home-electro 30276)
	(class-thief all home-electro 30277)
	(class-ranger all home-electro 30278)

	(class-ranger all home-solace-cove 63134)
	(class-magic-user all home-solace-cove 63137)
	(class-barb all home-solace-cove 63140)
	(class-knight evil home-solace-cove 63143)
	(class-knight good home-solace-cove 63146)
	(class-cleric good home-solace-cove 63149)
	(class-thief all home-solace-cove 63152)))

;;;**************************************************************************
;;;**************************************************************************


;;; GAME OPERATION OPTIONS

;; default port the game should run on if no port given on command-line
(defparameter +default-port+ 4040)

;; default directory to use as data directory
(defparameter +default-directory+ "lib")

;; maximum number of players allowed before game starts to turn people away
(defparameter +max-players+ 300)

;; maximum size of bug, typo and idea files (to prevent bombing)
(defparameter +max-filesize+ 50000)n

;; maximum number of password attempts before disconnection
(defparameter +max-bad-pws+ 2)

;; Some nameservers are very slow and cause the game to lag terribly every
;; time someone logs in.  The lag is caused by the gethostbyaddr() function
;; which is responsible for resolving numeric IP addresses to alphabetic names.
;; Sometimes, nameservers can be so slow that the incredible lag caused by
;; gethostbyaddr() isn't worth the luxury of having names instead of numbers
;; for players' sitenames.
;;
;; If your nameserver is fast, set the variable below to NO.  If your
;; nameserver is slow, of it you would simply prefer to have numbers
;; instead of names for some other reason, set the variable to YES.
;;
;; You can experiment with the setting of +nameserver-is-slow+ on-line using
;; the SLOWNS command from within the MUD.

(defparameter +nameserver-is-slow+ t)

(defparameter +greetings+
  ".   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

.   .   .  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ .   .   .
          @@@****************************************************@@@
.   .   . @@* ************************************************** *@@.   .   .
          @@* *                     TEMPUS                     * *@@
.   .   . @@* *                                                * *@@.   .   .
          @@* *               the anachronistic                * *@@
.   .   . @@* *             Multiple User Domain               * *@@.   .   .
          @@* *                                                * *@@
.   .   . @@* *                                                * *@@.   .   .
          @@* *     FOUNDATION:  Circle 3.00, Jeremy Elson     * *@@
.   .   . @@* *       A derivative of DikuMUD (GAMMA 0.0)      * *@@.   .   .
          @@* *                                                * *@@
.   .   . @@* *               Powered by Linux                 * *@@.   .   .
          @@* ************************************************** *@@
.   .   . @@@****************************************************@@@.   .   .
           @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
.   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

.   .   .   .   .   .   . Welcome to the Mothership .   .   .   .   .   .   .

.   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

")

(defparameter +welcome-message+ "
Welcome to the realms of Tempus, adventurer.
")

(defparameter +start-message+ 
  "Welcome.  This is your new character in the world of Tempus!
You must be strong to survive, but in time you may become powerful
beyond your wildest dreams...

")
