(in-package :tempus)

(defparameter +zone-autosave+ (ash 1 0))
(defparameter +zone-resetsave+ (ash 1 1))
(defparameter +zone-notifyowner+ (ash 1 2))
(defparameter +zone-locked+ (ash 1 3))
(defparameter +zone-nomagic+ (ash 1 4))
(defparameter +zone-nolaw+ (ash 1 5))
(defparameter +zone-noweather+ (ash 1 6))
(defparameter +zone-nocrime+ (ash 1 7))
(defparameter +zone-frozen+ (ash 1 8))
(defparameter +zone-isolated+ (ash 1 9))
(defparameter +zone-soundproof+ (ash 1 10))
(defparameter +zone-noidle+ (ash 1 11))
(defparameter +zone-fullcontrol+ (ash 1 12))
(defparameter +zone-paused+ (ash 1 13))
(defparameter +zone-evil-ambience+ (ash 1 14))
(defparameter +zone-good-ambience+ (ash 1 15))
(defparameter +zone-norecalc+ (ash 1 16))

(defparameter +zone-search-approved+ (ash 1 19))
(defparameter +zone-mobs-approved+ (ash 1 20))
(defparameter +zone-objs-approved+ (ash 1 21))
(defparameter +zone-rooms-approved+ (ash 1 22))
(defparameter +zone-zcmds-approved+ (ash 1 23))
(defparameter +zone-inplay+ (ash 1 24))
(defparameter +zone-mobs-modified+ (ash 1 25))
(defparameter +zone-objs-modified+ (ash 1 26))
(defparameter +zone-rooms-modified+ (ash 1 27))
(defparameter +zone-zone-modified+ (ash 1 28))

(defun zone-flagged (zone bit)
  (logtest (flags-of zone) bit))

(defun zone-approvedp (zone)
  (not
   (or (zone-flagged zone +zone-mobs-approved+)
       (zone-flagged zone +zone-rooms-approved+)
       (zone-flagged zone +zone-objs-approved+)
       (zone-flagged zone +zone-search-approved+)
       (zone-flagged zone +zone-zcmds-approved+))))

(defparameter +plane-prime-1+ 0)
(defparameter +plane-prime-2+ 1)
(defparameter +plane-neverwhere+ 2)
(defparameter +plane-underdark+ 3)
(defparameter +plane-western+ 4)
(defparameter +plane-morbidian+ 5)
(defparameter +max-prime-plane+ 9)
(defparameter +plane-astral+ 10)
(defparameter +plane-hell-1+ 11)
(defparameter +plane-hell-2+ 12)
(defparameter +plane-hell-3+ 13)
(defparameter +plane-hell-4+ 14)
(defparameter +plane-hell-5+ 15)
(defparameter +plane-hell-6+ 16)
(defparameter +plane-hell-7+ 17)
(defparameter +plane-hell-8+ 18)
(defparameter +plane-hell-9+ 19)
(defparameter +plane-ghenna+ 20)
(defparameter +plane-abyss+ 25)
(defparameter +plane-olc+ 39)
(defparameter +plane-olympus+ 40)
(defparameter +plane-costal+ 41)
(defparameter +plane-heaven+ 43)
(defparameter +plane-doom+ 50)
(defparameter +plane-shadow+ 51)
(defparameter +plane-elem-water+ 70)
(defparameter +plane-elem-fire+ 71)
(defparameter +plane-elem-air+ 72)
(defparameter +plane-elem-earth+ 73)
(defparameter +plane-elem-pos+ 74)
(defparameter +plane-elem-neg+ 75)
(defparameter +plane-pelem-magma+ 76)
(defparameter +plane-pelem-ooze+ 77)
(defparameter +plane-pelem-ice+ 78)
(defparameter +plane-pelem-smoke+ 79)
(defparameter +plane-elysium+ 80)

(defparameter +time-timeless+ 0)
(defparameter +time-modrian+ 1)
(defparameter +time-electro+ 2)
(defparameter +time-past+ +time-modrian+)
(defparameter +time-future+ +time-electro+)

(defparameter +zone-no-pk+ 0)
(defparameter +zone-neutral-pk+ 1)
(defparameter +zone-chaotic-pk+ 2)

(defclass reset-com ()
  ((command :accessor command-of :initarg :command :initform nil)
   (if-flag :accessor if-flag-of :initarg :if-flag :initform nil)
   (arg1 :accessor arg1-of :initarg :arg1 :initform nil)
   (arg2 :accessor arg2-of :initarg :arg2 :initform nil)
   (arg3 :accessor arg3-of :initarg :arg3 :initform nil)
   (line :accessor line-of :initarg :line :initform nil)
   (prob :accessor prob-of :initarg :prob :initform nil)))

(defclass weather-data ()
  ((pressure :accessor pressure-of :initarg :pressure :initform 0)
   (change :accessor change-of :initarg :change :initform 0)
   (sky :accessor sky-of :initarg :sky :initform 0)
   (sunlight :accessor sunlight-of :initarg :sunlight :initform 0)
   (moonlight :accessor moonlight-of :initarg :moonlight :initform 0)
   (temp :accessor temp-of :initarg :temp :initform 0)
   (humid :accessor humid-of :initarg :humid :initform 0)))

(defclass zone-data ()
  ((name :accessor name-of :initarg :name :initform nil)
   (lifespan :accessor lifespan-of :initarg :lifespan :initform 30)
   (age :accessor age-of :initarg :age :initform 0)
   (top :accessor top-of :initarg :top)
   (respawn-pt :accessor respawn-pt-of :initarg :respawn-pt :initform nil)
   (reset-mode :accessor reset-mode-of :initarg :reset-mode :initform 0)
   (number :accessor number-of :initarg :number)
   (time-frame :accessor time-frame-of :initarg :time-frame :initform 0)
   (plane :accessor plane-of :initarg :plane :initform +plane-olc+)
   (owner-idnum :accessor owner-idnum-of :initarg :owner-idnum :initform nil)
   (co-owner-idnum :accessor co-owner-idnum-of :initarg :co-owner-idnum :initform nil)
   (author :accessor author-of :initarg :author :initform nil)
   (enter-count :accessor enter-count-of :initarg :enter-count :initform 0)
   (flags :accessor flags-of :initarg :flags :initform (logior +zone-mobs-approved+
                                                               +zone-objs-approved+
                                                               +zone-rooms-approved+
                                                               +zone-zcmds-approved+
                                                               +zone-search-approved+))
   (hour-mod :accessor hour-mod-of :initarg :hour-mod :initform 0)
   (year-mod :accessor year-mod-of :initarg :year-mod :initform 0)
   (latitude :accessor latitude-of :initarg :latitude :initform nil)
   (min-lvl :accessor min-lvl-of :initarg :min-lvl :initform nil)
   (min-gen :accessor min-gen-of :initarg :min-gen :initform nil)
   (max-lvl :accessor max-lvl-of :initarg :max-lvl :initform nil)
   (max-gen :accessor max-gen-of :initarg :max-gen :initform nil)
   (pk-style :accessor pk-style-of :initarg :pk-style :initform nil)
   (public-desc :accessor public-desc-of :initarg :public-desc :initform nil)
   (private-desc :accessor private-desc-of :initarg :private-desc :initform nil)
   (num-players :accessor num-players-of :initarg :num-players :initform 0)
   (idle-time :accessor idle-time-of :initarg :idle-time :initform 0)
   (world :accessor world-of :initarg :world :initform nil)
   (cmds :accessor cmds-of :initarg :cmds :initform nil)
   (weather :accessor weather-of :initarg :weather :initform nil)))

(defmethod print-object ((cmd reset-com) stream)
  (print-unreadable-object (cmd stream :type t)
    (format stream "~a ~a ~a ~a ~a ~a%"
            (if-flag-of cmd)
            (command-of cmd)
            (arg1-of cmd)
            (arg2-of cmd)
            (arg3-of cmd)
            (prob-of cmd))))

(defmethod print-object ((zone zone-data) stream)
  (print-unreadable-object (zone stream :type t)
    (format stream "~a ~s"
            (number-of zone)
            (name-of zone))))

(defun copy-reset-cmd (cmd)
  (make-instance 'reset-com
                 :command (command-of cmd)
                 :if-flag (if-flag-of cmd)
                 :arg1 (arg1-of cmd)
                 :arg2 (arg2-of cmd)
                 :arg3 (arg3-of cmd)
                 :prob (prob-of cmd)))

(defun copy-zone (zone)
  (make-instance 'zone-data
                 :name (name-of zone)
                 :lifespan (lifespan-of zone)
                 :age (age-of zone)
                 :top (top-of zone)
                 :respawn-pt (respawn-pt-of zone)
                 :reset-mode (reset-mode-of zone)
                 :number (number-of zone)
                 :time-frame (time-frame-of zone)
                 :plane (plane-of zone)
                 :owner-idnum (owner-idnum-of zone)
                 :co-owner-idnum (co-owner-idnum-of zone)
                 :author (author-of zone)
                 :enter-count (enter-count-of zone)
                 :flags (flags-of zone)
                 :hour-mod (hour-mod-of zone)
                 :year-mod (year-mod-of zone)
                 :latitude (latitude-of zone)
                 :min-lvl (min-lvl-of zone)
                 :min-gen (min-gen-of zone)
                 :max-lvl (max-lvl-of zone)
                 :max-gen (max-gen-of zone)
                 :pk-style (pk-style-of zone)
                 :public-desc (public-desc-of zone)
                 :private-desc (private-desc-of zone)
                 :num-players (num-players-of zone)
                 :idle-time (idle-time-of zone)
                 :world (copy-list (world-of zone))
                 :cmds (mapcar 'copy-reset-cmd (cmds-of zone))
                 :weather (make-instance 'weather-data
                                         :pressure (pressure-of (weather-of zone))
                                         :change (change-of (weather-of zone))
                                         :sky (sky-of (weather-of zone))
                                         :sunlight (sunlight-of (weather-of zone))
                                         :moonlight (moonlight-of (weather-of zone))
                                         :temp (temp-of (weather-of zone))
                                         :humid (humid-of (weather-of zone)))))
