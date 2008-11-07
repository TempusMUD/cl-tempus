(in-package :tempus)

(defconstant +north+ 0)
(defconstant +east+ 1)
(defconstant +south+ 2)
(defconstant +west+ 3)
(defconstant +up+ 4)
(defconstant +down+ 5)
(defconstant +future+ 6)
(defconstant +past+ 7)
(defconstant +num-dirs+ 8)

(defconstant +door-open+ (ash 1 0))
(defconstant +door-closed+ (ash 1 1))
(defconstant +door-locked+ (ash 1 2))
(defconstant +door-hidden+ (ash 1 3))

(defconstant +room-dark+              (ash 1 0))
(defconstant +room-death+             (ash 1 1))
(defconstant +room-nomob+             (ash 1 2))
(defconstant +room-indoors+           (ash 1 3))
(defconstant +room-peaceful+          (ash 1 4))
(defconstant +room-soundproof+        (ash 1 5))
(defconstant +room-notrack+           (ash 1 6))
(defconstant +room-nomagic+           (ash 1 7))
(defconstant +room-tunnel+            (ash 1 8))
(defconstant +room-notel+             (ash 1 9))
(defconstant +room-godroom+           (ash 1 10))
(defconstant +room-house+             (ash 1 11))
(defconstant +room-house-crash+       (ash 1 12))
(defconstant +room-comfort+           (ash 1 13))
(defconstant +room-smoke-filled+      (ash 1 14))
(defconstant +room-noflee+            (ash 1 15))
(defconstant +room-nopsionics+        (ash 1 16))
(defconstant +room-noscience+         (ash 1 17))
(defconstant +room-nophysic+          +room-noscience+)
(defconstant +room-norecall+          (ash 1 18))
(defconstant +room-clan-house+        (ash 1 19))
(defconstant +room-arena+             (ash 1 20))
(defconstant +room-dock+              (ash 1 21))
(defconstant +room-flame-filled+      (ash 1 22))
(defconstant +room-ice-cold+          (ash 1 23))
(defconstant +room-null-magic+        (ash 1 24))
(defconstant +room-holyocean+         (ash 1 25))
(defconstant +room-radioactive+       (ash 1 26))
(defconstant +room-sleep-gas+         (ash 1 27))
(defconstant +room-explosive-gas+     (ash 1 28))
(defconstant +room-poison-gas+        (ash 1 29))
(defconstant +room-vacuum+            (ash 1 30))
(defconstant +num-room-flags+         32)

(defconstant +ex-isdoor+              (ash 1 0))
(defconstant +ex-closed+              (ash 1 1))
(defconstant +ex-locked+              (ash 1 2))
(defconstant +ex-pickproof+           (ash 1 3))
(defconstant +ex-heavy-door+          (ash 1 4))
(defconstant +ex-hard-pick+           (ash 1 5))
(defconstant +ex-nomob+               (ash 1 6))
(defconstant +ex-hidden+              (ash 1 7))
(defconstant +ex-noscan+              (ash 1 8))
(defconstant +ex-tech+                (ash 1 9))
(defconstant +ex-oneway+              (ash 1 10))
(defconstant +ex-nopass+              (ash 1 11))
(defconstant +ex-wall-thorns+         (ash 1 12))
(defconstant +ex-wall-thorns-nopass+  (ash 1 13))
(defconstant +ex-wall-stone+          (ash 1 14))
(defconstant +ex-wall-ice+            (ash 1 15))
(defconstant +ex-wall-fire+           (ash 1 16))
(defconstant +ex-wall-fire-nopass+    (ash 1 17))
(defconstant +ex-wall-flesh+          (ash 1 18))
(defconstant +ex-wall-iron+           (ash 1 19))
(defconstant +ex-wall-energy-f+       (ash 1 20))
(defconstant +ex-wall-energy-f-nopass+(ash 1 21))
(defconstant +ex-wall-force+          (ash 1 22))
(defconstant +ex-special+             (ash 1 23))
(defconstant +ex-reinforced+          (ash 1 24))
(defconstant +ex-secret+              (ash 1 25))
(defconstant +num-doorflags+          26)

(defconstant +sect-inside+                0) ; Indoors
(defconstant +sect-city+                  1) ; In a city
(defconstant +sect-field+                 2) ; In a field
(defconstant +sect-forest+                3) ; In a forest
(defconstant +sect-hills+                 4) ; In the hills
(defconstant +sect-mountain+              5) ; On a mountain
(defconstant +sect-water-swim+            6) ; Swimmable water
(defconstant +sect-water-noswim+          7) ; Water - need a boat
(defconstant +sect-underwater+            8) ; Underwater
(defconstant +sect-flying+                9) ; Wheee!
(defconstant +sect-notime+               10) ; Between Times
(defconstant +sect-climbing+             11) ; Climbing skill required
(defconstant +sect-freespace+            12) ; Out of the atmosphere
(defconstant +sect-road+                 13) ; On the road
(defconstant +sect-vehicle+              14) ; In a car
(defconstant +sect-farmland+             15) ; In the corn
(defconstant +sect-swamp+                16) ; Swamp
(defconstant +sect-desert+               17) ; Sandy and hot
(defconstant +sect-fire-river+           18)
(defconstant +sect-jungle+               19)
(defconstant +sect-pitch-pit+            20)
(defconstant +sect-pitch-sub+            21)
(defconstant +sect-beach+                22)
(defconstant +sect-astral+               23)
(defconstant +sect-elemental-fire+       24)
(defconstant +sect-elemental-earth+      25)
(defconstant +sect-elemental-air+        26)
(defconstant +sect-elemental-water+      27)
(defconstant +sect-elemental-positive+   28)
(defconstant +sect-elemental-negative+   29)
(defconstant +sect-elemental-smoke+      30)
(defconstant +sect-elemental-ice+        31)
(defconstant +sect-elemental-ooze+       32)
(defconstant +sect-elemental-magma+      33)
(defconstant +sect-elemental-lightning+  34)
(defconstant +sect-elemental-steam+      35)
(defconstant +sect-elemental-radiance+   36)
(defconstant +sect-elemental-minerals+   37)
(defconstant +sect-elemental-vacuum+     38)
(defconstant +sect-elemental-salt+       39)
(defconstant +sect-elemental-ash+        40)
(defconstant +sect-elemental-dust+       41)
(defconstant +sect-blood+                42)
(defconstant +sect-rock+                 43)
(defconstant +sect-muddy+                44)
 (defconstant +sect-trail+                45)
(defconstant +sect-tundra+               46)
(defconstant +sect-catacombs+            47)
(defconstant +sect-cracked-road+         48)
(defconstant +sect-deep-ocean+           49)
(defconstant +num-sect-types+            50)

;;; Room-related structures

(defclass room-direction-data ()
  ((description :accessor description-of :initarg :description :initform nil)
   (keyword :accessor keyword-of :initarg :keyword :initform nil)
   (exit-info :accessor exit-info-of :initarg :exit-info :initform 0)
   (key :accessor key-of :initarg :key :initform -1)
   (to-room :accessor to-room-of :initarg :to-room :initform nil)))

(defclass room-affect-data ()
  ((description :accessor description-of :initarg :description :initform nil)
   (level :accessor level-of :initarg :level :initform nil)
   (flags :accessor flags-of :initarg :flags :initform nil)
   (kind :accessor kind-of :initarg :kind :initform nil)
   (duration :accessor duration-of :initarg :duration :initform nil)
   (val :accessor val-of :initarg :val-of :initform nil)
   (owner :accessor owner-of :initarg :owner :initform nil)
   (spell-kind :accessor spell-kind-of :initarg :spell-kind :initform nil)))

(defconstant +trail-exit+ 0)
(defconstant +trail-enter+ 0)

(defconstant +trail-flag-bloodprints+ (ash 1 0))
(defconstant +trail-flag-blood-drops+ (ash 1 1))

(defclass room-trail-data ()
  ((name :accessor name-of :initarg :name :initform nil)
   (aliases :accessor aliases-of :initarg :aliases :initform nil)
   (time :accessor time-of :initarg :time :initform nil)
   (idnum :accessor idnum-of :initarg :idnum :initform nil)
   (from-dir :accessor from-dir-of :initarg :from-dir :initform nil)
   (to-dir :accessor to-dir-of :initarg :to-dir :initform nil)
   (track :accessor track-of :initarg :track :initform nil)
   (flags :accessor flags-of :initarg :flags :initform nil)))

;;; Memory structure for room
(defclass room-data ()
  ((number :accessor number-of :initarg :number :initform nil)
   (terrain :accessor terrain-of :initarg :terrain :initform nil)
   (name :accessor name-of :initarg :name :initform nil)
   (description :accessor description-of :initarg :description :initform nil)
   (sounds :accessor sounds-of :initarg :sounds :initform nil)
   (prog-text :accessor prog-text-of :initarg :prog :initform nil)
   (progobj :accessor progobj-of :initarg :progobj :initform nil)
   (progobj-len :accessor progobj-len-of :initarg :progobj-len :initform nil)
   (prog-marker :accessor prog-marker-of :initarg :prog-marker :initform nil)
   (prog-state :accessor prog-state-of :initarg :prog-state :initform nil)
   (ex-description :accessor ex-description-of :initarg :ex-description :initform nil)
   (dir-option :accessor dir-option-of :initarg :dir-option :initform (make-array +num-dirs+ :initial-element nil))
   (searches :accessor searches-of :initarg :search :initform nil)
   (affects :accessor affects-of :initarg :affects :initform nil)
   (trail :accessor trail-of :initarg :trail :initform nil)
   (flags :accessor flags-of :initarg :room-flags :initform nil)
   (find-path-index :accessor find-path-index-of :initform 0)
   (max-occupancy :accessor max-occupancy-of :initarg :max-occupancy :initform nil)
   (light :accessor light-of :initarg :light :initform 0)
   (flow-dir :accessor flow-dir-of :initarg :flow-dir :initform nil)
   (flow-speed :accessor flow-speed-of :initarg :flow-speed :initform nil)
   (flow-kind :accessor flow-kind-of :initarg :flow-kind :initform nil)
   (func :accessor func-of :initarg :func :initform nil)
   (func-param :accessor func-param-of :initarg :func-param :initform nil)
   (zone :accessor zone-of :initarg :zone :initform nil)
   (contents :accessor contents-of :initarg :contents :initform nil)
   (people :accessor people-of :initarg :people :initform nil)))

(defmethod print-object ((room room-data) stream)
  (print-unreadable-object (room stream :type t)
    (format stream "~a ~s" (number-of room) (name-of room))))

(defun room-flagged (room flag)
  (logtest flag (flags-of room)))

(defun room-is-underwater (room)
  (let ((terrain (terrain-of room)))
    (or (= terrain +sect-underwater+)
        (= terrain +sect-elemental-water+)
        (= terrain +sect-deep-ocean+))))

(defun room-is-open-air (room)
  (let ((terrain (terrain-of room)))
    (or (= terrain +sect-flying+)
        (= terrain +sect-elemental-air+)
        (= terrain +sect-elemental-radiance+)
        (= terrain +sect-elemental-lightning+)
        (= terrain +sect-elemental-vacuum+))))

(defun can-enter-room (ch room)
  t)
