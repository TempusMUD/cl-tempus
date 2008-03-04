(in-package :tempus)

(defparameter +item-light+ 1)           ; Item is a light source
(defparameter +item-scroll+ 2)          ; Item is a scroll
(defparameter +item-wand+ 3)            ; Item is a wand
(defparameter +item-staff+ 4)           ; Item is a staff
(defparameter +item-weapon+ 5)          ; Item is a weapon
(defparameter +item-camera+ 6)          ; Unimplemented
(defparameter +item-missile+ 7)         ; Unimplemented
(defparameter +item-treasure+ 8)        ; Item is a treasure, not gold
(defparameter +item-armor+ 9)           ; Item is armor
(defparameter +item-potion+ 10)         ; Item is a potion
(defparameter +item-worn+ 11)           ; Unimplemented
(defparameter +item-other+ 12)          ; Misc object
(defparameter +item-trash+ 13)          ; Trash - shopkeeps won't buy
(defparameter +item-trap+ 14)           ; Unimplemented
(defparameter +item-container+ 15)      ; Item is a container
(defparameter +item-note+ 16)           ; Item is note
(defparameter +item-drinkcon+ 17)       ; Item is a drink container
(defparameter +item-key+ 18)            ; Item is a key
(defparameter +item-food+ 19)           ; Item is food
(defparameter +item-money+ 20)          ; Item is money (gold)
(defparameter +item-pen+ 21)            ; Item is a pen
(defparameter +item-boat+ 22)           ; Item is a boat
(defparameter +item-fountain+ 23)       ; Item is a fountain
(defparameter +item-wings+ 24)          ; Item allows flying
(defparameter +item-vr-arcade+ 25)
(defparameter +item-scuba-mask+ 26)
(defparameter +item-device+ 27)         ; Activatable device
(defparameter +item-interface+ 28)
(defparameter +item-holy-symb+ 29)
(defparameter +item-vehicle+ 30)
(defparameter +item-engine+ 31)
(defparameter +item-battery+ 32)
(defparameter +item-energy-gun+ 33)
(defparameter +item-window+ 34)
(defparameter +item-portal+ 35)
(defparameter +item-tobacco+ 36)
(defparameter +item-cigarette+ 37)
(defparameter +item-metal+ 38)
(defparameter +item-vstone+ 39)
(defparameter +item-pipe+ 40)
(defparameter +item-transporter+ 41)
(defparameter +item-syringe+ 42)
(defparameter +item-chit+ 43)
(defparameter +item-scuba-tank+ 44)
(defparameter +item-tattoo+ 45)
(defparameter +item-tool+ 46)
(defparameter +item-bomb+ 47)
(defparameter +item-detonator+ 48)
(defparameter +item-fuse+ 49)
(defparameter +item-podium+ 50)
(defparameter +item-pill+ 51)
(defparameter +item-energy-cell+ 52)
(defparameter +item-v-window+ 53)
(defparameter +item-v-door+ 54)
(defparameter +item-v-console+ 55)
(defparameter +item-gun+ 56)
(defparameter +item-bullet+ 57)
(defparameter +item-clip+ 58)
(defparameter +item-microchip+ 59)
(defparameter +item-communicator+ 60)
(defparameter +item-script+ 61)
(defparameter +item-instrument+ 62)
(defparameter +item-book+ 63)
(defparameter +num-item-kinds+ 64)

;;; Instrument Kinds
(defparameter +item-percussion+ 0)
(defparameter +item-string+ 1)
(defparameter +item-wind+ 2)

(defparameter +apply-none+ 0)           ; No effect
(defparameter +apply-str+ 1)            ; Apply to strength
(defparameter +apply-dex+ 2)            ; Apply to dexterity
(defparameter +apply-int+ 3)            ; Apply to intellegence
(defparameter +apply-wis+ 4)            ; Apply to wisdom
(defparameter +apply-con+ 5)            ; Apply to constitution
(defparameter +apply-cha+ 6)            ; Apply to charisma
(defparameter +apply-class+ 7)          ; Reserved
(defparameter +apply-level+ 8)          ; Reserved
(defparameter +apply-age+ 9)            ; Apply to age
(defparameter +apply-char-weight+ 10)	; Apply to weight
(defparameter +apply-char-height+ 11)	; Apply to height
(defparameter +apply-mana+ 12)          ; Apply to max mana
(defparameter +apply-hit+ 13)           ; Apply to max hit points
(defparameter +apply-move+ 14)          ; Apply to max move points
(defparameter +apply-gold+ 15)          ; Reserved
(defparameter +apply-exp+ 16)           ; Reserved
(defparameter +apply-ac+ 17)            ; Apply to Armor Class
(defparameter +apply-hitroll+ 18)       ; Apply to hitroll
(defparameter +apply-damroll+ 19)       ; Apply to damage roll
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
(defparameter +apply-caster+ 44)        ; special usage
(defparameter +apply-weaponspeed+ 45)
(defparameter +apply-disguise+ 46)
(defparameter +apply-nothirst+ 47)
(defparameter +apply-nohunger+ 48)
(defparameter +apply-nodrunk+ 49)
(defparameter +apply-speed+ 50)
(defparameter +num-applies+ 51)

(defclass obj-affected-type ()
  ((location :accessor location-of :initarg :location :initform nil)
   (modifier :accessor modifier-of :initarg :modifier :initform nil)))

(defclass tmp-obj-affect ()
  ((level :accessor level-of :initarg :level)
   (kind :accessor kind-of :initarg :kind)
   (duration :accessor duration-of :initarg :duration)
   (dam-mod :accessor dam-mod-of :initarg :dam-mod)
   (maxdam-mod :accessor maxdam-mod-of :initarg :maxdam-mod)
   (val-mod :accessor val-mod-of :initarg :val-mod)
   (kind-mod :accessor kind-mod-of :initarg :kind-mod)
   (old-kind :accessor old-kind-of :initarg :old-kind)
   (worn-mod :accessor worn-mod-of :initarg :worn-mod)
   (extra-mod :accessor extra-mod-of :initarg :extra-mod)
   (extra-index :accessor extra-index-of :initarg :extra-index)
   (weight-mod :accessor weight-mod-of :initarg :weight-mod)
   (affect-loc :accessor affect-loc-of :initarg :affect-loc)
   (affect-mod :accessor affect-mod-of :initarg :affect-mod)))

(defclass obj-shared-data ()
  ((vnum :accessor vnum-of :initarg :vnum :initform nil)
   (number :accessor number-of :initarg :number :initform nil)
   (cost :accessor cost-of :initarg :cost :initform nil)
   (cost-per-day :accessor cost-per-day-of :initarg :cost-per-day :initform nil)
   (house-count :accessor house-count-of :initarg :house-count :initform nil)
   (owner-id :accessor owner-id-of :initarg :owner-id :initform nil)
   (proto :accessor proto-of :initarg :proto :initform nil)
   (func :accessor func-of :initarg :func :initform nil)
   (func-param :accessor func-param-of :initarg :func-param :initform nil)))

(defclass obj-data ()
  ((shared :accessor shared-of :initarg :shared)
   (in-room :accessor in-room-of :initarg :in-room :initform nil)
   (cur-flow-pulse :accessor cur-flow-pulse-of :initarg :cur-flow-pulse)
   (obj-flags :accessor obj-flags-of :initarg :obj-flags)
   (affected :accessor affected-of :initarg :affected
             :initform (make-array +max-obj-affect+ :initial-element nil))
   (name :accessor name-of :initarg :name)
   (aliases :accessor aliases-of :initarg :aliases)
   (line-desc :accessor line-desc-of :initarg :line-desc)
   (action-desc :accessor action-desc-of :initarg :action-desc)
   (plrtext-len :accessor plrtext-len-of :initarg :plrtext-len)
   (ex-description :accessor ex-description-of :initarg :ex-description :initform nil)
   (carried-by :accessor carried-by-of :initarg :carried-by :initform nil)
   (worn-by :accessor worn-by-of :initarg :worn-by :initform nil)
   (worn-on :accessor worn-on-of :initarg :worn-on)
   (soilage :accessor soilage-of :initarg :soilage)
   (func-data :accessor func-data-of :initarg :func-data)
   (unique-id :accessor unique-id-of :initarg :unique-id)
   (creation-time :accessor creation-time-of :initarg :creation-time)
   (creation-method :accessor creation-method-of :initarg :creation-method)
   (creator :accessor creator-of :initarg :creator)
   (tmp-affects :accessor tmp-affects-of :initarg :tmp-affects)
   (in-obj :accessor in-obj-of :initarg :in-obj :initform nil)
   (contains :accessor contains-of :initarg :contains :initform nil)
   (aux-obj :accessor aux-obj-of :initarg :aux-obj :initform nil)

   ;; Originally from obj_flag_data
   (value :accessor value-of :initarg :value
          :initform (make-array 4 :initial-element nil))
   (kind :accessor kind-of :initarg :kind)
   (wear-flags :accessor wear-flags-of :initarg :wear-flags)
   (extra-flags :accessor extra-flags-of :initarg :extra-flags)
   (extra2-flags :accessor extra2-flags-of :initarg :extra2-flags)
   (extra3-flags :accessor extra3-flags-of :initarg :extra3-flags)
   (weight :accessor weight-of :initarg :weight :initform 0)
   (timer :accessor timer-of :initarg :timer)
   (bitvector :accessor bitvector-of :initarg :bitvector
              :initform (make-array 3 :initial-element 0))
   (material :accessor material-of :initarg :material)
   (max-dam :accessor max-dam-of :initarg :max-dam)
   (damage :accessor damage-of :initarg :damage)
   (sigil-idnum :accessor sigil-idnum-of :initarg :sigil-idnum)
   (sigil-level :accessor sigil-level-of :initarg :sigil-level)))

(defmethod get-weight ((obj obj-data))
  (weight-of obj))

(defmethod set-weight ((obj obj-data) new-weight)
  (modify-weight obj (- new-weight (get-weight obj))))

(defmethod modify-weight ((obj obj-data) mod-weight)
  ;; If object is inside another object, recursively
  ;; go up and modify it
  (cond
    ((in-obj-of obj)
     (modify-weight (in-obj-of obj) mod-weight))
    ((worn-by-of obj)
     (if (eql (aref (implants-of (worn-by-of obj)) (worn-on-of obj)) obj)
         ;; implant, increase character weight
         (modify-weight (worn-by-of obj) mod-weight)
         ;; simply worn, increase character worn weight
         (modify-worn-weight (worn-by-of obj) mod-weight)))
    ((carried-by-of obj)
     (modify-carried-weight (carried-by-of obj) mod-weight)))

  (setf (weight-of obj) (+ (get-weight obj) mod-weight)))

