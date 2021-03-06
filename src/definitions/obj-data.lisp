(in-package :tempus)

(defparameter +blood-vnum+ 1579)
(defparameter +ice-vnum+ 1580)
(defparameter +rustpile-vnum+ 15284)

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
(defparameter +apply-char-weight+ 10)   ; Apply to weight
(defparameter +apply-char-height+ 11)   ; Apply to height
(defparameter +apply-mana+ 12)          ; Apply to max mana
(defparameter +apply-hit+ 13)           ; Apply to max hit points
(defparameter +apply-move+ 14)          ; Apply to max move points
(defparameter +apply-gold+ 15)          ; Reserved
(defparameter +apply-exp+ 16)           ; Reserved
(defparameter +apply-ac+ 17)            ; Apply to Armor Class
(defparameter +apply-hitroll+ 18)       ; Apply to hitroll
(defparameter +apply-damroll+ 19)       ; Apply to damage roll
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
(defparameter +apply-caster+ 44)        ; special usage
(defparameter +apply-weaponspeed+ 45)
(defparameter +apply-disguise+ 46)
(defparameter +apply-nothirst+ 47)
(defparameter +apply-nohunger+ 48)
(defparameter +apply-nodrunk+ 49)
(defparameter +apply-speed+ 50)
(defparameter +num-applies+ 51)

;;; Extra object flags: used by obj_data.obj_flags.extra_flags
(defparameter +item-glow+ (ash 1 0))    ; Item is glowing
(defparameter +item-hum+ (ash 1 1)) ; Item is humming
(defparameter +item-norent+ (ash 1 2))  ; Item cannot be rented
(defparameter +item-nodonate+ (ash 1 3))    ; Item cannot be donated
(defparameter +item-noinvis+ (ash 1 4)) ; Item cannot be made invis
(defparameter +item-invisible+ (ash 1 5))   ; Item is invisible
(defparameter +item-magic+ (ash 1 6))   ; Item is magical
(defparameter +item-nodrop+ (ash 1 7))  ; Item is cursed: can't drop
(defparameter +item-bless+ (ash 1 8))   ; Item is blessed
(defparameter +item-anti-good+ (ash 1 9))   ; Not usable by good people
(defparameter +item-anti-evil+ (ash 1 10))  ; Not usable by evil people
(defparameter +item-anti-neutral+ (ash 1 11))   ; Not usable by neutral people
(defparameter +item-anti-magic-user+ (ash 1 12))    ; Not usable by mages
(defparameter +item-anti-cleric+ (ash 1 13))    ; Not usable by clerics
(defparameter +item-anti-thief+ (ash 1 14)) ; Not usable by thieves
(defparameter +item-anti-warrior+ (ash 1 15))   ; Not usable by warriors
(defparameter +item-nosell+ (ash 1 16)) ; Shopkeepers won't touch it
(defparameter +item-anti-barb+ (ash 1 17))  ; no barb
(defparameter +item-anti-psychic+ (ash 1 18))   ; no psychic
(defparameter +item-anti-physic+ (ash 1 19))    ; no physic
(defparameter +item-anti-cyborg+ (ash 1 20))
(defparameter +item-anti-knight+ (ash 1 21))
(defparameter +item-anti-ranger+ (ash 1 22))
(defparameter +item-anti-bard+ (ash 1 23))
(defparameter +item-anti-monk+ (ash 1 24))
(defparameter +item-blurred+ (ash 1 25))
(defparameter +item-magic-nodispel+ (ash 1 26))
(defparameter +item-unused+ (ash 1 27))
(defparameter +item-repulsion-field+ (ash 1 28))
(defparameter +item-transparent+ (ash 1 29))
(defparameter +item-damned+ (ash 1 30)) ; Evil equivalent to Bless
(defparameter +num-extra-flags+ 31)

(defparameter +item2-radioactive+ (ash 1 0))
(defparameter +item2-anti-merc+ (ash 1 1))
(defparameter +item2-anti-spare1+ (ash 1 2))
(defparameter +item2-anti-spare2+ (ash 1 3))
(defparameter +item2-anti-spare3+ (ash 1 4))
(defparameter +item2-hidden+ (ash 1 5))
(defparameter +item2-trapped+ (ash 1 6))
(defparameter +item2-singular+ (ash 1 7))
(defparameter +item2-nolocate+ (ash 1 8))
(defparameter +item2-nosoil+ (ash 1 9))
(defparameter +item2-cast-weapon+ (ash 1 10))
(defparameter +item2-two-handed+ (ash 1 11))
(defparameter +item2-body-part+ (ash 1 12))
(defparameter +item2-ablaze+ (ash 1 13))
(defparameter +item2-cursed-perm+ (ash 1 14))
(defparameter +item2-noremove+ (ash 1 15))
(defparameter +item2-thrown-weapon+ (ash 1 16))
(defparameter +item2-godeq+ (ash 1 17))
(defparameter +item2-no-mort+ (ash 1 18))
(defparameter +item2-broken+ (ash 1 19))
(defparameter +item2-implant+ (ash 1 20))
(defparameter +item2-reinforced+ (ash 1 21))
(defparameter +item2-enhanced+ (ash 1 22))
(defparameter +item2-req-mort+ (ash 1 23))
(defparameter +item2-protected-hunt+ (ash 1 28))
(defparameter +item2-renamed+ (ash 1 29))
(defparameter +item2-unapproved+ (ash 1 30))
(defparameter +num-extra2-flags+ 31)

;;; extra3 flags

(defparameter +item3-req-mage+ (ash 1 0))
(defparameter +item3-req-cleric+ (ash 1 1))
(defparameter +item3-req-thief+ (ash 1 2))
(defparameter +item3-req-warrior+ (ash 1 3))
(defparameter +item3-req-barb+ (ash 1 4))
(defparameter +item3-req-psionic+ (ash 1 5))
(defparameter +item3-req-physic+ (ash 1 6))
(defparameter +item3-req-cyborg+ (ash 1 7))
(defparameter +item3-req-knight+ (ash 1 8))
(defparameter +item3-req-ranger+ (ash 1 9))
(defparameter +item3-req-bard+ (ash 1 10))
(defparameter +item3-req-monk+ (ash 1 11))
(defparameter +item3-req-vampire+ (ash 1 12))
(defparameter +item3-req-mercenary+ (ash 1 13))
(defparameter +item3-req-spare1+ (ash 1 14))
(defparameter +item3-req-spare2+ (ash 1 15))
(defparameter +item3-req-spare3+ (ash 1 16))
(defparameter +item3-lattice-hardened+ (ash 1 17))
(defparameter +item3-stay-zone+ (ash 1 18))
(defparameter +item3-hunted+ (ash 1 19))
(defparameter +item3-nomag+ (ash 1 20))
(defparameter +item3-nosci+ (ash 1 21))
(defparameter +num-extra3-flags+ 22)

(defparameter +cont-closeable+ (ash 1 0))
(defparameter +cont-pickproof+ (ash 1 1))
(defparameter +cont-closed+ (ash 1 2))
(defparameter +cont-locked+ (ash 1 3))

(defparameter +interface-none+ 0)
(defparameter +interface-power+ 1)
(defparameter +interface-chips+ 2)
(defparameter +num-interfaces+ 3)

(defparameter +chip-none+ 0)
(defparameter +chip-skill+ 1)
(defparameter +chip-affects+ 2)
(defparameter +num-chips+ 3)

(defparameter +gun-none+ 0)
(defparameter +gun-22-cal+ 1)
(defparameter +gun-223-cal+ 2)
(defparameter +gun-25-cal+ 3)
(defparameter +gun-30-cal+ 4)
(defparameter +gun-357-cal+ 5)
(defparameter +gun-38-cal+ 6)
(defparameter +gun-40-cal+ 7)
(defparameter +gun-44-cal+ 8)
(defparameter +gun-45-cal+ 9)
(defparameter +gun-50-cal+ 10)
(defparameter +gun-555-cal+ 11)
(defparameter +gun-nailgun+ 12)
(defparameter +gun-410+ 13)
(defparameter +gun-20-gauge+ 14)
(defparameter +gun-16-gauge+ 15)
(defparameter +gun-12-gauge+ 16)
(defparameter +gun-10-gauge+ 17)
(defparameter +gun-7-mm+ 18)
(defparameter +gun-762-mm+ 19)
(defparameter +gun-9-mm+ 20)
(defparameter +gun-10-mm+ 21)
(defparameter +gun-7mm-mag+ 22)
(defparameter +gun-bow+ 25)
(defparameter +gun-xbow+ 26)
(defparameter +gun-flamethrower+ 30)
(defparameter +num-gun-types+ 31)

(defparameter +egun-laser+ 0)
(defparameter +egun-plasma+ 1)
(defparameter +egun-ion+ 2)
(defparameter +egun-photon+ 3)
(defparameter +egun-sonic+ 4)
(defparameter +egun-particle+ 5)
(defparameter +egun-gamma+ 6)
(defparameter +egun-lightning+ 7)
(defparameter +egun-top+ 8)

(defclass obj-affected-type ()
  ((location :accessor location-of :initarg :location :initform 0)
   (modifier :accessor modifier-of :initarg :modifier :initform 0)))

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
  ((vnum :accessor vnum-of :initarg :vnum :initform -1)
   (number :accessor number-of :initarg :number :initform 0)
   (cost :accessor cost-of :initarg :cost :initform 0)
   (cost-per-day :accessor cost-per-day-of :initarg :cost-per-day :initform 0)
   (house-count :accessor house-count-of :initarg :house-count :initform 0)
   (owner-id :accessor owner-id-of :initarg :owner-id :initform 0)
   (proto :accessor proto-of :initarg :proto :initform nil)
   (func :accessor func-of :initarg :func :initform nil)
   (func-param :accessor func-param-of :initarg :func-param :initform nil)))

(defclass obj-data ()
  ((shared :accessor shared-of :initarg :shared)
   (in-room :accessor in-room-of :initarg :in-room :initform nil)
   (flow-pulse :accessor flow-pulse-of :initform 0)
   (affected :accessor affected-of :initarg :affected
             :initform (make-array +max-obj-affect+))
   (name :accessor name-of :initarg :name :initform nil)
   (aliases :accessor aliases-of :initarg :aliases :initform nil)
   (line-desc :accessor line-desc-of :initarg :line-desc :initform nil)
   (action-desc :accessor action-desc-of :initarg :action-desc :initform nil)
   (plrtext-len :accessor plrtext-len-of :initarg :plrtext-len :initform 0)
   (ex-description :accessor ex-description-of :initarg :ex-description :initform nil)
   (carried-by :accessor carried-by-of :initarg :carried-by :initform nil)
   (worn-by :accessor worn-by-of :initarg :worn-by :initform nil)
   (worn-on :accessor worn-on-of :initarg :worn-on :initform 0)
   (soilage :accessor soilage-of :initarg :soilage :initform 0)
   (func-data :accessor func-data-of :initarg :func-data :initform nil)
   (unique-id :accessor unique-id-of :initarg :unique-id :initform 0)
   (creation-time :accessor creation-time-of :initarg :creation-time :initform (now))
   (creation-method :accessor creation-method-of :initarg :creation-method :initform nil)
   (creator :accessor creator-of :initarg :creator :initform nil)
   (tmp-affects :accessor tmp-affects-of :initarg :tmp-affects :initform nil)
   (in-obj :accessor in-obj-of :initarg :in-obj :initform nil)
   (contains :accessor contains-of :initarg :contains :initform nil)
   (aux-obj :accessor aux-obj-of :initarg :aux-obj :initform nil)

   ;; Originally from obj_flag_data
   (value :accessor value-of :initarg :value
          :initform (make-array 4 :initial-element 0))
   (kind :accessor kind-of :initarg :kind :initform 0)
   (wear-flags :accessor wear-flags-of :initarg :wear-flags :initform 0)
   (extra-flags :accessor extra-flags-of :initarg :extra-flags :initform 0)
   (extra2-flags :accessor extra2-flags-of :initarg :extra2-flags :initform 0)
   (extra3-flags :accessor extra3-flags-of :initarg :extra3-flags :initform 0)
   (weight :accessor weight-of :initarg :weight :initform 0)
   (timer :accessor timer-of :initarg :timer :initform -1)
   (bitvector :accessor bitvector-of :initarg :bitvector
              :initform (make-array 3 :initial-element 0))
   (material :accessor material-of :initarg :material :initform 0)
   (max-dam :accessor max-dam-of :initarg :max-dam :initform 0)
   (damage :accessor damage-of :initarg :damage :initform 0)
   (sigil-idnum :accessor sigil-idnum-of :initarg :sigil-idnum :initform 0)
   (sigil-level :accessor sigil-level-of :initarg :sigil-level :initform 0)))

(defun make-object (creation-method creator
                    &key
                    vnum
                    (name "")
                    (aliases "")
                    (line-desc "")
                    (action-desc "")
                    (value0 0)
                    (value1 0)
                    (value2 0)
                    (value3 0)
                    (kind 0)
                    (wear-flags 0)
                    (extra-flags 0)
                    (extra2-flags 0)
                    (extra3-flags 0)
                    (weight 0)
                    (timer 0)
                    (bitvector0 0)
                    (bitvector1 0)
                    (bitvector2 0)
                    (material 0)
                    (max-dam 0)
                    (damage 0))
  (let ((obj (make-instance 'obj-data
                            :name name
                            :aliases aliases
                            :line-desc line-desc
                            :action-desc action-desc
                            :kind kind
                            :wear-flags wear-flags
                            :extra-flags extra-flags
                            :extra2-flags extra2-flags
                            :extra3-flags extra3-flags
                            :weight weight
                            :timer timer
                            :material material
                            :max-dam max-dam
                            :damage damage
                            :creation-time (now)
                            :creation-method creation-method
                            :creator creator
                            :shared (if vnum
                                        (shared-of (gethash vnum *object-prototypes*))
                                        +null-obj-shared+))))
    (dotimes (j +max-obj-affect+)
      (setf (aref (affected-of obj) j)
            (make-instance 'obj-affected-type
                           :location +apply-none+
                           :modifier 0)))
    (setf (aref (value-of obj) 0) value0)
    (setf (aref (value-of obj) 1) value1)
    (setf (aref (value-of obj) 2) value2)
    (setf (aref (value-of obj) 3) value3)
    (setf (aref (bitvector-of obj) 0) bitvector0)
    (setf (aref (bitvector-of obj) 1) bitvector1)
    (setf (aref (bitvector-of obj) 2) bitvector2)

    (incf *top-unique-id*)
    (setf (unique-id-of obj) *top-unique-id*)

    obj))

(defun clone-object-proto (proto)
  (let ((obj (make-instance 'obj-data
                 :shared (shared-of proto)
                 :name (name-of proto)
                 :aliases (aliases-of proto)
                 :line-desc (line-desc-of proto)
                 :action-desc (action-desc-of proto)
                 :plrtext-len (plrtext-len-of proto)
                 :ex-description (ex-description-of proto)
                 :worn-on (worn-on-of proto)
                 :soilage (soilage-of proto)
                 :creation-time (now)
                 :creation-method 'unknown
                 :creator 0
                 :value (make-array 4 :initial-contents (value-of proto))
                 :affected (make-array +max-obj-affect+)
                 :kind (kind-of proto)
                 :wear-flags (wear-flags-of proto)
                 :extra-flags (extra-flags-of proto)
                 :extra2-flags (extra2-flags-of proto)
                 :extra3-flags (extra3-flags-of proto)
                 :weight (weight-of proto)
                 :timer (timer-of proto)
                 :bitvector (make-array 3 :initial-contents (bitvector-of proto))
                 :material (material-of proto)
                 :max-dam (max-dam-of proto)
                 :damage (damage-of proto)
                 :sigil-idnum (sigil-idnum-of proto)
                 :sigil-level (sigil-level-of proto))))
    (dotimes (j +max-obj-affect+)
      (let ((proto-af (aref (affected-of proto) j)))
        (setf (aref (affected-of obj) j)
              (make-instance 'obj-affected-type
                             :location (location-of proto-af)
                             :modifier (modifier-of proto-af)))))
    obj))

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

(defmethod vnum-of ((obj obj-data))
  (vnum-of (shared-of obj)))

(defmethod cost-of ((obj obj-data))
  (cost-of (shared-of obj)))

(defmethod cost-per-day-of ((obj obj-data))
  (cost-per-day-of (shared-of obj)))

(defmethod obj-val-of ((obj obj-data) num)
  (aref (value-of obj) num))

(defmethod (setf obj-val-of) (value (obj obj-data) num)
  (setf (aref (value-of obj) num) value))


(defun is-obj-kind (obj type)
  (= (kind-of obj) type))

(defun is-obj-stat (obj flag)
  (logtest (extra-flags-of obj) flag))
(defun is-obj-stat2 (obj flag)
  (logtest (extra2-flags-of obj) flag))
(defun is-obj-stat3 (obj flag)
  (logtest (extra3-flags-of obj) flag))

(defun obj-soiled (obj soil)
  (logtest (soilage-of obj) soil))

(defun is-two-hand (obj)
  (is-obj-stat2 obj +item2-two-handed+))
(defun is-corpse (obj)
  (and (= (kind-of obj) +item-container+)
       (plusp (aref (value-of obj) 3))))
(defun is-implant (obj)
  (is-obj-stat2 obj +item2-implant+))
(defun is-any-gun (obj)
  (or (is-obj-kind obj +item-gun+)
      (is-obj-kind obj +item-energy-gun+)))
(defun is-soilage (obj)
  (or (= (vnum-of (shared-of obj)) +blood-vnum+)
      (= (vnum-of (shared-of obj)) +ice-vnum+)))
(defun same-obj (obj-a obj-b)
  (and (= (vnum-of (shared-of obj-a)) (vnum-of (shared-of obj-b)))
       (string= (name-of obj-a) (name-of obj-b))))
(defun can-wear (obj pos)
  (logtest (wear-flags-of obj) pos))

(defun unrentable? (obj)
  "Returns T if OBJ cannot be rented by a player."
  (or
   (is-obj-stat obj +item-norent+)
   (not (approvedp obj))
   (minusp (vnum-of (shared-of obj)))
   (and (is-obj-kind obj +item-key+)
        (zerop (obj-val-of obj 1)))
   (and (is-obj-kind obj +item-cigarette+)
        (not (zerop (obj-val-of obj 3))))))

(defun hidden-obj-prob (ch obj)
  (+ (level-of ch)
     (int-of ch)
     (if (affected-by-spell ch +skill-hyperscan+) 40 0)
     (if (aff3-flagged ch +aff3-sonic-imagery+) 30 0)
     (if (aff2-flagged ch +aff2-true-seeing+) 60 0)
     (if (pref-flagged ch +pref-holylight+) 500 0)
     (if (is-obj-stat obj +item-glow+) -20 0)
     (if (is-obj-stat obj +item-hum+) -20 0)
     (if (and (is-cigarette obj) (smoke-lit obj)) 10 0)
     (if (and (is-obj-stat obj +item-magic+)
              (aff-flagged ch +aff-detect-magic+)) 20 0)))

(defun is-cigarette (obj)
  (= (kind-of obj) +item-cigarette+))
(defun is-interface (obj)
  (= (kind-of obj) +item-interface+))
(defun is-chip (obj)
  (= (kind-of obj) +item-microchip+))

(defun destroyedp (obj)
  (minusp (damage-of obj)))

(defmethod approvedp ((obj obj-data))
  "Returns T if the object has been approved."
  (not (is-obj-stat2 obj +item2-unapproved+)))

;;; Vehicles
(defun key-number (car)
  (aref (value-of car) 0))
(defun door-state (car)
  (aref (value-of car) 1))
(defun room-number (car)
  (aref (value-of car) 0))
(defun car-flags (car)
  (aref (value-of car) 2))
(defun car-special (car)
  (aref (value-of car) 3))
(defun max-energy (engine)
  (aref (value-of engine) 0))
(defun cur-energy (engine)
  (aref (value-of engine) 1))
(defun (setf cur-energy) (val engine)
  (setf (aref (value-of engine) 1) val))
(defun engine-state (engine)
  (aref (value-of engine) 2))
(defun use-rate (engine)
  (aref (value-of engine) 3))
(defun car-openable (car)
  (logtest (door-state car) +cont-closeable+))
(defun car-closed (car)
  (logtest (door-state car) +cont-closed+))
(defun is-vehicle (obj)
  (= (kind-of obj) +item-vehicle+))

;;; Devices
(defun is-device (obj)
  (= (kind-of obj) +item-device+))

;;; Chip interfaces
(defun interface-type (obj)
  (aref (value-of obj) 0))
(defun interface-max (obj)
  (aref (value-of obj) 2))
(defun chip-type (obj)
  (aref (value-of obj) 0))
(defun chip-data (obj)
  (aref (value-of obj) 1))
(defun chip-max (obj)
  (aref (value-of obj) 2))
(defun skillchip (obj)
  (= (chip-type obj) +chip-skill+))

;;; Corpses/containers
(defun corpse-killer (obj)
  (aref (value-of obj) 1))
(defun corpse-idnum (obj)
  (aref (value-of obj) 2))

;;; Guns
(defun is-energy-cell (obj)
  (is-obj-kind obj +item-energy-cell+))
(defun is-energy-gun (obj)
  (is-obj-kind obj +item-energy-gun+))
(defun is-gun (obj)
  (is-obj-kind obj +item-gun+))
(defun is-rifle (obj)
  (and (is-obj-kind obj +item-gun+)
       (eql (gun-type obj) +gun-7mm-mag+)))
(defun egun-max-energy (obj)
  (let ((battery (first (contains-of obj))))
    (or (and battery
             (is-energy-cell battery)
             (max-energy battery))
        0)))
(defun egun-cur-energy (obj)
  (let ((battery (first (contains-of obj))))
    (or (and battery
             (is-energy-cell battery)
             (cur-energy battery))
        0)))
(defun max-rate-of-fire (obj)
  (aref (value-of obj) 0))
(defun cur-rate-of-fire (obj)
  (aref (value-of obj) 1))
(defun gun-discharge (obj)
  (aref (value-of obj) 0))
(defun gun-type (obj)
  (aref (value-of obj) 3))

(defun get-worn-type (obj)
  (cond
    ((or (= (worn-on-of obj) -1)
         (null (worn-by-of obj)))
     "none")
    ((eql obj (aref (equipment-of (worn-by-of obj)) (worn-on-of obj)))
     "equipped")
    ((eql obj (aref (implants-of (worn-by-of obj)) (worn-on-of obj)))
     "implanted")
    ((eql obj (aref (tattoos-of (worn-by-of obj)) (worn-on-of obj)))
     "tattooed")
    (t
     "unknown")))

;; Material predicates
(defun is-paper (obj)
  (and (>= (material-of obj) +mat-paper+) (< (material-of obj) +mat-cloth+)))
(defun is-cloth (obj)
  (and (>= (material-of obj) +mat-cloth+) (< (material-of obj) +mat-leather+)))
(defun is-leather (obj)
  (and (>= (material-of obj) +mat-leather+) (< (material-of obj) +mat-flesh+)))
(defun is-flesh (obj)
  (and (>= (material-of obj) +mat-flesh+) (< (material-of obj) +mat-vegetable+)))
(defun is-vegetable (obj)
  (and (>= (material-of obj) +mat-vegetable+) (< (material-of obj) +mat-wood+)))
(defun is-wood (obj)
  (and (>= (material-of obj) +mat-wood+) (< (material-of obj) +mat-metal+)))
(defun is-metal (obj)
  (and (>= (material-of obj) +mat-metal+) (< (material-of obj) +mat-plastic+)))
(defun is-plastic (obj)
  (and (>= (material-of obj) +mat-plastic+) (< (material-of obj) +mat-glass+)))
(defun is-glass (obj)
  (and (>= (material-of obj) +mat-glass+) (< (material-of obj) +mat-stone+)))
(defun is-stone (obj)
  (>= (material-of obj) +mat-stone+))

(defun is-ferrous (obj)
  (find (material-of obj) (list +mat-metal+
                                +mat-iron+
                                +mat-steel+
                                +mat-bronze+
                                +mat-mithril+
                                +mat-adamantium+
                                +mat-tin+)))
(defun is-combustable (obj)
  (or (is-paper obj)
      (is-cloth obj)
      (is-flesh obj)
      (is-leather obj)
      (is-vegetable obj)
      (is-wood obj)))

(defun stabbing-weapon? (obj)
  (and (is-obj-kind obj +item-weapon+)
       (or (= (+ (obj-val-of obj 3) +type-hit+) +type-stab+)
           (= (+ (obj-val-of obj 3) +type-hit+) +type-pierce+))))

(defun slashing-weapon? (obj)
  (and (is-obj-kind obj +item-weapon+)
       (= (+ (obj-val-of obj 3) +type-hit+) +type-slash+)))

(defun unserialize-object (container victim room object-node)
  (let* ((vnum (xml-attr object-node "vnum" :numeric t))
         (obj (if (plusp vnum)
                  (read-object vnum)
                  (make-object :unknown nil)))
         (placed nil))
    (dolist (node (cddr object-node))
      (when (consp node)
        (string-case (first node)
          ("name"
           (setf (name-of obj) (third node)))
          ("aliases"
           (setf (aliases-of obj) (third node)))
          ("line_desc"
           (setf (line-desc-of obj) (third node)))
          ("action_desc"
           (setf (action-desc-of obj) (third node)))
          ("extra_desc"
           (push (make-instance 'extra-descr-data
                                :keyword (xml-attr node "keywords")
                                :description (third node))
                 (ex-description-of obj)))
          ("points"
           (setf (kind-of obj) (xml-attr node "type" :numeric t))
           (setf (soilage-of obj) (xml-attr node "soilage" :numeric t))
           (setf (weight-of obj) (xml-attr node "weight" :float t))
           (setf (material-of obj) (xml-attr node "material" :numeric t))
           (setf (timer-of obj) (xml-attr node "timer" :numeric t)))
          ("tracking"
           (setf (unique-id-of obj) (xml-attr node "id" :numeric t))
           (setf (creation-method-of obj)
                 (aref #(:unknown :invalid :imm :search :zone :mob :prog :player)
                       (xml-attr node "method" :numeric t)))
           (setf (creator-of obj) (xml-attr node "creator" :numeric t))
           (setf (creation-time-of obj) (unix-to-timestamp (xml-attr node "time" :numeric t))))
          ("damage"
           (setf (damage-of obj) (xml-attr node "current" :numeric t))
           (setf (max-dam-of obj) (xml-attr node "max" :numeric t))
           (setf (sigil-idnum-of obj) (xml-attr node "sigil_id" :numeric t))
           (setf (sigil-level-of obj) (xml-attr node "sigil_level" :numeric t)))
          ("flags"
           (setf (extra-flags-of obj) (xml-attr node "extra" :hex t))
           (setf (extra2-flags-of obj) (xml-attr node "extra2" :hex t))
           (setf (extra3-flags-of obj) (xml-attr node "extra3" :hex t)))
          ("values"
           (setf (aref (value-of obj) 0) (xml-attr node "v0" :numeric t))
           (setf (aref (value-of obj) 1) (xml-attr node "v1" :numeric t))
           (setf (aref (value-of obj) 2) (xml-attr node "v2" :numeric t))
           (setf (aref (value-of obj) 3) (xml-attr node "v3" :numeric t)))
          ("affectbits"
           (setf (aref (bitvector-of obj) 0) (xml-attr node "aff1" :hex t))
           (setf (aref (bitvector-of obj) 1) (xml-attr node "aff2" :hex t))
           (setf (aref (bitvector-of obj) 2) (xml-attr node "aff3" :hex t)))
          ("affect"
           (let* ((loc (xml-attr node "location" :numeric t))
                  (idx (or (position loc (affected-of obj) :key 'location-of)
                           (position 0 (affected-of obj) :key 'location-of))))
             (setf (location-of (aref (affected-of obj) idx)) loc)
             (setf (modifier-of (aref (affected-of obj) idx))
                   (xml-attr node "modifier" :numeric t))))
          ("object"
           (unserialize-object obj victim room node))
          ("worn"
           (setf (wear-flags-of obj) (xml-attr node "possible" :hex t))
           (let ((position (xml-attr node "pos" :numeric t)))
             (cond
               ((string= (xml-attr node "type") "equipped")
                (equip-char victim obj position :worn))
               ((string= (xml-attr node "type") "implanted")
                (equip-char victim obj position :implant))
               ((string= (xml-attr node "type") "tattooed")
                (equip-char victim obj position :tattoo))
               (container
                (obj-to-obj obj container t))
               (victim
                (obj-to-char obj victim t))
               (room
                (obj-to-room obj room))
               (t
                (errlog "Don't know where to put object!"))))
           (setf placed t)))))

    (unless placed
      (cond
        (container
         (obj-to-obj obj container nil))
        (victim
         (obj-to-char obj victim nil))
        (room
         (obj-to-room obj room nil))))

    obj))

(defun serialize-object (obj)
  ;; TODO: unapply temp affects from object
  `("object"
    (("vnum" ,(write-to-string (vnum-of (shared-of obj)))))
    ("name" () ,(name-of obj))
    ("aliases" () ,(aliases-of obj))
    ("line_desc" () ,(line-desc-of obj))
    ,@(loop for desc in (ex-description-of obj)
           collect `("extra_desc" (("keywords" ,(keyword-of desc)))
                                ,(description-of desc)))
    ,@(unless (string= "" (action-desc-of obj))
            `(("action_desc" () ,(action-desc-of obj))))
    ("points" (("type" ,(write-to-string (kind-of obj)))
               ("soilage" ,(write-to-string (soilage-of obj)))
               ("weight" ,(write-to-string (weight-of obj)))
               ("material" ,(write-to-string (material-of obj)))
               ("timer" ,(write-to-string (timer-of obj)))))
    ("tracking" (("id" ,(write-to-string (unique-id-of obj)))
               ("method" ,(write-to-string (or (position (creation-method-of obj)
                                                         #(:unknown :invalid :imm :search :zone :mob :prog :player))
                                               0)))

               ("creator" ,(write-to-string (creator-of obj)))
               ("time" ,(write-to-string (timestamp-to-unix (creation-time-of obj))))))
    ("damage" (("current" ,(write-to-string (damage-of obj)))
               ("max" ,(write-to-string (max-dam-of obj)))
               ("sigil_id" ,(write-to-string (sigil-idnum-of obj)))
               ("sigil_level" ,(write-to-string (sigil-level-of obj)))))
    ("flags" (("extra" ,(write-to-string (extra-flags-of obj) :base 16))
               ("extra2" ,(write-to-string (extra2-flags-of obj) :base 16))
               ("extra3" ,(write-to-string (extra3-flags-of obj) :base 16))))
    ("values" (("v0" ,(write-to-string (aref (value-of obj) 0)))
               ("v1" ,(write-to-string (aref (value-of obj) 1)))
               ("v2" ,(write-to-string (aref (value-of obj) 2)))
               ("v3" ,(write-to-string (aref (value-of obj) 3)))))
    ("affectbits" (("aff1" ,(write-to-string (aref (bitvector-of obj) 0) :base 16))
                   ("aff2" ,(write-to-string (aref (bitvector-of obj) 1) :base 16))
                   ("aff3" ,(write-to-string (aref (bitvector-of obj) 2) :base 16))))
    ,@(loop for aff across (affected-of obj)
           when (plusp (location-of aff))
           collect `("affect" (("modifier" ,(write-to-string (modifier-of aff)))
                               ("location" ,(write-to-string (location-of aff))))))
    ,@(mapcar 'serialize-object (contains-of obj))
    ;; Intentionally last since reading this propery in load-from-xml causes
    ;; the eq to be worn on the character
    ("worn" (("possible" ,(write-to-string (wear-flags-of obj) :base 16))
             ("pos" ,(write-to-string (worn-on-of obj)))
             ("type" ,(get-worn-type obj)))))
  ;; TODO: reapply temp affects from object
  )



(defun find-object-room (obj)
  "Given OBJ, find the room that it is located in."
  (cond
    ((worn-by-of obj)
     (in-room-of (worn-by-of obj)))
    ((carried-by-of obj)
     (in-room-of (carried-by-of obj)))
    ((in-obj-of obj)
     (find-object-room (in-obj-of obj)))
    ((in-room-of obj)
     (in-room-of obj))
    (t
     (errlog "Object ~a [~d] detected in limbo" (name-of obj)
             (vnum-of obj)))))