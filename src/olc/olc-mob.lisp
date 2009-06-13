(in-package #:tempus)

(defparameter +mset-params+
  '(;; NPC/PC/OLC slots
    ("alias" :type string :slot aliases :desc "mobile alias" :update-mobiles t)
    ("name" :type string :slot name :desc "mobile name" :update-mobiles t)
    ("ldesc" :type string :slot ldesc :desc "mobile ldesc" :nil-allowed t :update-mobiles t)
    ("desc" :type text :slot desc :desc "mobile description")
    ("flags" :type bitflag :slot mob-flags :desc "mobile" :table +action-bits-desc+)
    ("flags2" :type bitflag :slot mob2-flags :desc "mobile" :table +action2-bits-desc+)
    ("aff" :type bitflag :slot aff-flags :desc "mobile aff" :table +affected-bits-desc+)
    ("aff2" :type bitflag :slot aff2-flags :desc "mobile aff2" :table +affected2-bits-desc+)
    ("aff3" :type bitflag :slot aff3-flags :desc "mobile aff3" :table +affected3-bits-desc+)
    ("alignment" :type number :slot alignment :desc "mobile alignment" :min -1000 :max 1000)
    ("str" :type attribute :slot str :desc "mobile strength")
    ("int" :type attribute :slot int :desc "mobile intelligence")
    ("wis" :type attribute :slot wis :desc "mobile wisdom")
    ("dex" :type attribute :slot dex :desc "mobile dexterity")
    ("con" :type attribute :slot con :desc "mobile constitution")
    ("cha" :type attribute :slot cha :desc "mobile charisma")
    ("level" :type number :slot level :desc "mobile level" :min 1 :max 50)
    ("generation" :type number :slot remort-gen :desc "mobile remort generation"
     :min 0 :max 10)
    ("hitp_mod" :type number :slot move :desc "mobile hitpoint modifier"
     :min 1 :max 32767)
    ("hitd_num" :type number :slot hitp :desc "mobile hitpoint dice number"
     :min 1 :max 200)
    ("hitd_size" :type number :slot mana :desc "mobile hitpoint dice size"
     :min 1 :max 200)
    ("mana" :type number :slot max-mana :desc "mobile mana" :min 1 :max 32767)
    ("move" :type number :slot max-move :desc "mobile movement" :min 1 :max 32767)
    ("gold" :type number :slot gold :desc "mobile gold" :min 0 :max 10000000)
    ("exp" :type number :slot exp :desc "mobile experience" :min 1 :max 200000000)
    ("position" :type enumerated :slot position :desc "mobile position"
     :table +position-types+)
    ("sex" :type enumerated :slot sex :desc "mobile sex" :table +sexes+)
    ("remort_class" :type enumerated :slot remort-char-class :desc "mobile remort class"
     :table +class-names+)
    ("cash" :type number :slot cash :desc "mobile cash credits" :min 0 :max 10000000)
    ("hitroll" :type number :slot hitroll :desc "mobile hitroll" :min -125 :max 125)
    ("damroll" :type number :slot damroll :desc "mobile damroll" :min -125 :max 125)
    ("ac" :type number :slot armor :desc "mobile armor class" :min -500 :max 100)
    ("class" :type enumerated :slot char-class :desc "mobile class"
     :table +class-names+)
    ("race" :type enumerated :slot race :desc "mobile race" :table +player-races+)
    ("height" :type number :slot height :desc "mobile height" :min 1 :max 10000)
    ("weight" :type number :slot weight :desc "mobile weight" :min 1 :max 50000)

    ;; OLC shared slots
    ("baredam" :type number :slot damnodice :desc "mobile bare hand damage"
     :shared t :min 1 :max 125)
    ("baredsize" :type number :slot damsizedice :desc "mobile bare hand damage dice size"
     :shared t :min 1 :max 125)
    ("attacktype" :type enumerated :slot attack-type :desc "mobile attack type"
     :shared t :table +attack-types+)
    ("dpos" :type enumerated :slot default-pos :desc "mobile default position"
     :table +position-types+ :shared t)
    ("morale" :type number :slot morale :desc "mobile morale"
     :shared t :min 1 :max 125)
    ("move_buf" :type string :slot move-buf :desc "mobile move buffer"
     :shared t :allow-nil t)
    ("lair" :type number :slot lair :desc "mobile lair" :shared t)
    ("leader" :type number :slot leader :desc "mobile leader vnum" :shared t)
    ("specparam" :type text :slot func-param :desc "mobile specparam" :shared t)
    ("loadparam" :type text :slot load-param :desc "mobile loadparam" :shared t)
    ("prog" :type text :slot prog-text :desc "mobile prog" :shared t)
    ))

(defun update-moblist-full (vnum)
  (let ((proto (real-mobile-proto vnum)))
    (dolist (ch *characters*)
      (when (and (is-npc ch)
                 (= vnum (vnum-of ch))
                 (not (mob2-flagged ch +mob2-renamed+)))
        (setf (name-of ch) (name-of proto))
        (setf (aliases-of ch) (aliases-of proto))
        (setf (ldesc-of ch) (ldesc-of proto))))))

(defun perform-create-mobile (ch vnum)
  (let ((mob (make-instance 'mobile)))
    (setf (shared-of mob) (make-instance 'mob-shared-data :vnum vnum :proto mob))
    (setf (name-of mob) "a fresh blank mobile")
    (setf (aliases-of mob) "fresh blank mobile")
    (setf (ldesc-of mob) "A fresh blank mobile is here waiting to be violated.")
    ;; max-hitp of 0 is a flag that H, M, V is xdy+z
    (setf (max-hitp-of mob) 0)
    (setf (hitp-of mob) 10)
    (setf (mana-of mob) 10)
    (setf (move-of mob) 50)
    (setf (damnodice-of (shared-of mob)) 5)
    (setf (damsizedice-of (shared-of mob)) 2)
    (setf (char-class-of mob) +class-normal+)
    (setf (race-of mob) +race-mobile+)
    (setf (exp-of mob) 100)
    (setf (morale-of (shared-of mob)) 100)
    (setf (gethash vnum *mobile-prototypes*) mob)
    (setf (olc-mob-of ch) mob)
    (send-to-char ch "Mobile ~d successfully created.~%Now editing mobile ~d.~%"
                  vnum vnum)))

(defun perform-destroy-mobile (ch vnum)
  (dolist (mob (copy-list *characters*))
    (when (and (is-npc mob) (= (vnum-of mob) vnum))
      (purge-creature mob nil)))

  (remhash vnum *mobile-prototypes*)

  (dolist (tch *characters*)
    (when (and (typep tch 'player)
               (olc-mob-of tch)
               (eql (vnum-of (olc-mob-of tch)) vnum))
      (setf (olc-mob-of tch) nil)
      (send-to-char tch "The mobile you were editing has been destroyed.~%")))
  (send-to-char ch "Mobile eliminated.~%"))

(defun save-mobile (ouf mob)
  (let ((extended-p (or (/= (str-of (aff-abils-of mob)) 11)
                        (/= (str-add-of (aff-abils-of mob)) 0)
                        (/= (int-of (aff-abils-of mob)) 11)
                        (/= (wis-of (aff-abils-of mob)) 11)
                        (/= (dex-of (aff-abils-of mob)) 11)
                        (/= (con-of (aff-abils-of mob)) 11)
                        (/= (cha-of (aff-abils-of mob)) 11)
                        (/= (max-mana-of mob) 10)
                        (/= (max-move-of mob) 50)
                        (/= (weight-of mob) 200)
                        (/= (height-of mob) 198)
                        (plusp (lair-of (shared-of mob)))
                        (plusp (leader-of (shared-of mob)))
                        (/= (remort-char-class-of mob) -1)
                        (/= (cash-of mob) 0)
                        (/= (morale-of (shared-of mob)) 100)
                        (move-buf-of (shared-of mob))
                        (/= (current-tongue-of mob) 0))))
  (format ouf "#~d~%" (vnum-of mob))
  (format ouf "~a~~~%" (aliases-of mob))
  (format ouf "~a~~~%" (name-of mob))
  (format ouf "~a~%~~~%" (ldesc-of mob))
  (format ouf "~a~~~%" (fdesc-of mob))
  (format ouf "~a ~a ~a ~a ~a ~d ~a~%"
          (bits-to-asciiflag (mob-flags-of mob))
          (bits-to-asciiflag (mob2-flags-of mob))
          (bits-to-asciiflag (aff-flags-of mob))
          (bits-to-asciiflag (aff2-flags-of mob))
          (bits-to-asciiflag (aff3-flags-of mob))
          (alignment-of mob)
          (if extended-p "E" "S"))
  (format ouf "~d ~d ~d ~dd~d+~d ~dd~d+~d~%"
          (level-of mob)
          (- 20 (hitroll-of mob))
          (floor (armor-of mob) 10)
          (hitp-of mob)
          (mana-of mob)
          (move-of mob)
          (damnodice-of (shared-of mob))
          (damsizedice-of (shared-of mob))
          (damroll-of mob))
  (format ouf "~d ~d ~d ~d~%"
          (gold-of mob)
          (exp-of mob)
          (race-of mob)
          (char-class-of mob))
  (format ouf "~d ~d ~d ~d~%"
          (position-of mob)
          (default-pos-of (shared-of mob))
          (case (sex-of mob)
            (neuter 0)
            (male 1)
            (female 2))
          (attack-type-of (shared-of mob)))
  (when extended-p
    (when (/= (str-of (aff-abils-of mob)) 11)
      (format ouf "Str: ~d~%" (str-of (aff-abils-of mob))))
    (when (/= (str-add-of (aff-abils-of mob)) 0)
      (format ouf "StrAdd: ~d~%" (str-add-of (aff-abils-of mob))))
    (when (/= (int-of (aff-abils-of mob)) 11)
      (format ouf "Int: ~d~%" (int-of (aff-abils-of mob))))
    (when (/= (wis-of (aff-abils-of mob)) 11)
      (format ouf "Wis: ~d~%" (wis-of (aff-abils-of mob))))
    (when (/= (dex-of (aff-abils-of mob)) 11)
      (format ouf "Dex: ~d~%" (dex-of (aff-abils-of mob))))
    (when (/= (con-of (aff-abils-of mob)) 11)
      (format ouf "Con: ~d~%" (con-of (aff-abils-of mob))))
    (when (/= (cha-of (aff-abils-of mob)) 11)
      (format ouf "Cha: ~d~%" (cha-of (aff-abils-of mob))))
    (when (/= (max-mana-of mob) 10)
      (format ouf "MaxMana: ~d~%" (max-mana-of mob)))
    (when (/= (max-move-of mob) 50)
      (format ouf "MaxMove: ~d~%" (max-move-of mob)))
    (when (/= (weight-of mob) 200)
      (format ouf "Weight: ~d~%" (weight-of mob)))
    (when (/= (height-of mob) 198)
      (format ouf "Height: ~d~%" (height-of mob)))
    (when (/= (current-tongue-of mob) 0)
      (format ouf "CurTongue: ~d~%" (current-tongue-of mob)))
    (dolist (tongue-id (hash-keys *tongues*))
      (when (plusp (check-tongue mob tongue-id))
        (format ouf "KnownTongue: ~d~%" tongue-id)))
    (when (/= (cash-of mob) 0)
      (format ouf "Cash: ~d~%" (cash-of mob)))
    (when (/= (morale-of (shared-of mob)) 100)
      (format ouf "Morale: ~d~%" (morale-of (shared-of mob))))
    (when (plusp (lair-of (shared-of mob)))
      (format ouf "Lair: ~d~%" (lair-of (shared-of mob))))
    (when (plusp (leader-of (shared-of mob)))
      (format ouf "Leader: ~d~%" (leader-of (shared-of mob))))
    (when (/= (remort-char-class-of mob) -1)
      (format ouf "RemortClass: ~d~%" (remort-char-class-of mob)))
    (when (move-buf-of (shared-of mob))
      (format ouf "Move_buf: ~a~%" (move-buf-of (shared-of mob))))
    (when (func-param-of (shared-of mob))
      (format ouf "SpecParam:~%~a~~~%" (func-param-of (shared-of mob))))
    (when (load-param-of (shared-of mob))
      (format ouf "LoadParam:~%~a~~~%" (load-param-of (shared-of mob))))
    (when (prog-text-of (shared-of mob))
      (format ouf "Prog:~%~a~~~%" (prog-text-of (shared-of mob))))
    (when (/= (remort-gen-of mob) 0)
      (format ouf "Generation: ~d~%" (remort-gen-of mob)))
    (format ouf "E~%"))))

(defun save-zone-mobiles (ch zone)
  (let ((context nil))
    (handler-case
        (progn
          (setf context "mobile index")
          (update-index-file (tempus-path "lib/world/mob/index") (number-of zone) "mob")

          (setf context "mobile file")
          (with-open-file (ouf (tempus-path "lib/world/mob/~d.mob" (number-of zone))
                               :direction :output
                               :if-exists :rename-and-delete
                               :if-does-not-exist :create)
            (loop
               for vnum from (* (number-of zone) 100) upto (top-of zone)
               as mob-proto = (real-mobile-proto vnum)
               when mob-proto
               do (save-mobile ouf mob-proto))
            (format ouf "$~%"))

          (slog "OLC: ~a msaved ~d" (name-of ch) (number-of zone))
          t)
      (error (err)
        (slog "Error while saving ~a: ~a!" context err)
        nil))))

(defun perform-olc-mload (ch vnum)
  (when (check-can-edit ch (zone-containing-number vnum) +zone-mobs-approved+)
    (cond
      ((not (eql (zone-containing-number vnum) (zone-of (in-room-of ch))))
       (send-to-char ch "You cannot olc mload mobiles from other zones.~%"))
      ((and (not (security-is-member ch "OLCWorldWrite")) (approvedp (real-object-proto vnum)))
       (send-to-char ch "You cannot olc mload approved mobiles.~%"))
      (t
       (let ((mob (read-mobile vnum)))
         (char-to-room mob (in-room-of ch) nil)
         (act ch :target mob
              :subject-emit "$N appears next to you."
              :place-emit "$n creates $N in $s hands.")
         (slog "OLC: ~a mloaded ~a(~d)" (name-of ch) (name-of mob) (vnum-of mob)))))))

(defun perform-mmimic (dst src)
  (setf (name-of dst) (name-of src))
  (setf (aliases-of dst) (aliases-of src))
  (setf (ldesc-of dst) (ldesc-of src))
  (setf (fdesc-of dst) (fdesc-of src))
  (setf (mob-flags-of dst) (mob-flags-of src))
  (setf (mob2-flags-of dst) (mob2-flags-of src))
  (setf (aff-flags-of dst) (aff-flags-of src))
  (setf (aff2-flags-of dst) (aff2-flags-of src))
  (setf (aff3-flags-of dst) (aff3-flags-of src))
  (setf (alignment-of dst) (alignment-of src))
  (setf (remort-gen-of dst) (remort-gen-of src))
  (setf (level-of dst) (level-of src))
  (setf (hitroll-of dst) (hitroll-of src))
  (setf (armor-of dst) (armor-of src))
  (setf (max-hitp-of dst) (max-hitp-of src))
  (setf (max-mana-of dst) (max-mana-of src))
  (setf (max-move-of dst) (max-move-of src))
  (setf (hitp-of dst) (hitp-of src))
  (setf (mana-of dst) (mana-of src))
  (setf (move-of dst) (move-of src))
  (setf (gold-of dst) (gold-of src))
  (setf (exp-of dst) (exp-of src))
  (setf (position-of dst) (position-of src))
  (setf (weight-of dst) (weight-of src))
  (setf (height-of dst) (height-of src))
  (setf (remort-char-class-of dst) (remort-char-class-of src))
  (setf (char-class-of dst) (char-class-of src))
  (setf (race-of dst) (race-of src))

  (setf (attack-type-of (shared-of dst)) (attack-type-of (shared-of src)))
  (setf (default-pos-of (shared-of dst)) (default-pos-of (shared-of src)))
  (setf (damnodice-of (shared-of dst)) (damnodice-of (shared-of src)))
  (setf (damsizedice-of (shared-of dst)) (damsizedice-of (shared-of src)))
  (setf (lair-of (shared-of dst)) (lair-of (shared-of src)))
  (setf (leader-of (shared-of dst)) (leader-of (shared-of src)))
  (setf (morale-of (shared-of dst)) (morale-of (shared-of src)))
  (setf (move-buf-of (shared-of dst)) (move-buf-of (shared-of src)))
  (setf (func-of (shared-of dst)) (func-of (shared-of src)))
  (setf (func-param-of (shared-of dst)) (func-param-of (shared-of src)))
  (setf (load-param-of (shared-of dst)) (load-param-of (shared-of src)))
  (setf (prog-text-of (shared-of dst)) (prog-text-of (shared-of src)))

  (setf (real-abils-of dst) (copy-abilities (real-abils-of src)))
  (setf (aff-abils-of dst) (copy-abilities (aff-abils-of src))))

(defun save-mobile-special-assignments ()
  (with-open-file (ouf (tempus-path "lib/etc/spec_ass_mob")
                       :direction :output
                       :if-exists :rename-and-delete
                       :if-does-not-exist :create)
    (dolist (obj (sort (hash-values *mobile-prototypes*) #'< :key 'vnum-of))
      (format ouf "~6d ~20s ## ~a~%"
              (vnum-of (shared-of obj))
              (find-special-by-func (func-of (shared-of obj)))
              (name-of obj)))))

(defun perform-mlist (ch vnum-start vnum-end)
  (with-pagination ((link-of ch))
    (loop
       with index = 1
       for vnum from vnum-start upto vnum-end
       as mob = (real-mobile-proto vnum)
       when mob
       do
         (send-to-char ch "~4d. &g[&n~5d&g]&y ~40a &n[~2d] <~3d>~:[~; (!ap)~]~:[~; (prog)~]~%"
                       index vnum
                       (name-of mob)
                       (level-of mob)
                       (number-of (shared-of mob))
                       (mob2-flagged mob +mob2-unapproved+)
                       (prog-text-of (shared-of mob)))
         (incf index))))

(defcommand (ch "olc" "create" "mobile" "next") (:immortal)
  (let* ((zone (zone-of (in-room-of ch)))
         (number (loop for num from (* (number-of zone) 100) upto (top-of zone)
                    when (null (real-mobile-proto num)) do (return num)
                    finally (return nil))))
    (cond
      ((null zone)
       (send-to-char ch "A zone must be defined for the mobile first.~%"))
      ((not (check-can-edit ch zone +zone-mobs-approved+))
       nil)
      ((real-mobile-proto number)
       (send-to-char ch "That mobile already exists.~%"))
      (t
       (perform-create-mobile ch number)))))

(defcommand (ch "olc" "create" "mobile" number) (:immortal)
  (with-numeric-input ((number "You must specify a proper mobile vnum!"))
    (let ((zone (zone-containing-number number)))
      (cond
        ((null zone)
          (send-to-char ch "A zone must be defined for the mobile first.~%"))
        ((not (check-can-edit ch zone +zone-mobs-approved+))
         nil)
        ((real-mobile-proto number)
         (send-to-char ch "That mobile already exists.~%"))
        (t
         (perform-create-mobile ch number))))))

(defcommand (ch "approve" "mobile") (:immortal)
  (send-to-char ch "Usage: approve mobile <vnum>~%"))

(defcommand (ch "approve" "mobile" vnum) (:immortal)
  (with-numeric-input ((vnum "That's no mobile vnum.~%"))
    (let* ((mob (real-mobile-proto vnum))
           (zone (zone-containing-number vnum)))
      (cond
        ((null mob)
         (send-to-char ch "There exists no mobile with that number, slick.~%"))
        ((null zone)
         (send-to-char ch "That mobile belongs to no zone.~%"))
        ((not (logtest (mob2-flags-of mob) +mob2-unapproved+))
         (send-to-char ch "That item is already approved.~%"))
        (t
         (setf (mob2-flags-of mob) (logandc2 (mob2-flags-of mob) +mob2-unapproved+))
         (send-to-char ch "Mobile approved for full inclusion in the game.~%")
         (slog "~a approved mobile ~a[~d]" (name-of ch) (name-of mob) (vnum-of mob))
         (save-zone-mobiles ch zone))))))

(defcommand (ch "unapprove" "mobile") (:immortal)
  (send-to-char ch "Usage: unapprove mobile <vnum>~%"))

(defcommand (ch "unapprove" "mobile" vnum) (:immortal)
  (with-numeric-input ((vnum "That's no mobile vnum.~%"))
    (let* ((mob (real-mobile-proto vnum))
           (zone (zone-containing-number vnum)))
      (cond
        ((null mob)
         (send-to-char ch "There exists no mobile with that number, slick.~%"))
        ((null zone)
         (send-to-char ch "That mobile belongs to no zone.~%"))
        ((logtest (mob2-flags-of mob) +mob2-unapproved+)
         (send-to-char ch "That item is already approved.~%"))
        (t
         (setf (mob2-flags-of mob) (logior (mob2-flags-of mob) +mob2-unapproved+))
         (send-to-char ch "Mobile unapproved.~%")
         (slog "~a unapproved mobile ~a[~d]" (name-of ch) (name-of mob) (vnum-of mob))
         (save-zone-mobiles ch zone))))))

(defcommand (ch "olc" "clear" "mobile") (:immortal)
  (when (and (check-is-editing ch "mobile" (olc-mob-of ch))
             (check-can-edit ch
                             (zone-containing-number (vnum-of (shared-of (olc-obj-of ch))))
                             +zone-mobs-approved+))

    (with-slots (name aliases ldesc fdesc mob-flags mob2-flags aff-flags
                      aff2-flags aff3-flags alignment remort-gen
                      real-abils aff-abils level hitroll armor
                      max-hitp max-mana max-move damroll char-class
                      race gold exp weight height remort-char-class shared)
        (olc-mob-of ch)
      (setf name "a fresh blank mobile")
      (setf aliases "fresh blank mobile")
      (setf ldesc "A fresh blank mobile is here waiting to be violated.")
      (setf fdesc nil)
      (setf mob-flags 0)
      (setf mob2-flags 0)
      (setf aff-flags 0)
      (setf aff2-flags 0)
      (setf aff3-flags 0)
      (setf alignment 0)
      (setf remort-gen 0)
      (setf real-abils (make-instance 'char-ability-data
                                      :str 11
                                      :str-add 0
                                      :int 11
                                      :wis 11
                                      :dex 11
                                      :con 11
                                      :cha 11))
      (setf aff-abils (copy-abilities real-abils))
      (setf level 1)
      (setf hitroll 0)
      (setf armor 0)
      (setf max-hitp 0)
      (setf max-mana 0)
      (setf max-move 0)

      (setf damroll 0)
      (setf char-class +class-normal+)
      (setf race +race-mobile+)
      (setf gold 0)
      (setf exp 100)

      (setf (damnodice-of shared) 5)
      (setf (damsizedice-of shared) 2)
      (setf (morale-of shared) 100)
      (setf (lair-of shared) -1)
      (setf (leader-of shared) -1)
      (setf (attack-type-of shared) 0)

      (setf weight 130)
      (setf height 166)
      (setf remort-char-class -1))

    (send-to-char ch "Okay, mobile #~d fully cleared.~%" (vnum-of (olc-mob-of ch)))))

(defcommand (ch "olc" "destroy" "mobile") (:immortal)
  (send-to-char ch "Usage: olc destroy mobile <vnum>~%"))

(defcommand (ch "olc" "destroy" "mobile" number) (:immortal)
  (with-numeric-input ((number "Usage: olc destroy object <vnum>"))
    (let ((zone (zone-containing-number number)))
      (cond
        ((null zone)
         (send-to-char ch "That mobile doesn't belong to any zone.~%"))
        ((can-edit-zone ch zone +zone-mobs-approved+)
         (perform-destroy-mobile ch number))))))

(defcommand (ch "olc" "msave") (:immortal)
  (let ((zone (if (olc-mob-of ch)
                  (zone-containing-number (vnum-of (olc-mob-of ch)))
                  (zone-of (in-room-of ch)))))
    (when (check-can-edit ch zone +zone-mobs-approved+)
      (if (save-zone-mobiles ch zone)
        (send-to-char ch "You save the mobiles for zone #~d (~a).~%"
                      (number-of zone)
                      (name-of zone))
        (send-to-char ch "The mobiles for zone #~d (~a) could not be saved.~%"
                      (number-of zone)
                      (name-of zone))))))

(defcommand (ch "olc" "medit") (:immortal)
  (if (olc-mob-of ch)
      (send-to-char ch "Current olc mobile: [~d] &g~a&n~%"
                    (vnum-of (shared-of (olc-mob-of ch)))
                    (name-of (olc-mob-of ch)))
      (send-to-char ch "You are not currently editing an mobile.~%")))

(defcommand (ch "olc" "medit" vnum) (:immortal)
  (with-numeric-input ((vnum "Usage: olc medit (<number>|exit)~%"))
    (let ((mob (real-mobile-proto vnum)))
      (cond
        ((null mob)
         (send-to-char ch "There is no such mobile.~%"))
        ((check-can-edit ch (zone-containing-number vnum) +zone-mobs-approved+)
         (setf (olc-mob-of ch) mob)
         (send-to-char ch "Now editing mobile [~d] &y~a&n~%"
                       vnum (name-of mob)))))))

(defcommand (ch "olc" "medit" "exit") (:immortal)
  (setf (olc-mob-of ch) nil)
  (send-to-char ch "Exiting mobile editor.~%"))

(defcommand (ch "olc" "mstat") (:immortal)
  (if (check-is-editing ch "mobile" (olc-mob-of ch))
      (send-stats-to-char ch (olc-mob-of ch))
      (send-to-char ch "You aren't editing a mobile.~%")))

(defcommand (ch "olc" "mstat" number) (:immortal)
  (let* ((vnum (parse-integer number :junk-allowed t))
         (mob (and vnum (real-mobile-proto vnum))))
    (cond
      ((null vnum)
       (send-to-char ch "Usage: olc mstat [<number>]~%"))
      ((null mob)
       (send-to-char ch "There is no such mobile.~%"))
      (t
       (send-stats-to-char ch mob)))))

(defcommand (ch "olc" "mload") (:immortal)
  (if (olc-mob-of ch)
      (perform-olc-mload ch (vnum-of (olc-mob-of ch)))
      (send-to-char ch "Which mobile?~%")))

(defcommand (ch "olc" "mload" vnum) (:immortal)
  (with-numeric-input ((vnum "You must specify a valid vnum."))
    (if (null (real-mobile-proto vnum))
        (send-to-char ch "No such mobile exists.~%")
        (perform-olc-mload ch vnum))))

(defcommand (ch "olc" "mmimic" vnum) (:immortal)
  (when (and (check-is-editing ch "mobile" (olc-mob-of ch))
             (check-can-edit ch (zone-containing-number (vnum-of (olc-mob-of ch)))
                             +zone-mobs-approved+))
    (with-numeric-input ((vnum "You must specify a valid vnum."))
      (let ((proto (real-mobile-proto vnum)))
        (cond
          ((null proto)
           (send-to-char ch "That's not a valid mobile, genius.~%"))
          ((eql proto (olc-mob-of ch))
           (send-to-char ch "Real funny.~%"))
          (t
           (perform-mmimic (olc-mob-of ch) proto)
           (unless (security-is-member ch "OLCWorldWrite")
             (setf (mob2-flags-of (olc-obj-of ch)) (logandc2 (mob2-flags-of (olc-obj-of ch))
                                                             +mob2-unapproved+)))
           (update-moblist-full vnum)
           (send-to-char ch "Okay, done mimicing.~%")))))))

(defcommand (ch "olc" "mset") (:immortal)
  (with-pagination ((link-of ch))
    (send-to-char ch "Valid mset commands:~%~{  &y~a&n~%~}"
                  (remove-duplicates
                   (mapcan (lambda (cmd)
                             (when (and (string= "olc" (first (command-info-pattern cmd)))
                                        (string= "mset" (second (command-info-pattern cmd)))
                                        (stringp (third (command-info-pattern cmd))))
                               (list (third (command-info-pattern cmd)))))
                           *commands*)
                   :test #'string=))))

(defcommand (ch "olc" "mset" param) (:immortal)
  (when (and (check-is-editing ch "mobile" (olc-mob-of ch))
             (check-can-edit ch (zone-containing-number (vnum-of (olc-mob-of ch)))
                             +zone-objs-approved+))
    (perform-set ch (olc-mob-of ch) t +mset-params+ param nil)))

(defcommand (ch "olc" "mset" param value) (:immortal)
  (when (and (check-is-editing ch "mobile" (olc-mob-of ch))
             (check-can-edit ch (zone-containing-number (vnum-of (olc-mob-of ch)))
                             +zone-objs-approved+))
    (perform-set ch (olc-mob-of ch) t +mset-params+ param value)))

(defcommand (ch "olc" "mset" "special") (:immortal)
  (send-to-char ch "Usage: olc mset special <special>~%"))

(defcommand (ch "olc" "mset" "special" special-name) (:immortal)
  (when (and (check-is-editing ch "mobile" (olc-mob-of ch))
             (check-can-edit ch (zone-containing-number (vnum-of (olc-mob-of ch)))
                             +zone-objs-approved+))
    (let ((special (find special-name (hash-keys *special-funcs*) :test 'string-abbrev)))
      (cond
        ((null special)
         (send-to-char ch "That is not a valid special.~%"))
        ((not (logtest (gethash special *special-flags*) +spec-mob+))
         (send-to-char ch "This special is not for mobiles.~%"))
        ((and (logtest (gethash special *special-flags*) +spec-res+)
              (not (security-is-member ch "OLCWorldWrite")))
         (send-to-char ch "This special is reserved.~%"))
        (t
         (setf (func-of (shared-of (olc-mob-of ch))) (gethash special *special-funcs*))
         (save-mobile-special-assignments)
         (send-to-char ch "Mobile special set, you trickster you.~%"))))))

(defcommand (ch "olc" "mset" "knownlanguage") (:immortal)
  (send-to-char ch "Usage: olc mset knownlanguage (+\-) <languages>~%"))

(defcommand (ch "olc" "mset" "knownlanguage" plus-or-minus languages) (:immortal)
  (when (and (check-is-editing ch "mobile" (olc-mob-of ch))
             (check-can-edit ch (zone-containing-number (vnum-of (olc-mob-of ch)))
                             +zone-mobs-approved+))
    (let ((val (case (char plus-or-minus 0)
                 (#\+ 100)
                 (#\- 0)
                 (t (signal 'parser-error :message "Usage: olc mset knownlanguage (+|-) <languages>\n")))))
      (dolist (language (split-sequence #\space languages :remove-empty-subseqs t))
        (let ((tongue-idx (find-tongue-idx-by-name language)))
          (cond
            (tongue-idx
              (setf (aref (tongues-of (olc-mob-of ch)) tongue-idx) val)
              (send-to-char ch "~a mobile language ~a.~%"
                            (if (zerop val) "Removing" "Adding")
                            (tongue-name tongue-idx)))
            (t
              (send-to-char ch "Invalid language ~a, skipping...~%" language))))))))

(defcommand (ch "olc" "mset" "curlanguage") (:immortal)
  (send-to-char ch "Usage: olc mset curlanguage <language>~%"))

(defcommand (ch "olc" "mset" "curlanguage" language) (:immortal)
  (when (and (check-is-editing ch "mobile" (olc-mob-of ch))
             (check-can-edit ch (zone-containing-number (vnum-of (olc-mob-of ch)))
                             +zone-mobs-approved+))
    (let ((tongue-idx (find-tongue-idx-by-name language)))
          (cond
            (tongue-idx
             (setf (current-tongue-of (olc-mob-of ch)) tongue-idx)
             (send-to-char ch "Mobile language set to ~a.~%" (tongue-name tongue-idx)))
            (t
              (send-to-char ch "That's not a language.~%" language)))
      (if tongue-idx
          (setf (current-tongue-of (olc-mob-of ch)) tongue-idx)
          (send-to-char ch "That's not a language~%" language)))))

(defcommand (ch "mlist") (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (perform-mlist ch (* (number-of zone) 100) (top-of zone))))

(defcommand (ch "mlist" junk) (:immortal)
  (declare (ignore junk))
  (send-to-char ch "Usage: mlist [<start vnum> <end vnum>]~%"))

(defcommand (ch "mlist" start end) (:immortal)
  (let ((start-num (parse-integer start :junk-allowed t))
        (end-num (parse-integer end :junk-allowed t)))
    (if (or (null start-num) (null end-num))
        (send-to-char ch "Usage: mlist [<start vnum> <end vnum>]~%")
        (perform-mlist ch (min start-num end-num) (max start-num end-num)))))