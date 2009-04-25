(in-package #:tempus)

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

(defcommand (ch "olc" "mset" junk) (:immortal)
  (send-to-char ch "'~a' is not a supported mset command.~%" junk))

(defcommand (ch "olc" "mset" "alias") (:immortal)
  (send-to-char ch "Usage: olc mset alias <alias>~%"))

(defcommand (ch "olc" "mset" "alias" alias) (:immortal)
  (perform-set-string ch alias (olc-mob-of ch) "mobile alias"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+
                      nil
                      (lambda (val mob)
                        (setf (aliases-of mob) val)
                        (update-moblist-full (vnum-of (olc-mob-of ch))))))

(defcommand (ch "olc" "mset" "name") (:immortal)
  (send-to-char ch "Usage: olc mset name <name>~%"))

(defcommand (ch "olc" "mset" "name" name) (:immortal)
  (perform-set-string ch name (olc-mob-of ch) "mobile name"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+
                      nil
                      (lambda (val mob)
                        (setf (name-of mob) val)
                        (update-moblist-full (vnum-of (olc-mob-of ch))))))

(defcommand (ch "olc" "mset" "ldesc") (:immortal)
  (send-to-char ch "Usage: olc mset ldesc <ldesc>~%"))

(defcommand (ch "olc" "mset" "ldesc" ldesc) (:immortal)
  (perform-set-string ch ldesc (olc-mob-of ch) "mobile ldesc"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+
                      nil
                      (lambda (val mob)
                        (setf (ldesc-of mob) val)
                        (update-moblist-full (vnum-of (olc-mob-of ch))))))

(defcommand (ch "olc" "mset" "desc") (:immortal)
  (perform-set-text ch (olc-mob-of ch) "mobile description"
                    (vnum-of (olc-mob-of ch))
                    +zone-mobs-approved+
                    (fdesc-of (olc-mob-of ch))
                    #'(setf fdesc-of)))

(defcommand (ch "olc" "mset" "flags") (:immortal)
  (send-to-char ch "Usage: olc mset flags (+|-) <flags>"))

(defcommand (ch "olc" "mset" "flags" plus-or-minus flags) (:immortal)
  (when (and (check-is-editing ch "mobile" (olc-mob-of ch))
             (check-can-edit ch (zone-containing-number (vnum-of (olc-mob-of ch)))
                             +zone-mobs-approved+))
    (perform-set-flags ch plus-or-minus flags +action-bits-desc+
                       "mobile"
                       "olc mset flags (+|-) <flags>"
                       (lambda () (mob-flags-of (olc-mob-of ch)))
                       (lambda (val) (setf (mob-flags-of (olc-mob-of ch)) val)))))

(defcommand (ch "olc" "mset" "flags2") (:immortal)
  (send-to-char ch "Usage: olc mset flags2 (+|-) <flags>"))

(defcommand (ch "olc" "mset" "flags2" plus-or-minus flags) (:immortal)
  (when (and (check-is-editing ch "mobile" (olc-mob-of ch))
             (check-can-edit ch (zone-containing-number (vnum-of (olc-mob-of ch)))
                             +zone-mobs-approved+))
    (perform-set-flags ch plus-or-minus flags +action2-bits-desc+
                       "mobile"
                       "olc mset flags2 (+|-) <flags>"
                       (lambda () (mob2-flags-of (olc-mob-of ch)))
                       (lambda (val) (setf (mob2-flags-of (olc-mob-of ch)) val)))))

(defcommand (ch "olc" "mset" "aff") (:immortal)
  (send-to-char ch "Usage: olc mset aff (+|-) <flags>"))

(defcommand (ch "olc" "mset" "aff" plus-or-minus flags) (:immortal)
  (when (and (check-is-editing ch "mobile" (olc-mob-of ch))
             (check-can-edit ch (zone-containing-number (vnum-of (olc-mob-of ch)))
                             +zone-mobs-approved+))
    (perform-set-flags ch plus-or-minus flags +affected-bits-desc+
                       "mobile aff"
                       "olc mset aff (+|-) <flags>"
                       (lambda () (aff-flags-of (olc-mob-of ch)))
                       (lambda (val) (setf (aff-flags-of (olc-mob-of ch)) val)))))

(defcommand (ch "olc" "mset" "aff2") (:immortal)
  (send-to-char ch "Usage: olc mset aff2 (+|-) <flags>"))

(defcommand (ch "olc" "mset" "aff2" plus-or-minus flags) (:immortal)
  (when (and (check-is-editing ch "mobile" (olc-mob-of ch))
             (check-can-edit ch (zone-containing-number (vnum-of (olc-mob-of ch)))
                             +zone-mobs-approved+))
    (perform-set-flags ch plus-or-minus flags +affected2-bits-desc+
                       "mobile aff2"
                       "olc mset aff2 (+|-) <flags>"
                       (lambda () (aff2-flags-of (olc-mob-of ch)))
                       (lambda (val) (setf (aff2-flags-of (olc-mob-of ch)) val)))))

(defcommand (ch "olc" "mset" "aff3") (:immortal)
  (send-to-char ch "Usage: olc mset aff3 (+|-) <flags>"))

(defcommand (ch "olc" "mset" "aff3" plus-or-minus flags) (:immortal)
  (when (and (check-is-editing ch "mobile" (olc-mob-of ch))
             (check-can-edit ch (zone-containing-number (vnum-of (olc-mob-of ch)))
                             +zone-mobs-approved+))
    (perform-set-flags ch plus-or-minus flags +affected3-bits-desc+
                       "mobile aff3"
                       "olc mset aff3 (+|-) <flags>"
                       (lambda () (aff3-flags-of (olc-mob-of ch)))
                       (lambda (val) (setf (aff3-flags-of (olc-mob-of ch)) val)))))

(defcommand (ch "olc" "mset" "alignment") (:immortal)
  (send-to-char ch "Usage: olc mset align <align>~%"))

(defcommand (ch "olc" "mset" "alignment" align) (:immortal)
  (perform-set-number ch align (olc-mob-of ch) "mobile alignment" (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ -1000 1000
                      #'(setf alignment-of)))

(defcommand (ch "olc" "mset" "str") (:immortal)
  (send-to-char ch "Usage: olc mset str <str>~%"))

(defcommand (ch "olc" "mset" "str" str) (:immortal)
  (perform-set-number ch str (olc-mob-of ch) "mobile strength" (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ 1 25
                      (lambda (val mob)
                        (setf (str-of (real-abils-of mob)) val)
                        (setf (str-of (aff-abils-of mob)) val))))

(defcommand (ch "olc" "mset" "int") (:immortal)
  (send-to-char ch "Usage: olc mset int <int>~%"))

(defcommand (ch "olc" "mset" "int" int) (:immortal)
  (perform-set-number ch int (olc-mob-of ch) "mobile intelligence" (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ 1 25
                      (lambda (val mob)
                        (setf (int-of (real-abils-of mob)) val)
                        (setf (int-of (aff-abils-of mob)) val))))

(defcommand (ch "olc" "mset" "wis") (:immortal)
  (send-to-char ch "Usage: olc mset wis <wis>~%"))

(defcommand (ch "olc" "mset" "wis" wis) (:immortal)
  (perform-set-number ch wis (olc-mob-of ch) "mobile wisdom" (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ 1 25
                      (lambda (val mob)
                        (setf (wis-of (real-abils-of mob)) val)
                        (setf (wis-of (aff-abils-of mob)) val))))

(defcommand (ch "olc" "mset" "dex") (:immortal)
  (send-to-char ch "Usage: olc mset dex <dex>~%"))

(defcommand (ch "olc" "mset" "dex" dex) (:immortal)
  (perform-set-number ch dex (olc-mob-of ch) "mobile dexterity" (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ 1 25
                      (lambda (val mob)
                        (setf (dex-of (real-abils-of mob)) val)
                        (setf (dex-of (aff-abils-of mob)) val))))

(defcommand (ch "olc" "mset" "con") (:immortal)
  (send-to-char ch "Usage: olc mset con <con>~%"))

(defcommand (ch "olc" "mset" "con" con) (:immortal)
  (perform-set-number ch con (olc-mob-of ch) "mobile constitution" (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ 1 25
                      (lambda (val mob)
                        (setf (con-of (real-abils-of mob)) val)
                        (setf (con-of (aff-abils-of mob)) val))))

(defcommand (ch "olc" "mset" "cha") (:immortal)
  (send-to-char ch "Usage: olc mset cha <cha>~%"))

(defcommand (ch "olc" "mset" "cha" cha) (:immortal)
  (perform-set-number ch cha (olc-mob-of ch) "mobile charisma" (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ 1 25
                      (lambda (val mob)
                        (setf (cha-of (real-abils-of mob)) val)
                        (setf (cha-of (aff-abils-of mob)) val))))

(defcommand (ch "olc" "mset" "level") (:immortal)
  (send-to-char ch "Usage: olc mset level <level>~%"))

(defcommand (ch "olc" "mset" "level" level) (:immortal)
  (perform-set-number ch level (olc-mob-of ch) "mobile level" (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ 1 50
                      #'(setf level-of)))

(defcommand (ch "olc" "mset" "hitp_mod") (:immortal)
  (send-to-char ch "Usage: olc mset hitp_mod <hitp_mod>~%"))

(defcommand (ch "olc" "mset" "hitp_mod" hitp-mod) (:immortal)
  (perform-set-number ch hitp-mod (olc-mob-of ch) "mobile hit point modifier"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ 1 32767
                      #'(setf move-of)))

(defcommand (ch "olc" "mset" "hitd_num") (:immortal)
  (send-to-char ch "Usage: olc mset hitd_num <hitd_num>~%"))

(defcommand (ch "olc" "mset" "hitd_num" hitd-num) (:immortal)
  (perform-set-number ch hitd-num (olc-mob-of ch) "mobile hit point dice number"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ 1 200
                      #'(setf hitp-of)))

(defcommand (ch "olc" "mset" "hitd_size") (:immortal)
  (send-to-char ch "Usage: olc mset hitd_size <hitd_size>~%"))

(defcommand (ch "olc" "mset" "hitd_size" hitd-size) (:immortal)
  (perform-set-number ch hitd-size (olc-mob-of ch) "mobile hit point dice size"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ 1 200
                      #'(setf mana-of)))

(defcommand (ch "olc" "mset" "mana") (:immortal)
  (send-to-char ch "Usage: olc mset mana <mana>~%"))

(defcommand (ch "olc" "mset" "mana" mana) (:immortal)
  (perform-set-number ch mana (olc-mob-of ch) "mobile mana"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ 1 32767
                      #'(setf max-mana-of)))

(defcommand (ch "olc" "mset" "move") (:immortal)
  (send-to-char ch "Usage: olc mset move <move>~%"))

(defcommand (ch "olc" "mset" "move" movement) (:immortal)
  (perform-set-number ch movement (olc-mob-of ch) "mobile movement"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ 1 32767
                      #'(setf max-move-of)))

(defcommand (ch "olc" "mset" "baredam") (:immortal)
  (send-to-char ch "Usage: olc mset baredam <baredam>~%"))

(defcommand (ch "olc" "mset" "baredam" baredam) (:immortal)
  (perform-set-number ch baredam (olc-mob-of ch) "mobile bare hand damage"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ 1 125
                      (lambda (val mob)
                        (setf (damnodice-of (shared-of mob)) val))))

(defcommand (ch "olc" "mset" "baredsize") (:immortal)
  (send-to-char ch "Usage: olc mset baredsize <baredsize>~%"))

(defcommand (ch "olc" "mset" "baredsize" baredsize) (:immortal)
  (perform-set-number ch baredsize (olc-mob-of ch) "mobile bare hand damage dice size"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ 1 125
                      (lambda (val mob)
                        (setf (damsizedice-of (shared-of mob)) val))))

(defcommand (ch "olc" "mset" "gold") (:immortal)
  (send-to-char ch "Usage: olc mset gold <gold>~%"))

(defcommand (ch "olc" "mset" "gold" gold) (:immortal)
  (perform-set-number ch gold (olc-mob-of ch) "mobile gold"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ 1 10000000
                      #'(setf gold-of)))

(defcommand (ch "olc" "mset" "exp") (:immortal)
  (send-to-char ch "Usage: olc mset exp <exp>~%"))

(defcommand (ch "olc" "mset" "exp" exp) (:immortal)
  (perform-set-number ch exp (olc-mob-of ch) "mobile experience"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ 1 200000000
                      #'(setf exp-of)))

(defcommand (ch "olc" "mset" "attacktype") (:immortal)
  (send-to-char ch "Usage: olc mset attacktype <attacktype>~%"))

(defcommand (ch "olc" "mset" "attacktype" attacktype) (:immortal)
  (perform-set-enumerated ch attacktype (olc-mob-of ch) "mobile attack type"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ +attack-types+
                      (lambda (val mob)
                        (setf (attack-type-of (shared-of mob)) val))))

(defcommand (ch "olc" "mset" "position") (:immortal)
  (send-to-char ch "Usage: olc mset position <position>~%"))

(defcommand (ch "olc" "mset" "position" position) (:immortal)
  (perform-set-enumerated ch position (olc-mob-of ch) "mobile position"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ +position-types+
                      #'(setf position-of)))

(defcommand (ch "olc" "mset" "sex") (:immortal)
  (send-to-char ch "Usage: olc mset sex <sex>~%"))

(defcommand (ch "olc" "mset" "sex" sex) (:immortal)
  (perform-set-enumerated ch sex (olc-mob-of ch) "mobile sex"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ #("neuter" "male" "female")
                      #'(setf sex-of)))

(defcommand (ch "olc" "mset" "remort_class") (:immortal)
  (send-to-char ch "Usage: olc mset remort_class <remort_class>~%"))

(defcommand (ch "olc" "mset" "remort_class" remort-class) (:immortal)
  (perform-set-enumerated ch remort-class (olc-mob-of ch) "mobile remort class"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ +class-names+
                      #'(setf remort-char-class-of)))

(defcommand (ch "olc" "mset" "cash") (:immortal)
  (send-to-char ch "Usage: olc mset cash <cash>~%"))

(defcommand (ch "olc" "mset" "cash" cash) (:immortal)
  (perform-set-number ch cash (olc-mob-of ch) "mobile cash credits"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ 1 10000000
                      #'(setf cash-of)))

(defcommand (ch "olc" "mset" "hitroll") (:immortal)
  (send-to-char ch "Usage: olc mset hitroll <hitroll>~%"))

(defcommand (ch "olc" "mset" "hitroll" hitroll) (:immortal)
  (perform-set-number ch hitroll (olc-mob-of ch) "mobile hitroll"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ -125 125
                      #'(setf hitroll-of)))

(defcommand (ch "olc" "mset" "damroll") (:immortal)
  (send-to-char ch "Usage: olc mset damroll <damroll>~%"))

(defcommand (ch "olc" "mset" "damroll" damroll) (:immortal)
  (perform-set-number ch damroll (olc-mob-of ch) "mobile damroll"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ -125 125
                      #'(setf damroll-of)))

(defcommand (ch "olc" "mset" "ac") (:immortal)
  (send-to-char ch "Usage: olc mset ac <ac>~%"))

(defcommand (ch "olc" "mset" "ac" ac) (:immortal)
  (perform-set-number ch ac (olc-mob-of ch) "mobile armor class"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ -500 100
                      #'(setf armor-of)))

(defcommand (ch "olc" "mset" "class") (:immortal)
  (send-to-char ch "Usage: olc mset class <class>~%"))

(defcommand (ch "olc" "mset" "class" class) (:immortal)
  (perform-set-enumerated ch class (olc-mob-of ch) "mobile class"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ +class-names+
                      #'(setf char-class-of)))

(defcommand (ch "olc" "mset" "race") (:immortal)
  (send-to-char ch "Usage: olc mset race <race>~%"))

(defcommand (ch "olc" "mset" "race" race) (:immortal)
  (perform-set-enumerated ch race (olc-mob-of ch) "mobile race"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ +player-races+
                      #'(setf race-of)))

(defcommand (ch "olc" "mset" "dpos") (:immortal)
  (send-to-char ch "Usage: olc mset dpos <dpos>~%"))

(defcommand (ch "olc" "mset" "dpos" dpos) (:immortal)
  (perform-set-enumerated ch dpos (olc-mob-of ch) "mobile default position"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ +position-types+
                      (lambda (val mob)
                        (setf (default-pos-of (shared-of mob)) val))))

(defcommand (ch "olc" "mset" "height") (:immortal)
  (send-to-char ch "Usage: olc mset height <height>~%"))

(defcommand (ch "olc" "mset" "height" height) (:immortal)
  (perform-set-number ch height (olc-mob-of ch) "mobile height"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ 1 10000
                      #'(setf height-of)))

(defcommand (ch "olc" "mset" "weight") (:immortal)
  (send-to-char ch "Usage: olc mset weight <weight>~%"))

(defcommand (ch "olc" "mset" "weight" weight) (:immortal)
  (perform-set-number ch weight (olc-mob-of ch) "mobile weight"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ 1 50000
                      #'(setf weight-of)))

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

(defcommand (ch "olc" "mset" "morale") (:immortal)
  (send-to-char ch "Usage: olc mset morale <morale>~%"))

(defcommand (ch "olc" "mset" "morale" morale) (:immortal)
  (perform-set-number ch morale (olc-mob-of ch) "mobile morale"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ 1 125
                      (lambda (val mob)
                        (setf (morale-of (shared-of mob)) val))))

(defcommand (ch "olc" "mset" "move_buf") (:immortal)
  (send-to-char ch "Usage: olc mset move_buf <move_buf>~%"))

(defcommand (ch "olc" "mset" "move_buf" move-buf) (:immortal)
  (perform-set-string ch move-buf (olc-mob-of ch) "mobile move buffer"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+
                      t
                      (lambda (val mob)
                        (setf (move-buf-of (shared-of mob)) val))))

(defcommand (ch "olc" "mset" "lair") (:immortal)
  (send-to-char ch "Usage: olc mset lair <lair>~%"))

(defcommand (ch "olc" "mset" "lair" lair) (:immortal)
  (perform-set-number ch lair (olc-mob-of ch) "mobile lair"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ nil nil
                      (lambda (val mob)
                        (setf (lair-of (shared-of mob)) val))))

(defcommand (ch "olc" "mset" "leader") (:immortal)
  (send-to-char ch "Usage: olc mset leader <leader>~%"))

(defcommand (ch "olc" "mset" "leader" leader) (:immortal)
  (perform-set-number ch leader (olc-mob-of ch) "mobile leader vnum"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ nil nil
                      (lambda (val mob)
                        (setf (leader-of (shared-of mob)) val))))

(defcommand (ch "olc" "mset" "specparam") (:immortal)
  (perform-set-text ch (olc-mob-of ch) "mobile specparam"
                    (vnum-of (olc-mob-of ch))
                    +zone-mobs-approved+
                    (func-param-of (shared-of (olc-mob-of ch)))
                    (lambda (val mob)
                      (setf (func-param-of (shared-of mob)) val))))

(defcommand (ch "olc" "mset" "generation") (:immortal)
  (send-to-char ch "Usage: olc mset generation <generation>~%"))

(defcommand (ch "olc" "mset" "generation" generation) (:immortal)
  (perform-set-number ch generation (olc-mob-of ch) "mobile remort generation"
                      (vnum-of (olc-mob-of ch))
                      +zone-mobs-approved+ 0 10
                      #'(setf remort-gen-of)))

(defcommand (ch "olc" "mset" "loadparam") (:immortal)
  (perform-set-text ch (olc-mob-of ch) "mobile loadparam"
                    (vnum-of (olc-mob-of ch))
                    +zone-mobs-approved+
                    (load-param-of (shared-of (olc-mob-of ch)))
                    (lambda (val mob)
                      (setf (load-param-of (shared-of mob)) val))))

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

(defcommand (ch "olc" "mset" "prog") (:immortal)
  (perform-set-text ch (olc-mob-of ch) "mobile prog"
                    (vnum-of (olc-mob-of ch))
                    +zone-mobs-approved+
                    (prog-text-of (shared-of (olc-mob-of ch)))
                    (lambda (val mob)
                      (setf (prog-text-of (shared-of mob)) val))))

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