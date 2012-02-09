(in-package #:tempus)

(define-condition invalid-creature-file (error)
  ())
(define-condition invalid-equipment-file (error)
  ())

(defun xml-attr (node name &key numeric default numeric-with-nil hex float)
  (let ((val (assoc name (second node) :test #'string=)))
    (cond
      ((null (second val))
       default)
      (numeric-with-nil
       (let ((i (parse-integer (second val))))
         (if (zerop i)
             nil
             i)))
      (numeric
       (parse-integer (second val)))
      (hex
       (parse-integer (second val) :radix 16))
      (float
       (read-from-string (second val)))
      (t
       (second val)))))

(defparameter +rent-codes+ #(undef crash rented cryo forced quit new-char creating remorting))

(defparameter +rent-undef+ 0)
(defparameter +rent-crash+ 1)
(defparameter +rent-rented+ 2)
(defparameter +rent-cryo+ 3)
(defparameter +rent-forced+ 4)
(defparameter +rent-quit+ 5)
(defparameter +rent-new-char+ 6)
(defparameter +rent-creating+ 7)
(defparameter +rent-remorting+ 8)

(defun unserialize-creature (xml)
  (let ((ch (make-instance 'player)))
    (assert (string= (first xml) "creature") nil 'invalid-creature-file)
    (setf (idnum-of ch) (xml-attr xml "idnum" :numeric t))
    (setf (name-of ch) (xml-attr xml "name"))
    (setf (aliases-of ch) (format nil "~(~a .~:*~a~)" (name-of ch)))

    (dolist (node (cddr xml))
      (when (consp node)
        (string-case (first node)
          ("description"
           (setf (fdesc-of ch) (third node)))
          ("title"
           (setf (title-of ch) (or (third node) "")))
          ("points"
           (setf (hitp-of ch) (xml-attr node "hit" :numeric t))
           (setf (mana-of ch) (xml-attr node "mana" :numeric t))
           (setf (move-of ch) (xml-attr node "move" :numeric t))
           (setf (max-hitp-of ch) (xml-attr node "maxhit" :numeric t))
           (setf (max-mana-of ch) (xml-attr node "maxmana" :numeric t))
           (setf (max-move-of ch) (xml-attr node "maxmove" :numeric t)))
          ("money"
           (setf (exp-of ch) (xml-attr node "xp" :numeric t))
           (setf (cash-of ch) (xml-attr node "cash" :numeric t))
           (setf (gold-of ch) (xml-attr node "gold" :numeric t)))
          ("stats"
           (setf (alignment-of ch) (xml-attr node "align" :numeric t))
           (setf (weight-of ch) (xml-attr node "weight" :float t))
           (setf (height-of ch) (xml-attr node "height" :numeric t))
           (setf (race-of ch) (parse-pc-race (xml-attr node "race")))
           (setf (sex-of ch) (string-case (xml-attr node "sex")
                               ("Male" 'male)
                               ("Female" 'female)
                               ("Neuter" 'neuter)))
           (setf (level-of ch) (xml-attr node "level" :numeric t)))
          ("class"
           (setf (remort-gen-of ch) (xml-attr node "gen" :numeric t :default 0))
           (setf (char-class-of ch) (parse-player-class (xml-attr node "name")))
           (setf (remort-char-class-of ch) (parse-player-class (xml-attr node "remort")))
           (setf (total-dam-of ch) (xml-attr node "total_dam" :numeric t :default 0)))
          ("time"
           (setf (login-time-of ch) (unix-to-timestamp
                                     (xml-attr node "last" :numeric t)))
           (setf (birth-time-of ch) (unix-to-timestamp
                                     (xml-attr node "birth" :numeric t)))
           (setf (death-time-of ch) (xml-attr node "death" :numeric-with-nil t))
           (setf (played-time-of ch) (xml-attr node "played" :numeric t)))
          ("carnage"
           (setf (pkills-of ch) (xml-attr node "pkills" :numeric t :default 0))
           (setf (akills-of ch) (xml-attr node "akills" :numeric t :default 0))
           (setf (mobkills-of ch) (xml-attr node "mkills" :numeric t :default 0))
           (setf (deaths-of ch) (xml-attr node "deaths" :numeric t :default 0))
           (setf (reputation-of ch) (xml-attr node "reputation" :numeric t :default 0)))
          ("attr"
           (setf (real-abils-of ch)
                 (make-instance 'char-ability-data
                                :str (xml-attr node "str" :numeric t :default 11)
                                :int (xml-attr node "int" :numeric t :default 11)
                                :wis (xml-attr node "wis" :numeric t :default 11)
                                :dex (xml-attr node "dex" :numeric t :default 11)
                                :con (xml-attr node "con" :numeric t :default 11)
                                :cha (xml-attr node "cha" :numeric t :default 11)))
           (setf (aff-abils-of ch) (copy-abilities (real-abils-of ch))))
          ("condition"
           (setf (aref (conditions-of ch) +full+) (xml-attr node "hunger" :numeric t :default 0))
           (setf (aref (conditions-of ch) +thirst+) (xml-attr node "thirst" :numeric t :default 0))
           (setf (aref (conditions-of ch) +drunk+) (xml-attr node "drunk" :numeric t :default 0)))
          ("player"
           (setf (wimp-level-of ch) (xml-attr node "wimpy" :numeric t :default 0))
           (setf (life-points-of ch) (xml-attr node "lp" :numeric t :default 0))
           (setf (clan-of ch) (xml-attr node "clan" :numeric t :default 0)))
          ("rent"
           (setf (rentcode-of ch) (aref +rent-codes+
                                        (xml-attr node "code" :numeric t)))
           (setf (rent-per-day-of ch) (xml-attr node "perdiem" :numeric t))
           (setf (rent-currency-of ch) (if (zerop (xml-attr node "currency" :numeric t))
                                           'gold
                                           'credits)))
          ("home"
           (setf (home-room-of ch) (xml-attr node "homeroom" :numeric t))
           (setf (load-room-of ch) (xml-attr node "loadroom" :numeric t))
           (when (or (zerop (home-room-of ch))
                     (= (home-room-of ch) -1))
             (setf (home-room-of ch) nil))
           (when (or (zerop (load-room-of ch))
                     (= (load-room-of ch) -1))
             (setf (load-room-of ch) nil)))
          ("bits"
           (setf (plr-bits-of ch) (xml-attr node "flag1" :hex t))
           (setf (plr2-bits-of ch) (xml-attr node "flag2" :hex t)))
          ("prefs"
           (let ((prefs (make-array 64 :element-type 'bit)))
             (let ((num (xml-attr node "flag1" :hex t)))
               (dotimes (bit-index 32)
                 (setf (bit prefs bit-index) (if (logbitp bit-index num) 1 0))))
             (let ((num (xml-attr node "flag2" :hex t)))
               (dotimes (bit-index 32)
                 (setf (bit prefs (+ 31 bit-index))
                       (if (logbitp bit-index num) 1 0))))
             (setf (prefs-of ch) prefs))
           (setf (current-tongue-of ch) (or (find-tongue-idx-by-name (xml-attr node "tongue"))
                                            0)))
          ("affects"
           (setf (aff-flags-of ch) (xml-attr node "flag1" :hex t))
           (setf (aff2-flags-of ch) (xml-attr node "flag2" :hex t))
           (setf (aff3-flags-of ch) (xml-attr node "flag3" :hex t)))
          ("immort"
           (setf (badge-of ch) (xml-attr node "badge"))
           (setf (qlog-level-of ch) (xml-attr node "qlog" :numeric t :default 0))
           (setf (invis-level-of ch) (xml-attr node "invis" :numeric t)))
          ("poofin"
           (setf (poofin-of ch) (third node)))
          ("poofout"
           (setf (poofout-of ch) (third node)))
          ("tongue"
           (let ((tongue-id (find-tongue-idx-by-name (xml-attr node "name")))
                 (level (xml-attr node "level" :numeric t)))
             (assert tongue-id nil 'invalid-character-file)
             (assert level nil 'invalid-character-file)
             (setf (aref (tongues-of ch) tongue-id) level)))
          ("alias"
           (push (list (xml-attr node "alias")
                       (xml-attr node "replace"))
                 (command-aliases-of ch)))
          ("recentkill"
           (push (list (xml-attr node "vnum" :numeric t)
                       (xml-attr node "times" :numeric t))
                 (recently-killed-of ch))))))

    (setf (command-aliases-of ch) (nreverse (command-aliases-of ch)))
    (setf (recently-killed-of ch) (nreverse (recently-killed-of ch)))
    (when (immortal-level-p ch)
      (map-into (skills-of ch) (constantly 100))
      (map-into (tongues-of ch) (constantly 100)))
    ch))

(defun load-player-from-xml (idnum)
  (let ((xml (cxml:parse-file (player-pathname idnum) (cxml-xmls:make-xmls-builder))))
    (unserialize-creature xml)))

(defun load-player-objects (ch)
  (handler-case
      (let ((xml (cxml:parse-file (equipment-pathname (idnum-of ch)) (cxml-xmls:make-xmls-builder))))
        (assert (string= (first xml) "objects") nil 'invalid-equipment-file)
        (dolist (node (cddr xml))
          (when (and (consp node)
                     (string-equal (first node) "object"))
            (unserialize-object nil ch nil node))))
    (file-error ()
      nil)))

(defun unrent (ch)
  (load-player-objects ch)
  ;; TODO: pay-rent
  )

(defun save-player-objects (ch)
  (with-open-file (ouf (equipment-pathname (idnum-of ch))
                       :direction :output
                       :if-exists :rename-and-delete)
    (let ((sink (cxml:make-character-stream-sink ouf :canonical nil)))
      (cxml-xmls:map-node sink
             `("objects" NIL
                ,@(map 'list 'serialize-object
                       (remove nil (carrying-of ch)))
                ,@(map 'list 'serialize-object
                       (remove nil (equipment-of ch)))
                ,@(map 'list 'serialize-object
                       (remove nil (implants-of ch)))
                ,@(map 'list 'serialize-object
                       (remove nil (tattoos-of ch))))
             :include-namespace-uri nil))))

(defmethod save-player-to-xml ((ch mobile))
  (values))

(defun serialize-creature (ch)
  (let ((prefs1 (loop
                   with num = 0
                   for bit from 0 upto 30
                   when (bitp (prefs-of ch) bit)
                   do (setf num (logior num (ash 1 bit)))
                   finally (return num)))
        (prefs2 (loop
                   with num = 0
                   for bit from 0 upto 31
                   when (bitp (prefs-of ch) (+ bit 31))
                   do (setf num (logior num (ash 1 bit)))
                   finally (return num))))
    `("creature"
      (("idnum" ,(idnum-of ch)) ("name" ,(name-of ch)))
      ("points"
       (("hit" ,(hitp-of ch))
        ("mana" ,(mana-of ch))
        ("move" ,(move-of ch))
        ("maxhit" ,(max-hitp-of ch))
        ("maxmana" ,(max-mana-of ch))
        ("maxmove" ,(max-move-of ch))))
      ("money"
       (("gold" ,(gold-of ch))
        ("cash" ,(cash-of ch))
        ("xp" ,(exp-of ch))))
      ("stats"
       (("level" ,(level-of ch))
        ("sex" ,(string-capitalize (sex-of ch)))
        ("race" ,(aref +player-races+ (race-of ch)))
        ("height" ,(height-of ch))
        ("weight" ,(weight-of ch))
        ("align" ,(alignment-of ch))))
      ("class"
       (("name" ,(aref +class-names+ (char-class-of ch)))
       ,@(when (is-remort ch)
               `(("remort" ,(aref +class-names+ (remort-char-class-of ch)))
                 ("gen" ,(remort-gen-of ch))))
       ,@(when (is-cyborg ch)
         `(,@(unless (zerop (broken-component-of ch))
               `(("broken" ,(broken-component-of ch))))
             ,@(unless (zerop (total-dam-of ch))
               `(("total_dam" ,(total-dam-of ch))))))
       ,@(when (and (= (char-class-of ch) +class-mage+)
                    (plusp (aref (skills-of ch) +spell-mana-shield+)))
               `(("manash_low" ,(mana-shield-low-of ch))
                 ("manash_pct" ,(mana-shield-pct-of ch))))))
      ("time"
       (("birth" ,(timestamp-to-unix (birth-time-of ch)))
        ("death" ,(if (death-time-of ch)
                      (timestamp-to-unix (death-time-of ch))
                      0))
        ("played" ,(played-time-of ch))
        ("last" ,(timestamp-to-unix (login-time-of ch)))))
      ("carnage"
       (("pkills" ,(pkills-of ch))
        ("akills" ,(akills-of ch))
        ("mkills" ,(mobkills-of ch))
        ("deaths" ,(deaths-of ch))
        ("reputation" ,(reputation-of ch))))
      ("attr"
       (("str" ,(real-str-of ch))
        ("int" ,(real-int-of ch))
        ("wis" ,(real-wis-of ch))
        ("dex" ,(real-dex-of ch))
        ("con" ,(real-con-of ch))
        ("cha" ,(real-cha-of ch))))
      ("condition"
       (("hunger" ,(aref (conditions-of ch) +full+))
        ("thirst" ,(aref (conditions-of ch) +thirst+))
        ("drunk" ,(aref (conditions-of ch) +drunk+))))
      ("player"
       (("wimpy" ,(wimp-level-of ch))
        ("lp" ,(life-points-of ch))
        ("clan" ,(clan-of ch))))
      ("rent"
       (("code" ,(position (rentcode-of ch) +rent-codes+))
        ("perdiem" ,(rent-per-day-of ch))
        ("currency" ,(if (eql (rent-currency-of ch) 'gold) 0 1))
        ,@(when (and (link-of ch)
                     (or (eql (rentcode-of ch) 'creating)
                         (eql (rentcode-of ch) 'remorting)))
            `(("state" ,(string-downcase (state-of (link-of ch))))))))
      ("home"
       (("town" ,(hometown-of ch))
        ("homeroom" ,(or (home-room-of ch) 0))
        ("loadroom" ,(or (load-room-of ch) 0))))
      ,@(when (or (plusp (quest-id-of ch))
                  (plusp (qp-allowance-of ch))
                  (plusp (imm-qp-of ch)))
              `(("quest"
                 (,@(when (plusp (quest-id-of ch))
                          `(("current" ,(quest-id-of ch))))
                  ,@(when (immortal-level-p ch)
                          `(("allowance" ,(qp-allowance-of ch))))
                  ,@(when (plusp (imm-qp-of ch))
                          `(("points" ,(imm-qp-of ch))))))))
      ("bits"
       (("flag1" ,(write-to-string (plr-bits-of ch) :base 16))
        ("flag2" ,(write-to-string (plr2-bits-of ch) :base 16))))
      ,@(when (plr-flagged ch +plr-frozen+)
              `(("frozen"
                 ("thaw_time" ,(timestamp-to-unix (thaw-time-of ch)))
                 ("freezer_id" ,(freezer-id-of ch)))))
      ("prefs"
       (("flag1" ,(write-to-string prefs1 :base 16))
        ("flag2" ,(write-to-string prefs2 :base 16))
        ("tongue" ,(name-of (gethash (current-tongue-of ch) *tongues*)))))
      ("affects"
       (("flag1" ,(write-to-string (aff-flags-of ch) :base 16))
        ("flag2" ,(write-to-string (aff2-flags-of ch) :base 16))
        ("flag3" ,(write-to-string (aff3-flags-of ch) :base 16))))
      ,@(loop for spec across (weap-spec-of ch)
             when spec
             collect `("weaponspec" (("vnum" ,(vnum-of spec))
                                     ("level" ,(level-of spec)))))
      ,@(when (title-of ch)
          `(("title" nil ,(title-of ch))))
      ,@(when (immortal-level-p ch)
          `(("immort"
             (("badge" ,(badge-of ch))
              ("qlog" ,(qlog-level-of ch))
              ("invis" ,(invis-level-of ch))))))
      ,@(when (poofin-of ch)
          `(("poofin" nil ,(poofin-of ch))))
      ,@(when (poofout-of ch)
          `(("poofout" nil ,(poofout-of ch))))
      ,@(when (fdesc-of ch)
          `(("description" nil ,(fdesc-of ch))))
      ,@(mapcar (lambda (alias)
                  `("alias"
                    (("type" ,(if (find #\$ (second alias)) 1 0))
                     ("alias" ,(first alias))
                     ("replace" ,(second alias)))))
             (command-aliases-of ch))
      ,@(mapcar (lambda (aff)
                  `("affect"
                    (("type" ,(kind-of aff))
                     ("duration" ,(duration-of aff))
                     ("modifier" ,(modifier-of aff))
                     ("location" ,(location-of aff))
                     ("level" ,(level-of aff))
                     ("instant" ,(if (is-instant-of aff) "yes" "no"))
                     ("affbits" ,(write-to-string (bitvector-of aff) :base 16))
                     ("index" ,(aff-index-of aff))
                     ("owner" ,(owner-of aff)))))
                (affected-of ch))
      ,@(unless (immortal-level-p ch)
          (append
           (loop for skill from 0 upto +max-skills+
              unless (zerop (aref (skills-of ch) skill))
              collect `("skill" (("name" (spell-to-str skill))
                                 ("level" (aref (skills-of ch) skill)))))
           (loop for tongue from 0 upto (1- +max-tongues+)
              unless (zerop (aref (tongues-of ch) tongue))
              collect `("tongue" (("name" ,(tongue-name tongue))
                                  ("level" ,(aref (tongues-of ch) tongue)))))
           (loop for tuple in (recently-killed-of ch)
              collect `("recentkill"
                        (("vnum" ,(first tuple))
                         ("times" ,(second tuple)))))
           (loop for griev in (grievances-of ch)
              collect `("grievance"
                        (("time" ,(timestamp-to-unix (time-of griev)))
                         ("player" ,(player-idnum-of griev))
                         ("reputation" ,(reputation-of griev))
                         ("kind" ,(string-downcase (kind-of griev)))))))))))

(defmethod save-player-to-xml ((ch player))
  ;; Update the amount of time played
  (let ((now (now)))
    (incf (played-time-of ch) (floor (timestamp-difference now (login-time-of ch))))
    (setf (login-time-of ch) now))

  ;; Remove all spell affects
  (dolist (aff (affected-of ch))
    (affect-modify ch (location-of aff) (modifier-of aff)
                   (bitvector-of aff) (aff-index-of aff) nil))

  (with-open-file (ouf (player-pathname (idnum-of ch))
                       :direction :output
                       :if-exists :rename-and-delete
                       :if-does-not-exist :create)
    (let ((sink (cxml:make-character-stream-sink ouf :canonical nil)))
      (cxml-xmls:map-node sink (serialize-creature ch)
                          :include-namespace-uri nil)))

  ;; Reinstate all spell affects
  (dolist (aff (affected-of ch))
    (affect-modify ch (location-of aff) (modifier-of aff)
                   (bitvector-of aff) (aff-index-of aff) t))
  (affect-total ch)

  (setf (hitp-of ch) (min (hitp-of ch) (max-hitp-of ch)))
  (setf (mana-of ch) (min (mana-of ch) (max-mana-of ch)))
  (setf (move-of ch) (min (move-of ch) (max-move-of ch)))

  (save-player-objects ch)
  (values))