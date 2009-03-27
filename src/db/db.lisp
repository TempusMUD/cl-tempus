(in-package :tempus)

(defparameter +index-file+ "index")       ; index of world files
(defparameter +mindex-file+ "index.mini") ; ... and for mini-mud-mode
(defparameter +tindex-file+ "index.test") ; ... and for test-mud-mode
(defparameter +wld-prefix+ "world/wld")   ; room definitions
(defparameter +mob-prefix+ "world/mob")   ; monster prototypes
(defparameter +obj-prefix+ "world/obj")   ; object prototypes
(defparameter +zon-prefix+ "world/zon")   ; zon defs & command tables
(defparameter +xml-prefix+ "world/xml")   ;

(defparameter +credits-file+ "text/credits") ; for the 'credits' command
(defparameter +motd-file+ "text/motd")  ; messages of the day / mortal
(defparameter +ansi-motd-file+ "text/ansi-motd") ; messages of the day / mortal
(defparameter +imotd-file+ "text/imotd") ; messages of the day / immort
(defparameter +ansi-imotd-file+ "text/ansi-imotd") ; messages of the day / imm
(defparameter +help-kwrd-file+ "text/help-table")  ; for HELP <keywrd>
(defparameter +help-kwrd-wiz+ "text/help-table-wiz") ; for HELP <keywrd>
(defparameter +help-page-file+ "text/help")          ; for HELP <CR>
(defparameter +info-file+ "text/info")               ; for INFO
(defparameter +wizlist-file+ "text/wizlist")         ; for WIZLIST
(defparameter +ansi-wizlist-file+ "text/ansi-wizlist") ; for WIZLIST
(defparameter +immlist-file+ "text/immlist")           ; for IMMLIST
(defparameter +ansi-immlist-file+ "text/ansi-immlist") ; for IMMLIST
(defparameter +background-file+ "text/background") ; for the background story
(defparameter +policies-file+ "text/policies") ; player policies/rules
(defparameter +handbook-file+ "text/handbook") ; handbook for new immorts
(defparameter +areas-low-file+ "text/areas-low") ; list of areas
(defparameter +areas-mid-file+ "text/areas-mid") ;
(defparameter +areas-high-file+ "text/areas-high") ;
(defparameter +areas-remort-file+ "text/areas-remort") ;
(defparameter +olc-guide-file+ "text/olc-creation-guide") ; tips for creators
(defparameter +quest-guide-file+ "text/quest-guide") ; quest guidelines
(defparameter +quest-list-file+ "text/quest-list")   ; list of quests

(defparameter +idea-file+ "misc/ideas")    ; for the 'idea'-command
(defparameter +typo-file+ "misc/typos")    ;         'typo'
(defparameter +bug-file+ "misc/bugs")      ;         'bug'
(defparameter +mess-file+ "misc/messages") ; damage messages
(defparameter +socmess-file+ "lib/misc/socials") ; messgs for social acts
(defparameter +xname-file+ "misc/xnames") ; invalid name substrings
(defparameter +nasty-file+ "misc/nasty") ; nasty words for public comm

(defparameter +player-file+ "etc/players")    ; the player database
(defparameter +mail-file+ "etc/plrmail")      ; for the mudmail system
(defparameter +ban-file+ "etc/badsites")      ; for the siteban system
(defparameter +hcontrol-file+ "etc/hcontrol") ; for the house system
(defparameter +alias-dir+ "plralias/")

(defparameter +cmd-log-file+ "cmd-log")

(defparameter +index-file+ "index")
(defparameter +mindex-file+ "index.mini")
(defparameter +null-mob-shared+ nil)
(defparameter +null-obj-shared+ nil)
(defparameter +dummy-mob+ nil)

(defvar *credits* "")
(defvar *motd* "")
(defvar *ansi-motd* "")
(defvar *imotd* "")
(defvar *ansi-imotd* "")
(defvar *info* "")
(defvar *policies* "")
(defvar *handbook* "")
(defvar *olc-guide* "")
(defvar *quest-guide* "")

(defvar *rooms* (make-hash-table))
(defvar *mobile-prototypes* (make-hash-table))
(defvar *object-prototypes* (make-hash-table))
(defvar *characters* nil)
(defvar *character-map* (make-hash-table))
(defvar *zone-table* nil)
(defvar *default-quad-zone*)
(defvar *top-of-world* 0)
(defvar *boot-time* nil)
(defvar *reset-q* nil)
(defvar *top-unique-id* 0)
(defvar *unique-id-changed* t)
(defvar *time-info* nil)
(defvar *current-mob-idnum* 1)

(defparameter *no-specials* nil)
(defparameter *welcome-message* nil)

;; mud-life time
(defparameter +secs-per-mud-hour+ 60)
(defparameter +secs-per-mud-day+ (* 24 +secs-per-mud-hour+))
(defparameter +secs-per-mud-month+ (* 35 +secs-per-mud-day+))
(defparameter +secs-per-mud-year+ (* 16 +secs-per-mud-month+))

(defvar *room-nr* 0)

(defun real-zone (zone-nr)
  (find zone-nr *zone-table* :key 'number-of))

(defun renum-world ()
  nil)

(defun renum-zone-table ()
  (setf *zone-table* (sort *zone-table* #'< :key 'number-of))
  (dolist (zone *zone-table*)
    (setf (world-of zone) (sort (world-of zone) #'< :key 'number-of))))

(defun xml-boot ()
  nil)

(defun clear-world ()
  (close-all-cxns)

  ;; Clear hashes
  (dolist (hash (list *rooms*
                      *mobile-prototypes*
                      *object-prototypes*
                      *character-map*
                      *account-idnum-cache*
                      *account-name-cache*))
    (clrhash hash))

  (setf *default-quad-zone* nil)
  (setf *zone-table* nil)
  (setf *object-list* nil)
  (setf *characters* nil)
  (setf *top-of-world* 0)
  (sb-ext:gc :full t))

(defun boot-world ()
  (slog "Initializing world")
  (clear-world)

  (slog "Loading zone table.")
  (index-boot :zon)

  (slog "Loading rooms.")
  (index-boot :wld)

  (slog "Loading XML data.")
  (xml-boot)

  (slog "Renumbering rooms.")
  (renum-world)

  (slog "Loading mobs and generating index.")
  (index-boot :mob)

  (slog "Loading objs and generating index.")
  (index-boot :obj)

  (slog "Renumbering zone table.")
  (renum-zone-table)

  ;; For quad damage bamfing
  (unless (setf *default-quad-zone* (real-zone 25))
    (setf *default-quad-zone* *zone-table*)))

(defun reset-all-zones ()
    (dolist (zone *zone-table*)
      (slog "Resetting ~a (rms ~d-~d)"
            (name-of zone)
            (* (number-of zone) 100)
            (top-of zone))
      (reset-zone zone)))

(defun boot-db ()
  (slog "Boot db -- BEGIN.")

  (slog "Resetting the game time:")
  (reset-time)

  (unless (and *database*
               (connected-p *database*))
    (set-local-time-cl-postgres-readers)
    (slog "Connecting to postgres.")
    (apply 'connect-toplevel
           (if *production-mode*
               '("tempus" "realm" "" "localhost")
               '("devtempus" "realm" "tarrasque" "localhost"))))

  (when *production-mode*
    (slog "Vacuuming old database transactions")
    (execute (:vacuum :full :analyze)))

  (setf *top-unique-id*  (query (:select 'last-value :from 'unique-id) :single))
  (slog "Top unique object id = ~d" *top-unique-id*)

  (account-boot)
#+nil  (load-bounty-data)
  (slog "Reading credits, bground, info & motds.")
  (setf *credits* (snarf-file +credits-file+ :ignore-errors t)
        *motd* (snarf-file +motd-file+ :ignore-errors t)
        *ansi-motd* (snarf-file +ansi-motd-file+ :ignore-errors t)
        *imotd* (snarf-file +imotd-file+ :ignore-errors t)
        *ansi-imotd* (snarf-file +ansi-imotd-file+ :ignore-errors t)
        *info* (snarf-file +info-file+ :ignore-errors t)
        *policies* (snarf-file +policies-file+ :ignore-errors t)
        *handbook* (snarf-file +handbook-file+ :ignore-errors t)
        *olc-guide* (snarf-file +olc-guide-file+ :ignore-errors t)
        *quest-guide* (snarf-file +quest-guide-file+ :ignore-errors t))

  (boot-tongues)
  (boot-dynamic-text)
  (boot-world)

  (reset-zone-weather)
  (slog "Booting clans.")
  (boot-clans)

  (slog "Booting quests.")
  (boot-quests)

  (slog "Loading fight messages.")
  (load-messages)

  (slog "Loading social messages.")
  (boot-social-messages +socmess-file+)

  (slog "Assigning function pointers:")

  (unless *no-specials*
    (slog "   Mobiles.")
    (assign-mobiles)
    (slog "   Objects.")
    (assign-objects)
    (slog "   Rooms.")
    (assign-rooms)
    (slog "   Artisans.")
    (assign-artisans))
  (slog "   Spells.")
  (boot-spells)

  (slog "Sorting command list and spells.")
  (sort-commands)
  (sort-spells)
  (sort-skills)

  (security-load-groups)

  (slog "Compiling progs.")
  (compile-all-progs)

  (slog "reading banned site, invalid-name, and NASTY word lists.")
  (load-banned)
  (read-invalid-list)
  (read-nasty-list)

  (slog "Reading paths.")
  (load-paths)

  (slog "Booting timewarp data.")
  (boot-timewarp-data)

  (if *mini-mud*
      (slog "HOUSE: Mini-mud detected. Houses not loading.")
      (progn
        (slog "HOUSE: Booting houses.")
        (housing-load)
        (housing-count-objects)))

  (unless *no-initial-zreset*
    (reset-all-zones))

  (slog "Booting help system.")
  (if (boot-help-system)
      (slog "Help system boot succeeded.")
      (slog "Help system boot FAILED."))

  (setf *reset-q* nil)
  (setf *boot-time* (now))

  (slog "Boot db -- DONE."))

(defun reset-time ()
  "Reset the time in the game"
  (let* ((epoch 650336715)
         (now (timestamp-to-unix (now))))
    (setf *lunar-day* (mod (floor (- now epoch) +secs-per-mud-day+) 24))
    (multiple-value-bind (hour day mon year)
        (mud-time-passed now epoch)
      (setf *time-info* (make-instance 'mud-time
                                       :hour hour
                                       :day day
                                       :month mon
                                       :year year))
      (slog "   Current Gametime (global): ~dH ~dD ~dM ~dY."
            hour day mon year))
    (slog "   Current lunar day: ~d (~a)"
          *lunar-day* (lunar-phase *lunar-day*))))

(defun count-hash-records (prefix fname)
  (with-open-file (inf (tempus-path (format nil "lib/~a/~a" prefix fname))
                       :direction :input)
    (loop for line = (get-line inf)
          while line
          count (and (string/= line "")
                     (char= (char line 0) #\#)))))

(defun index-boot (mode)
  (let* ((prefix (case mode
                  (:wld +wld-prefix+)
                  (:mob +mob-prefix+)
                  (:obj +obj-prefix+)
                  (:zon +zon-prefix+)
                  (t
                   (error "Unknown subcommand ~a to index-boot!" mode))))
         (index-filename (if *mini-mud* +mindex-file+ +index-file+))
         (path (tempus-path (format nil "lib/~a/~a" prefix index-filename)))
         (index-count 0)
         (rec-count 0))

    ;;; First, count the records in the file so we can cons
    (with-open-file (index path
                           :direction :input
                           :if-does-not-exist nil

)
      (unless index
        (error "Error opening index file '~a'~%" path))

      (loop for buf1 = (read-line index nil :eof)
            until (or (eql buf1 :eof) (char= (char buf1 0) #\$)) do
            (incf index-count)
            (if (eql mode :zon)
                (incf rec-count)
                (incf rec-count (count-hash-records prefix buf1))))

      (when (zerop rec-count)
        (errlog "boot error - 0 records counted"))

      (incf rec-count)

      (case mode
        (:mob
         (setf +null-mob-shared+ (make-instance 'mob-shared-data))
         (setf (vnum-of +null-mob-shared+) -1)
         (setf (number-of +null-mob-shared+) 0)
         (setf (func-of +null-mob-shared+) nil)
         (setf (proto-of +null-mob-shared+) nil)
         (setf (move-buf-of +null-mob-shared+) nil))
        (:obj
         (setf +null-obj-shared+ (make-instance 'obj-shared-data))
         (setf (vnum-of +null-obj-shared+) -1)
         (setf (number-of +null-obj-shared+) 0)
         (setf (house-count-of +null-obj-shared+) 0)
         (setf (func-of +null-obj-shared+) nil)
         (setf (proto-of +null-obj-shared+) nil)))

      (file-position index 0)

      (loop for buf1 = (read-line index nil :eof)
            until (or (eql buf1 :eof) (char= (char buf1 0) #\$)) do
            (with-open-file (inf (tempus-path (format nil "lib/~a/~a" prefix buf1))
                                 :direction :input)
              (if (eql mode :zon)
                  (load-zones inf buf1)
                  (discrete-load inf buf1 mode)))))))

(defun discrete-load (inf fname mode)
  (declare (ignore fname))
  (let ((line nil)
        (nr -1))
    (loop
     (when (or (not (eql mode :obj)) (minusp nr))
       (setf line (get-line inf))
       (when (null line)
         (error "Format error after ~a #~d~%" mode nr)))

     (cond
       ((eql (char line 0) #\$)
        (return))
       ((eql (char line 0) #\#)
        (let ((result (scan #/^#(\d+)$/ line)))
          (unless result
            (error "Format error after ~a #~d~%" mode nr))
          (setf nr (parse-integer (regref result 1))))
        (when (>= nr 99999)
          (return))

        (case mode
          (:wld
           (parse-room inf nr))
          (:mob
           (parse-mobile inf nr))
          (:obj
           (setf line (parse-object inf nr)))))
       (t
        (error "Format error in ~a file near ~a #~d~%Offending line: '~a'~%"
               mode mode nr line))))))

(defun asciiflag-to-bits (flag &key (start 0) end)
  (loop
     with result = 0
     for idx from start upto (1- (or end (length flag)))
     as char = (char flag idx)
     as code = (char-code char)
     when (alpha-char-p char)
     do (setf (ldb (byte 1 (if (lower-case-p char)
                               (- code (char-code #\a))
                               (+ 26 (- code (char-code #\A)))))
                   result)
              1)
     finally (return result)))

(defun bits-to-asciiflag (bits)
  (if (zerop bits)
      "0"
      (with-output-to-string (str)
        (loop
           for bit from 0 upto 25
           when (logtest bits (ash 1 bit))
           do (write-char (code-char (+ (char-code #\a) bit)) str))
        (loop
           for bit from 26 upto 31
           when (logtest bits (ash 1 bit))
           do (write-char (code-char (+ (char-code #\A) (- bit 26))) str)))))

(defun zone-containing-number (num)
  (find-if (lambda (zone)
             (<= (* (number-of zone) 100) num (top-of zone)))
           *zone-table*))

(defun parse-room (inf vnum-nr)
  (let ((zone (zone-containing-number vnum-nr)))
    (unless zone
      (error "Room ~d is outside of any zone.~%" vnum-nr))

    (let ((room (make-instance 'room-data
                              :number vnum-nr
                              :zone zone)))
      (setf (name-of room) (fread-string inf))
      (setf (description-of room) (fread-string inf))
      (setf (sounds-of room) nil)

      (let* ((line (get-line inf))
             (result (scan #/^\s*(\d+)\s+(\w+)\s+(\d+)\s*/ line)))
          (unless result
            (error "Format error in room #~d~%" vnum-nr))

          (setf (flags-of room) (asciiflag-to-bits (regref result 2)))
          (setf (terrain-of room) (parse-integer (regref result 3)))
          (setf zone (real-zone (parse-integer (regref result 1)))))

      (unless zone
        (error "Room ~d outside of any zone.~%" vnum-nr))

      (loop for line = (get-line inf) do
       (unless line
         (error "Format error in room #~d (expecting D/E/S)" vnum-nr))

       (case (char line 0)
         (#\R
          (setf (prog-text-of room) (fread-string inf)))
         (#\O
          (setf (max-occupancy-of room) (parse-integer line :start 1)))
         (#\D
          (setup-dir inf room (parse-integer line :start 1)))
         (#\E
          (let ((new-descr (make-instance 'extra-descr-data)))
            (setf (keyword-of new-descr) (fread-string inf))
            (setf (description-of new-descr) (fread-string inf))
            (setf (ex-description-of room)
                  (append (ex-description-of room) (list new-descr)))))
         (#\L
          (setf (sounds-of room) (fread-string inf)))
         (#\F
          (setf line (get-line inf))
          (let ((result (scan #/^(\d+)\s+(\d+)\s+(\d+)/ line)))
            (unless result
              (error "Flow field incorrect in room #~d.~%" vnum-nr))

            (when (plusp (flow-speed-of room))
              (errlog "Multiple flow states assigned to room #~d.~%" vnum-nr))

            (let ((flow-dir (parse-integer (regref result 1)))
                  (flow-speed (parse-integer (regref result 2)))
                  (flow-type (parse-integer (regref result 3))))
              (unless (<= 0 flow-dir 8)
                (error "Direction '~d' in room #~d flow field BUNK!~%"
                        flow-dir vnum-nr))

              (when (minusp flow-speed)
                (error "Negative speed in room #~d flow field!~%" vnum-nr))

              (unless (<= 0 flow-type 18)
                (errlog "Illegal flow type ~a in room #~d.~%" flow-type vnum-nr)
                (setf flow-type :none))

              (setf (flow-dir-of room) flow-dir
                    (flow-speed-of room) flow-speed
                    (flow-kind-of room) flow-type))))
         (#\Z
          (let ((new-search (make-instance 'special-search-data)))
            (setf (trigger-of new-search) (fread-string inf)
                  (keywords-of new-search) (fread-string inf)
                  (to-vict-of new-search) (fread-string inf)
                  (to-room-of new-search) (fread-string inf)
                  (to-remote-of new-search) (fread-string inf))

            (unless (setf line (get-line inf))
              (error "Search error in room #~d." vnum-nr))

            (let ((result (scan #/^(\d+)\s+([-\d]+)\s+([-\d]+)\s+([-\d]+)\s+([-\d]+)(?:\s+(\d+))?/ line)))
              (setf (command-of new-search) (parse-integer (regref result 1))
                    (aref (arg-of new-search) 0) (parse-integer (regref result 2))
                    (aref (arg-of new-search) 1) (parse-integer (regref result 3))
                    (aref (arg-of new-search) 2) (parse-integer (regref result 4))
                    (flags-of new-search) (parse-integer (regref result 5)))
              (let ((str (regref result 6)))
                (setf (fail-chance-of new-search) (if str (parse-integer str) 0))))

            ;; place the search at the end of the list
            (setf (searches-of room) (nconc (searches-of room) (list new-search)))))
         (#\P
          (setf (func-param-of room) (fread-string inf)))
         (#\S                           ; end of room
          (setf *top-of-world* (incf *room-nr*))
          (when (real-room vnum-nr)
            (errlog "Duplicate room ~d detected.  Ignoring second instance." vnum-nr)
            (return))
          (setf (gethash vnum-nr *rooms*) room)
          (push room (world-of zone))
          (return-from parse-room)))))))

(defun setup-dir (inf room dir)
  (when (>= dir +num-dirs+)
    (error "Room direction > +num-dirs+ in room #~d" (number-of room)))

  (let ((room-dir (make-instance 'room-direction-data)))
    (setf (aref (dir-option-of room) dir) room-dir)
    (setf (description-of room-dir) (fread-string inf))
    (setf (keyword-of room-dir) (fread-string inf))

    (let ((line (get-line inf)))

      (unless line
        (error "Format error room #~d, direction D~d." (number-of room) dir))

      (multiple-value-bind (start end reg-starts reg-ends)
          (cl-ppcre:scan #/^(\S+)\s+([\d-]+)\s+([-\d]+)/ line)
        (declare (ignore start end))
        (setf (exit-info-of room-dir) (asciiflag-to-bits line
                                                      :start (aref reg-starts 0)
                                                      :end (aref reg-ends 0)))
        (setf (key-of room-dir) (parse-integer line
                                               :start (aref reg-starts 1)
                                               :end (aref reg-ends 1)))
        (setf (to-room-of room-dir) (parse-integer line
                                                   :start (aref reg-starts 2)
                                                   :end (aref reg-ends 2)))))))

(defun compile-all-progs ()
  "Compiles all the progs on the mud."
  ;; Compile all room progs
  ;; Compile all mob progs
  nil)

(defun set-physical-attribs (ch)
  (assert ch)
  (assert (level-of ch))
  (assert (str-of ch))
  (assert (int-of ch))
  (assert (weight-of ch))

  (setf (max-mana-of ch) (max 100 (* (level-of ch) 8))
        (max-move-of ch) (max 100 (* (level-of ch) 16)))
  (cond
    ((or (= (race-of ch) +race-human+)
         (= (race-of ch) +race-mobile+)
         (is-humanoid ch))
     (setf (weight-of ch) (+ (random-range 130 180) (* (str-of ch) 2))
           (height-of ch) (+ (random-range 140 180) (floor (weight-of ch) 8))))
    ((= (race-of ch) +race-rotarian+)
     (setf (weight-of ch) (random-range 300 450)
           (height-of ch) (random-range 200 325)))
    ((= (race-of ch) +race-griffin+)
     (setf (weight-of ch) (random-range 1500 2300)
           (height-of ch) (random-range 400 550)))
    ((= (race-of ch) +race-dwarf+)
     (setf (weight-of ch) (+ (random-range 120 160) (* (str-of ch) 2))
           (height-of ch) (+ (random-range 100 115) (floor (weight-of ch) 16))
           (str-of (real-abils-of ch)) 15))
    ((or (= (race-of ch) +race-elf+)
         (= (race-of ch) +race-drow+))
     (setf (weight-of ch) (+ (random-range 120 180) (* (str-of ch) 2))
           (height-of ch) (+ (random-range 140 155) (floor (weight-of ch) 8))
           (int-of (real-abils-of ch)) 15))
    ((or (= (race-of ch) +race-half-orc+) (is-orc ch))
     (setf (weight-of ch) (+ (random-range 120 180) (* (str-of ch) 2))
           (height-of ch) (+ (random-range 120 190) (floor (weight-of ch) 8))))
    ((or (= (race-of ch) +race-halfling+) (is-goblin ch))
     (setf (weight-of ch) (+ (random-range 110 150) (* (str-of ch) 2))
           (height-of ch) (+ (random-range 100 125) (floor (weight-of ch) 8))))
    ((= (race-of ch) +race-wemic+)
     (setf (weight-of ch) (+ (random-range 500 560) (* (str-of ch) 2)))))

  (when (eql (sex-of ch) +sex-female+)
    (setf (weight-of ch) (floor (* (weight-of ch) 3) 4)
          (height-of ch) (floor (* (height-of ch) 3) 4))))

(defun parse-espec (line mobile nr)
  (let* ((result (scan #/^([^:]+):\s*(.*)$/ line))
         (value (regref result 2))
         (num-arg (parse-integer value :junk-allowed t)))
    (string-case (regref result 1)
      ("BareHandAttack"
       (setf (attack-type-of (shared-of mobile)) (pin num-arg 0 99)))
      ("Move_buf"
       (setf (move-buf-of (shared-of mobile)) value))
      ("Str"
       (setf (str-of (real-abils-of mobile)) (pin num-arg 3 25)))
      ("StrAdd"
       (setf (str-add-of (real-abils-of mobile)) (pin num-arg 0 100)))
      ("Int"
       (setf (int-of (real-abils-of mobile)) (pin num-arg 3 25)))
      ("Wis"
       (setf (wis-of (real-abils-of mobile)) (pin num-arg 3 25)))
      ("Dex"
       (setf (dex-of (real-abils-of mobile)) (pin num-arg 3 25)))
      ("Con"
       (setf (con-of (real-abils-of mobile)) (pin num-arg 3 25)))
      ("Cha"
       (setf (cha-of (real-abils-of mobile)) (pin num-arg 3 25)))
      ("MaxMana"
       (setf (max-mana-of mobile) (pin num-arg 0 4000)))
      ("MaxMove"
       (setf (max-move-of mobile) (pin num-arg 0 4000)))
      ("Height"
       (setf (height-of mobile) (pin num-arg 0 10000)))
      ("Weight"
       (setf (weight-of mobile) (pin num-arg 0 10000)))
      ("RemortClass"
       (setf (remort-char-class-of mobile) (pin num-arg 0 1000)))
      ("Class"
       (setf (char-class-of mobile) (pin num-arg 0 1000)))
      ("Race"
       (setf (race-of mobile) (pin num-arg 0 1000)))
      ("Credits"
       (setf (cash-of mobile) (pin num-arg 0 1000000)))
      ("Cash"
       (setf (cash-of mobile) (pin num-arg 0 1000000)))
      ("Morale"
       (setf (morale-of (shared-of mobile)) (pin num-arg 0 120)))
      ("Lair"
       (setf (lair-of (shared-of mobile)) (pin num-arg -99999 99999)))
      ("Leader"
       (setf (leader-of (shared-of mobile)) (pin num-arg -99999 99999)))
      ("Generation"
       (setf (remort-gen-of mobile) (pin num-arg 0 1000000)))
      ("CurTongue"
       (setf (current-tongue-of mobile) (pin num-arg 0 1000000)))
      ("KnownTongue"
       (setf (aref (tongues-of mobile) num-arg) 100))
      ("CurLang"
       ;; Deprecated conversion
       (setf (current-tongue-of mobile) (1+ (pin num-arg 0 1000000))))
      ("KnownLang"
       ;; Deprecated conversion
       (dotimes (bit 32)
         (when (logtest num-arg bit)
           (setf (aref (tongues-of mobile) (1+ bit)) 100))))
      (t
       (errlog "WARNING: Unrecognized espec keyword ~a in mobile #~d"
               (regref result 1)
               nr)))))

(defun parse-enhanced-mobile (inf nr mobile)
  (loop for line = (get-line inf)
     until (and line (string= line "E")) do
     (cond
       ((string-equal line "SpecParam:")
        (setf (func-param-of (shared-of mobile)) (fread-string inf)))
       ((string-equal line "LoadParam:")
        (setf (load-param-of (shared-of mobile)) (fread-string inf)))
       ((string-equal line "Prog:")
        (setf (prog-text-of (shared-of mobile)) (fread-string inf)))
       ((string-equal line "#" :end2 1)
        (error "Unterminated E section in mob #~d" nr))
       (t
        (parse-espec line mobile nr)))))

(defun parse-simple-mobile (inf nr mobile)
  (loop for line = (get-line inf)
     until (and line (string= line "E")) do
     (cond
       ((string-equal line "SpecParam:")
        (setf (func-param-of (shared-of mobile)) (fread-string inf)))
       ((string-equal line "LoadParam:")
        (setf (load-param-of (shared-of mobile)) (fread-string inf)))
       ((string-equal line "Prog:")
        (setf (prog-text-of (shared-of mobile)) (fread-string inf)))
       ((string-equal line "#" :end2 1)
        (error "Unterminated E section in mob #~d" nr))
       (t
        (parse-espec line mobile nr)))))

(defun parse-mobile (inf nr)
  (declare (ignorable inf nr))
  (let ((mobile (make-instance 'mobile
                               :shared (make-instance 'mob-shared-data)))
        (mob-type nil))
    (setf (vnum-of (shared-of mobile)) nr
          (number-of (shared-of mobile)) 0
          (proto-of (shared-of mobile)) mobile
          (aliases-of mobile) (fread-string inf)
          (name-of mobile) (fread-string inf)
          (ldesc-of mobile) (string-right-trim '(#\return #\newline)
                                               (fread-string inf))
          (fdesc-of mobile) (fread-string inf)
          (str-of (real-abils-of mobile)) 11
          (int-of (real-abils-of mobile)) 11
          (wis-of (real-abils-of mobile)) 11
          (dex-of (real-abils-of mobile)) 11
          (con-of (real-abils-of mobile)) 11
          (cha-of (real-abils-of mobile)) 11
          (weight-of mobile) 200
          (height-of mobile) 198)

    (let* ((line (get-line inf))
           (result (scan #/^(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+([\d-]+)\s+(.)/ line)))
      (assert result nil "Illegal flag line of mobile ~d: ~s~%" nr line)
      (setf (mob-flags-of mobile) (asciiflag-to-bits (regref result 1))
            (mob2-flags-of mobile) (asciiflag-to-bits (regref result 2))
            (aff-flags-of mobile) (asciiflag-to-bits (regref result 3))
            (aff2-flags-of mobile) (asciiflag-to-bits (regref result 4))
            (aff3-flags-of mobile) (asciiflag-to-bits (regref result 5))
            (alignment-of mobile) (parse-integer (regref result 6))
            mob-type (char (regref result 7) 0)))

    (assert (or (eql mob-type #\S) (eql mob-type #\E)) nil
            "Unsupported mob type '~c' in mob #~d"
            mob-type
            nr)

    (let* ((line (get-line inf))
           (result (scan #/^(\d+)\s+([\d-]+)\s+([\d-]+)\s+(\d+)d(\d+)\+(\d+)\s+(\d+)d(\d+)\+([-\d]+)/ line)))
      (assert result nil "Illegal numbers-1 line of mobile ~d: ~s~%" nr line)
      (setf (level-of mobile) (parse-integer (regref result 1))
            (hitroll-of mobile) (- 20 (parse-integer (regref result 2)))
            (armor-of mobile) (* 10 (parse-integer (regref result 3)))
            (max-hitp-of mobile) 0
            (hitp-of mobile) (parse-integer (regref result 4))
            (mana-of mobile) (parse-integer (regref result 5))
            (move-of mobile) (parse-integer (regref result 6))

            (max-mana-of mobile) 10
            (max-move-of mobile) 50

            (damnodice-of (shared-of mobile)) (parse-integer (regref result 7))
            (damsizedice-of (shared-of mobile)) (parse-integer (regref result 8))
            (damroll-of mobile) (parse-integer (regref result 9))))

    (when (mob-flagged mobile +mob-wimpy+)
      (setf (morale-of (shared-of mobile)) (max 30 (level-of mobile))))

    (let* ((line (get-line inf))
           (result (scan #/^(\d+)\s+(\d+)(?:\s+(\d+)\s+(\d+))?/ line)))
      (assert result nil "Illegal numbers-2 line of mobile ~d: ~s~%" nr line)

      (when (regref result 3)
        (setf (char-class-of mobile) (parse-integer (regref result 4))
              (race-of mobile) (parse-integer (regref result 3))))
      (setf (gold-of mobile) (parse-integer (regref result 1))
            (exp-of mobile) (parse-integer (regref result 2))))

    (let* ((line (get-line inf))
           (result (scan #/^(\d+)\s+(\d+)\s+(\d+)(?:\s+(\d+))?/ line)))
      (assert result nil "Illegal numbers-3 line of mobile ~d: ~s~%" nr line)
      (setf (position-of mobile) (parse-integer (regref result 1))
            (default-pos-of (shared-of mobile)) (parse-integer (regref result 2))
            (sex-of mobile) (case (parse-integer (regref result 3))
                              (0 'neuter)
                              (1 'male)
                              (2 'female)))
      (setf (attack-type-of (shared-of mobile))
            (if (regref result 4)
                (parse-integer (regref result 4))
                0)))

    (when (eql mob-type #\E)
      (parse-enhanced-mobile inf nr mobile))

    ;; Load reply structors until # or $ is reached
    (loop for line = (get-line inf) do
         (ecase (char line 0)
           (#\R
            (fread-string inf)
            (fread-string inf))
           ((#\$ #\#)
            (file-position inf (- (file-position inf) (1+ (length line))))
            (return))))

    (setf (aff-abils-of mobile) (copy-abilities (real-abils-of mobile)))
    (setf (gethash nr *mobile-prototypes*) mobile)))

(defun parse-object (inf nr)
  (let ((obj (make-instance 'obj-data
                            :shared (make-instance 'obj-shared-data)))
        (place (format nil "object #~d" nr)))
    (setf (vnum-of (shared-of obj)) nr
          (number-of (shared-of obj)) 0
          (house-count-of (shared-of obj)) 0
          (func-of (shared-of obj)) nil
          (proto-of (shared-of obj)) obj
          (owner-id-of (shared-of obj)) 0
          (in-room-of obj) nil
          (aliases-of obj) (fread-string inf))
    (assert (aliases-of obj) nil
            "Null obj aliases or format error at or near ~a" place)
    (setf (name-of obj) (fread-string inf))
    (when (scan #/^(A|An|The)\s/ (name-of obj))
      (slog "Fixed name capitalization in object ~d" nr)
      (setf (char (name-of obj) 0) (char-downcase (char (name-of obj) 0))))

    (setf (line-desc-of obj) (fread-string inf)
          (action-desc-of obj) (fread-string inf))
    (when (> (length (line-desc-of obj)) 1)
      (setf (line-desc-of obj) (string-upcase (line-desc-of obj) :end 1)))

    (let ((result (scan #/(\d+) (\S+) (\S+) (\S+)(?: (\S+))?/ (get-line inf))))
      (assert result nil "Expected 4 or 5 args in first numeric line, object ~d" nr)
      (setf (kind-of obj) (parse-integer (regref result 1))
            (extra-flags-of obj) (asciiflag-to-bits (regref result 2))
            (extra2-flags-of obj) (asciiflag-to-bits (regref result 3))
            (wear-flags-of obj) (asciiflag-to-bits (regref result 4))
            (extra3-flags-of obj) (if (regref result 5)
                                      (asciiflag-to-bits (regref result 5))
                                      0)))

    (let ((result (scan #/(\d+) ([\d-]+) ([\d-]+)(?: ([\d-]+))?/ (get-line inf))))
      (assert result nil "Expected 3 or 4 args in second numeric line, object ~d" nr)
      (setf (aref (value-of obj) 0) (parse-integer (regref result 1))
            (aref (value-of obj) 1) (parse-integer (regref result 2))
            (aref (value-of obj) 2) (parse-integer (regref result 3)))
      (setf (aref (value-of obj) 3)
            (if (regref result 4)
                (parse-integer (regref result 4))
                0)))

    (let ((result (scan #/(\d+) ([-\d]+) ([-\d]+)/ (get-line inf))))
      (assert result nil "Expected 3 args in third numeric line, object ~d" nr)
      (setf (material-of obj) (parse-integer (regref result 1))
            (max-dam-of obj) (parse-integer (regref result 2))
            (damage-of obj) (parse-integer (regref result 3))))

    (let ((result (scan #/([\d-]+) ([\d-]+) ([\d-]+)(?: ([\d-]+))?/ (get-line inf))))
      (assert result nil "Expected 3 or 4 args in fourth numeric line, object ~d" nr)
      (set-weight obj (parse-integer (regref result 1)))
      (setf (cost-of (shared-of obj)) (parse-integer (regref result 2))
            (cost-per-day-of (shared-of obj)) (parse-integer (regref result 3)))
      (let ((timer-str (regref result 4)))
        (setf (timer-of obj) (if timer-str (parse-integer timer-str) 0))))

    ;; Check to make sure that weight of containers exceeds current quantity
    (when (and (or (= (kind-of obj) +item-drinkcon+)
                   (= (kind-of obj) +item-fountain+))
               (< (get-weight obj) (aref (value-of obj) 1)))
      (set-weight obj (+ (aref (value-of obj) 1) 5)))

    ;; Extra descriptions and affect fields
    (dotimes (j +max-obj-affect+)
      (setf (aref (affected-of obj) j)
            (make-instance 'obj-affected-type
                           :location +apply-none+
                           :modifier 0)))

    (setf (aref (bitvector-of obj) 0) 0
          (aref (bitvector-of obj) 1) 0
          (aref (bitvector-of obj) 2) 0)

    (setf place (format nil "~a, after numeric constants (expecting E/A/#xxx)" place))

    (loop for line = (get-line inf)
       with affected-idx = 0 do
       (assert line nil "Format error in ~a" place)
       (case (char line 0)
         (#\E
          (push (make-instance 'extra-descr-data
                               :keyword (fread-string inf)
                               :description (fread-string inf))
                (ex-description-of obj)))
         (#\A
          (when (>= affected-idx +max-obj-affect+)
            (error "Too many A fields (~d max), ~a" +max-obj-affect+ place))
          (let ((result (scan #/(\d+) ([\d-]+)/ (get-line inf))))
            (assert result nil "Invalid affect field in ~a" place)
            (setf (location-of (aref (affected-of obj) affected-idx))
                  (parse-integer (regref result 1)))
            (setf (modifier-of (aref (affected-of obj) affected-idx))
                  (parse-integer (regref result 2)))
            (incf affected-idx)))
         (#\O
          (let ((result (scan #/O (\d+)/ line)))
            (setf (owner-id-of (shared-of obj))
                  (parse-integer (regref result 1)))))
         (#\V
          (let* ((result (scan #/(\d+) (\S+)/ (get-line inf)))
                 (num (parse-integer (regref result 1))))
            (assert (<= 1 num 3) nil "Extra index ~d is out of bounds, ~a"
                    num place)
            (setf (aref (bitvector-of obj) (1- num))
                  (asciiflag-to-bits (regref result 2)))))
         (#\P
          (setf (func-param-of (shared-of obj)) (fread-string inf)))
         ((#\$ #\#)
          (setf (gethash nr *object-prototypes*) obj)
          (return-from parse-object line))
         (t
          (error "Format error in ~a" place))))))

(defun load-zones (inf zonename)
  (let ((new-zone (make-instance 'zone-data :name zonename))
        (line nil)
        (line-num 0))

    (flet ((get-line-with-count (inf)
             (multiple-value-bind (new-line line-count)
                 (get-line inf)
               (setf line new-line)
               (incf line-num line-count))))

      (get-line-with-count inf)
      (let ((result (scan #/^#(\d+)/ line)))
        (unless result
          (error "Expected zone id in ~a, line ~d (~a)" zonename line-num line))

        (setf (number-of new-zone) (parse-integer (regref result 1))))

      (get-line-with-count inf)
      (setf (name-of new-zone) (remove #\~ line))

      (get-line-with-count inf)

      (when (string-equal "C " line :end2 2)
        (setf (owner-idnum-of new-zone) (parse-integer line :start 2))
        (get-line-with-count inf))
      (when (string-equal "C2 " line :end2 3)
        (setf (owner-idnum-of new-zone) (parse-integer line :start 3))
        (get-line-with-count inf))
      (when (string-equal "RP " line :end2 3)
        (setf (owner-idnum-of new-zone) (parse-integer line :start 3))
        (get-line-with-count inf))

      ;; New format reading starts now
      (loop
       (let ((args (cl-ppcre:split #/: */ line)))
         (string-case (first args)
           ("owner"
            (setf (owner-idnum-of new-zone) (parse-integer (second args))))
           ("co-owner"
            (setf (co-owner-idnum-of new-zone) (parse-integer (second args))))
           ("respawn-pt"
            (setf (respawn-pt-of new-zone) (parse-integer (second args))))
           ("minimum-level"
            (setf (min-lvl-of new-zone) (parse-integer (second args))))
           ("minimum-gen"
            (setf (min-gen-of new-zone) (parse-integer (second args))))
           ("maximum-level"
            (setf (max-lvl-of new-zone) (parse-integer (second args))))
           ("maximum-gen"
            (setf (max-gen-of new-zone) (parse-integer (second args))))
           ;; FIXME: These fread-string calls don't track line number
           ("public-desc"
            (setf (public-desc-of new-zone) (fread-string inf)))
           ("private-desc"
            (setf (private-desc-of new-zone) (fread-string inf)))
           ("author"
            (setf (author-of new-zone) (second args)))
           (t
            (return))))
       (get-line-with-count inf))

      (let ((result (scan #/^(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\S+)\s+([-\d]+)\s+([-\d]+)\s*(\d+)?/ line)))

        (unless result
          (error "Format error in 9-constant line of ~a~%Line was ~s" zonename line))

        (setf (top-of new-zone) (parse-integer (regref result 1)))
        (setf (lifespan-of new-zone) (parse-integer (regref result 2)))
        (setf (reset-mode-of new-zone) (parse-integer (regref result 3)))
        (setf (time-frame-of new-zone) (parse-integer (regref result 4)))
        (setf (plane-of new-zone) (parse-integer (regref result 5)))
        (setf (flags-of new-zone) (asciiflag-to-bits (regref result 6)))
        (setf (hour-mod-of new-zone) (parse-integer (regref result 7)))
        (setf (year-mod-of new-zone) (parse-integer (regref result 8)))
        (let ((str (regref result 9)))
          (setf (pk-style-of new-zone) (if str (parse-integer str) 0))))

      (setf (weather-of new-zone) (make-instance 'weather-data))

      (loop
       (get-line-with-count inf)

       (when (member (char line 0) '(#\S #\$))
         (return))

       (let ((new-zonecmd (make-instance 'reset-com))
             (cmd-num 0))
         (setf (command-of new-zonecmd) (char line 0))

         (cond
           ((find (char line 0) "MOEPIVWGR")
            (let ((result (scan #/^. ([-01]+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)/ line)))
              (unless result
                (error "Format error in ~a, line ~d: ~s" zonename line-num line))
              (setf (if-flag-of new-zonecmd) (parse-integer (regref result 1)))
              (setf (prob-of new-zonecmd) (parse-integer (regref result 2)))
              (setf (arg1-of new-zonecmd) (parse-integer (regref result 3)))
              (setf (arg2-of new-zonecmd) (parse-integer (regref result 4)))
              (setf (arg3-of new-zonecmd) (parse-integer (regref result 5)))))
           ((char= (char line 0) #\D)
            (let ((result (scan #/^. ([-01]+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\S+)/ line)))
              (unless result
                (error "Format error in ~a, line ~d: ~s" zonename line-num line))
              (setf (if-flag-of new-zonecmd) (parse-integer (regref result 1)))
              (setf (prob-of new-zonecmd) (parse-integer (regref result 2)))
              (setf (arg1-of new-zonecmd) (parse-integer (regref result 3)))
              (setf (arg2-of new-zonecmd) (parse-integer (regref result 4)))
              (setf (arg3-of new-zonecmd) (asciiflag-to-bits (regref result 5)))))
           (t
            (let ((result (scan #/^. ([-01]+)\s+(\d+)\s+(\d+)\s+(\d+)/ line)))
              (unless result
                (error "Format error in ~a, line ~d (~s)" zonename line-num line))
              (setf (if-flag-of new-zonecmd) (parse-integer (regref result 1)))
              (setf (prob-of new-zonecmd) (parse-integer (regref result 2)))
              (setf (arg1-of new-zonecmd) (parse-integer (regref result 3)))
              (setf (arg2-of new-zonecmd) (parse-integer (regref result 4))))))

         (setf (line-of new-zonecmd) cmd-num)
         (push new-zonecmd (cmds-of new-zone))
         (incf cmd-num)))

      ;; Reverse the commands so they're in the proper execution order
      (setf (cmds-of new-zone) (nreverse (cmds-of new-zone)))

      ;; Now we add the new zone to the zone-table linked list
      (push new-zone *zone-table*))))

(defun read-mobile (vnum)
  "Create a new mobile from a prototype"
  (let ((proto (real-mobile-proto vnum)))
    (unless proto
      (signal 'mobile-prototype-not-found vnum))
    (let ((mob (clone-mobile-proto proto)))
      (incf (number-of (shared-of proto)))
      (incf (loaded-of (shared-of proto)))
      (setf (birth-time-of mob) (now))
      (incf *current-mob-idnum*)
      (setf (mob-idnum-of mob) *current-mob-idnum*)

      (cond
        ((mob2-flagged mob +mob2-unapproved+)
         (setf (gold-of mob) 0)
         (setf (cash-of mob) 0)
         (setf (exp-of mob) 0))
        (t
         (setf (gold-of mob) (rand-value (gold-of mob)
                                         (truncate (* (gold-of mob) 0.15))
                                         nil nil))
         (setf (cash-of mob) (rand-value (cash-of mob)
                                         (truncate (* (cash-of mob) 0.15))
                                         nil nil))))

      (push mob *characters*)
      (setf (gethash (- (mob-idnum-of mob)) *character-map*) mob)

      mob)))

(defun read-object (vnum)
  (let ((proto (real-object-proto vnum)))
    (unless proto
      (signal 'object-prototype-not-found vnum))
    (let ((obj (clone-object-proto proto)))
      (incf (number-of (shared-of proto)))
      (incf *top-unique-id*)
      (setf *unique-id-changed* t)

      (push obj *object-list*)
      (when (is-obj-stat2 obj +item2-unapproved+)
        (setf (timer-of obj) 60))
      obj)))

(defun find-mob-leader (mob)
  (when (plusp (leader-of (shared-of mob)))
    (find-if (lambda (potential)
               (and (not (eql mob potential))
                    (is-npc potential)
                    (= (vnum-of potential) (leader-of (shared-of mob)))
                    (not (circle-follow mob potential))))
               (people-of (in-room-of mob)))))

(defun process-zone-command (zone-cmd zone command-num last-cmd last-mob prob-override)
  ;; if-flag
  ;; 0 - "Do regardless of previous"
  ;; 1 - "Do if previous succeeded"
  ;; -1 - "Do if previous failed"
  ;; last-cmd
  ;; 1 - "Last command succeeded"
  ;; 2 - "Last command had an error"
  ;; -1 - "Last command's percentage failed"
  (flet ((zone-error (fmt &rest args)
           (slog "ZONEERR: Zone #~d, Cmd ~d: ~?"
                 (number-of zone)
                 command-num
                 fmt args)))
    (cond
      ((and (= (if-flag-of zone-cmd) 1) (/= last-cmd 1))
       ;; skip
       (values last-cmd last-mob prob-override))
      ((and (= (if-flag-of zone-cmd) -1) (/= last-cmd -1))
       ;; skip
       (values last-cmd last-mob prob-override))
      ((and (not prob-override) (> (random-range 1 100) (prob-of zone-cmd)))
       (values -1 last-mob nil))
      (t
       (case (command-of zone-cmd)
         (#\*                           ; Ignore command
          (values -1 last-mob nil))
         (#\M                           ; Read a mobile
          (let ((tmob (real-mobile-proto (arg1-of zone-cmd)))
                (room (real-room (arg3-of zone-cmd))))
            (cond
              ((null tmob)
               (values 0 last-mob nil))
              ((> (number-of (shared-of tmob)) (arg2-of zone-cmd))
               (values 0 last-mob nil))
              ((null room)
               (values 0 last-mob nil))
              (t
               (let ((mob (read-mobile (arg1-of zone-cmd))))
                 (cond
                   (mob
                    (char-to-room mob room)
                    (let ((leader (find-mob-leader mob)))
                      (when leader
                        (add-follower mob leader)))
                    (values 1 mob nil))
                   (t
                    (values 0 mob nil))))))))
         (#\O                           ; Read an object
          (let ((tobj (real-object-proto (arg1-of zone-cmd)))
                (room (real-room (arg3-of zone-cmd))))
            (cond
              ((null tobj)
               (values 0 last-mob nil))
              ((>= (number-of (shared-of tobj)) (arg2-of zone-cmd))
               (values 0 last-mob nil))
              ((null room)
               (values 0 last-mob nil))
              (t
               (let ((obj (read-object (arg1-of zone-cmd))))
                 (cond
                   (obj
                    (setf (creation-method-of obj) :zone)
                    (setf (creator-of obj) (number-of zone))
                    (obj-to-room obj room)
                    (values 1 last-mob nil))
                   (t
                    (values 0 last-mob nil))))))))
         (#\P                           ; object to object
          (let ((tobj (real-object-proto (arg1-of zone-cmd)))
                (obj-to (find (arg3-of zone-cmd) *object-list* :key 'vnum-of)))
            (cond
              ((null obj-to)
               (zone-error "attempt to put obj ~a into nonexistent obj ~a"
                           (arg1-of zone-cmd)
                           (arg3-of zone-cmd))
               (values 0 last-mob nil))
              ((null tobj)
               (values 0 last-mob nil))
              ((>= (number-of (shared-of tobj)) (arg2-of zone-cmd))
               (values 0 last-mob nil))
              (t
               (let ((obj (read-object (arg1-of zone-cmd))))
                 (cond
                   (obj
                    (setf (creation-method-of obj) :zone)
                    (setf (creator-of obj) (number-of zone))
                    (when (zone-flagged zone +zone-zcmds-approved+)
                      (setf (extra2-flags-of obj)
                            (logior (extra2-flags-of obj) +item2-unapproved+))
                      (setf (timer-of obj) 60))
                    (obj-to-obj obj obj-to)
                    (values 1 last-mob nil))
                   (t
                    (values 0 last-mob nil))))))))
         (#\V                           ; add path to vehicle
          (values 0 last-mob nil))
         (#\W                           ; add path to mobile
          (values 0 last-mob nil))
         (#\G                           ; obj-to-char
          (let ((tobj (real-object-proto (arg1-of zone-cmd))))
            (cond
              ((null last-mob)
               (zone-error "attempt to give obj ~d to nonexistent mob"
                           (arg1-of zone-cmd))
               (values 0 last-mob nil))
              ((null tobj)
               (values 0 last-mob nil))
              ((>= (number-of (shared-of tobj)) (arg2-of zone-cmd))
               (values 0 last-mob nil))
              (t
               (let ((obj (read-object (arg1-of zone-cmd))))
                 (cond
                   (obj
                    (setf (creation-method-of obj) :zone)
                    (setf (creator-of obj) (number-of zone))
                    (when (zone-flagged zone +zone-zcmds-approved+)
                      (setf (extra2-flags-of obj)
                            (logior (extra2-flags-of obj) +item2-unapproved+))
                      (setf (timer-of obj) 60))
                    (obj-to-char obj last-mob)
                    (values 1 last-mob nil))
                   (t
                    (values 0 last-mob nil))))))))
         (#\E                           ; equipping object
          (let ((tobj (real-object-proto (arg1-of zone-cmd))))
            (cond
              ((null last-mob)
               (zone-error "attempt to equip obj ~d on nonexistent mob"
                           (arg1-of zone-cmd))
               (values 0 last-mob nil))
              ((null tobj)
               (values 0 last-mob nil))
              ((>= (number-of (shared-of tobj)) (arg2-of zone-cmd))
               (values 0 last-mob nil))
              ((not (<= 0 (arg3-of zone-cmd) +num-wears+))
               (zone-error "invalid equipment pos number ~d on obj ~d"
                           (arg3-of zone-cmd)
                           (arg1-of zone-cmd))
               (values 0 last-mob nil))
              ((not (can-wear tobj (aref +wear-bitvectors+ (arg3-of zone-cmd))))
               (zone-error "invalid eq pos ~d for object ~a"
                           (arg3-of zone-cmd)
                           (arg1-of zone-cmd))
               (values 0 last-mob nil))
              ((get-eq last-mob (arg3-of zone-cmd))
               (zone-error "char ~d already equipped in pos ~d while equipping ~d"
                           (vnum-of last-mob)
                           (arg3-of zone-cmd)
                           (arg1-of zone-cmd))
               (values 0 last-mob nil))
              (t
               (let ((obj (read-object (arg1-of zone-cmd))))
                 (cond
                   (obj
                    (setf (creation-method-of obj) :zone)
                    (setf (creator-of obj) (number-of zone))
                    (when (zone-flagged zone +zone-zcmds-approved+)
                      (setf (extra2-flags-of obj)
                            (logior (extra2-flags-of obj) +item2-unapproved+))
                      (setf (timer-of obj) 60))
                    (if (equip-char last-mob obj (arg3-of zone-cmd) :worn)
                        (values 1 last-mob nil)
                        (values 0 last-mob nil)))
                   (t
                    (values 0 last-mob nil))))))))
         (#\I                           ; implanting object
          (let ((tobj (real-object-proto (arg1-of zone-cmd))))
            (cond
              ((null last-mob)
               (zone-error "attempt to implant nonexistent mob")
               (values 0 last-mob nil))
              ((null tobj)
               (values 0 last-mob nil))
              ((>= (number-of (shared-of tobj)) (arg2-of zone-cmd))
               (values 0 last-mob nil))
              ((not (<= 0 (arg3-of zone-cmd) +num-wears+))
               (zone-error "invalid implant pos number ~d on obj ~d"
                           (arg3-of zone-cmd)
                           (arg1-of zone-cmd))
               (values 0 last-mob nil))
              ((not (can-wear tobj (aref +wear-bitvectors+ (arg3-of zone-cmd))))
               (zone-error "invalid implant pos ~d for object ~a"
                           (arg3-of zone-cmd)
                           (arg1-of zone-cmd))
               (values 0 last-mob nil))
              ((get-implant last-mob (arg3-of zone-cmd))
               (zone-error "char ~d already implanted in pos ~d while equipping ~d"
                           (vnum-of last-mob)
                           (arg3-of zone-cmd)
                           (arg1-of zone-cmd))
               (values 0 last-mob nil))
              (t
               (let ((obj (read-object (arg1-of zone-cmd))))
                 (cond
                   (obj
                    (setf (creation-method-of obj) :zone)
                    (setf (creator-of obj) (number-of zone))
                    (when (zone-flagged zone +zone-zcmds-approved+)
                      (setf (extra2-flags-of obj)
                            (logior (extra2-flags-of obj) +item2-unapproved+))
                      (setf (timer-of obj) 60))

                    (if (equip-char last-mob obj (arg3-of zone-cmd) :implant)
                        (values 1 last-mob nil)
                        (values 0 last-mob nil)))
                   (t
                    (values 0 last-mob nil))))))))
         (#\R                           ; rem obj from room
          (let* ((room (real-room (arg1-of zone-cmd)))
                 (obj (when room (get-obj-in-list-num (arg1-of zone-cmd)
                                                      (contents-of room)))))
            (cond
              ((and room obj (not (room-flagged room +room-house+)))
               (obj-from-room obj)
               (extract-obj obj)
               (values 1 last-mob t))
              (t
               (values 0 last-mob nil)))))
         (#\D                           ; set state of door
          (let ((room (real-room (arg1-of zone-cmd))))
            (cond
              ((or (null room)
                   (not (<= 0 (arg2-of zone-cmd) (1- +num-of-dirs+)))
                   (null (aref (dir-option-of room) (arg2-of zone-cmd))))
               (zone-error "~a door does not exist in room ~a"
                           (aref +dirs+ (arg2-of zone-cmd))
                           (arg1-of zone-cmd))
               (values 0 last-mob nil))
              (t
               (let* ((cmd-flags (arg3-of zone-cmd))
                      (dir-option (aref (dir-option-of room) (arg2-of zone-cmd)))
                      (exit-info (exit-info-of dir-option)))
                 (when (logtest cmd-flags +door-open+)
                   (setf (exit-info-of dir-option)
                         (logandc2 exit-info +ex-locked+))
                   (setf (exit-info-of dir-option)
                         (logandc2 exit-info +ex-closed+)))

                 (when (logtest cmd-flags +door-closed+)
                   (setf (exit-info-of dir-option)
                         (logandc2 exit-info +ex-locked+))
                   (setf (exit-info-of dir-option)
                         (logior exit-info +ex-closed+)))

                 (when (logtest cmd-flags +door-locked+)
                   (setf (exit-info-of dir-option)
                         (logior exit-info +ex-locked+))
                   (setf (exit-info-of dir-option)
                         (logior exit-info +ex-closed+)))

                 (when (logtest cmd-flags +door-hidden+)
                   (setf (exit-info-of dir-option)
                         (logior exit-info +ex-hidden+)))
                 (values 1 last-mob nil))))))
         (t
          (zone-error "Unknown cmd in reset table! cmd disabled")
          (setf (command-of zone-cmd) #\*)
          (values 0 last-mob nil)))))))

(defun reset-zone (zone)
  ;; Send +special-reset+ notification to all mobiles with specials
  (dolist (ch *characters*)
    (when (and (eql (zone-of (in-room-of ch)) zone)
               (mob-flagged ch +mob-spec+)
               (func-of (shared-of ch)))
      (funcall (func-of (shared-of ch)) ch ch 0 "" +special-reset+)))

  (let ((last-cmd 0)
        (last-mob nil)
        (command-num 0)
        (prob-override nil))
    (dolist (zone-cmd (cmds-of zone))
      (multiple-value-setq (last-cmd last-mob prob-override)
        (process-zone-command zone-cmd
                              zone
                              command-num
                              last-cmd
                              last-mob
                              prob-override))
      (incf command-num)))

  (setf (age-of zone) 0)

  (dolist (room (world-of zone))
    (dolist (search (searches-of room))
      (setf (flags-of search)
            (logand (flags-of search) (lognot +search-tripped+))))))

(defun fread-string (inf)
  (with-output-to-string (s nil :element-type 'base-char)
    (loop
       for line = (read-line inf)
       as tilde-pos = (position #\~ line)
       until tilde-pos
       do (write-line line s)
       finally (write-string line s :end tilde-pos))))

(defun real-room (vnum)
  (when (plusp (hash-table-count *rooms*))
    (gethash vnum *rooms*)))

(defun real-mobile-proto (vnum)
  (gethash vnum *mobile-prototypes*))
(defun real-object-proto (vnum)
  (gethash vnum *object-prototypes*))

(defun player-in-world (idnum)
  (find idnum *characters* :key #'idnum-of))