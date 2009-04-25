(in-package #:tempus)

(defun find-target-room (ch roomstr)
  (cond
    ((string= "" roomstr)
     (send-to-char ch "You must supply a room number or name.~%"))
    ((every #'digit-char-p roomstr)
     (let ((result (real-room (parse-integer roomstr))))
       (prog1 result
         (unless result
          (send-to-char ch "No room exists with that number.~%")))))
    ((string-abbrev roomstr "previous")
     (loop
        with bottom-room = (* (number-of (zone-of (in-room-of ch))) 100)
        for roomnum from (1- (idnum-of (in-room-of ch))) downto bottom-room
        as result = (real-room roomnum)
        until result
        finally (progn
                  (unless result
                    (send-to-char ch "No previous room exists in this zone!~%"))
                  (return result))))
    ((string-abbrev roomstr "next")
     (loop
        with top-room = (top-of (zone-of (in-room-of ch)))
        for roomnum from (1+ (idnum-of (in-room-of ch))) upto top-room
        as result = (real-room roomnum)
        until result
        finally (progn
                  (unless result
                    (send-to-char ch "No next room exists in this zone!~%"))
                  (return result))))
    (t
     (let ((obj (or (get-char-vis ch roomstr)
                    (get-obj-vis ch roomstr))))
       (cond
         ((null obj)
          (send-to-char ch "No such creature or object around.~%")
          nil)
         ((null (in-room-of obj))
          (send-to-char ch "That object is not available.~%")
          nil)
         (t
          (in-room-of obj)))))))

(defun perform-goto (ch room allow-follow)
  (if (can-enter-room ch room)
      (let ((was-in (in-room-of ch)))
        (act ch :place-emit (cond
                              ((or (null (poofout-of ch))
                                   (string= "" (poofout-of ch)))
                               "$n disappears in a puff of smoke.")
                              ((search "$n" (poofout-of ch))
                               (poofout-of ch))
                              (t
                               (format nil "$n ~a" (poofout-of ch)))))
        (when (in-room-of ch)
          (char-from-room ch t))
        (char-to-room ch room)
        (when (room-is-open-air room)
          (setf (position-of ch) +pos-flying+))
        (act ch :place-emit (cond
                              ((or (null (poofin-of ch))
                                   (string= "" (poofin-of ch)))
                               "$n appears with an ear-splitting bang.")
                              ((search "$n" (poofin-of ch))
                               (poofin-of ch))
                              (t
                               (format nil "$n ~a" (poofin-of ch)))))
        (look-at-room ch room nil)
        (when (and allow-follow (followers-of ch))
          (dolist (tch (followers-of ch))
            (when (and (eql was-in (in-room-of tch))
                       (immortal-level-p tch)
                       (not (plr-flagged ch (logior +plr-olc+ +plr-writing+ +plr-mailing+)))
                       (is-visible-to ch tch))
              (perform-goto tch room t))))

        (when (and (room-flagged room +room-death+)
                   (< (level-of ch) +lvl-impl+))
          (slog "(GC) ~a goto deathtrap [~d] ~a."
                (name-of ch)
                (number-of room)
                (name-of room))))
      (send-to-char ch "You cannot enter there.~%")))

(defun perform-transport (ch tch)
  (cond
    ((eql tch ch)
     (send-to-char ch "Sure, sure.  Try to transport yourself.~%"))
    ((and (< (level-of ch) (level-of tch))
          (not (is-npc tch)))
     (send-to-char ch "~a is far too powerful for you to transport.~%"
                   (name-of tch)))
    ((and (room-is-open-air (in-room-of ch))
          (/= (position-of tch) +pos-flying+))
     (send-to-char ch "You are in midair and ~a isn't flying.~%"
                   (name-of tch)))
    (t
     (let ((was-in (in-room-of tch)))
       (act tch :place-emit "$n disappears in a mushroom cloud.")
       (char-from-room tch t)
       (char-to-room tch (in-room-of ch) nil)
       (act ch :target tch
            :subject-emit "$N arrives from a puff of smoke."
            :target-emit "$n has transported you!"
            :not-target-emit "$N arrives from a puff of smoke.")
       (dolist (fch (followers-of tch))
         (when (and (eql was-in (in-room-of fch))
                    (immortal-level-p fch)
                    (not (plr-flagged ch (logior +plr-olc+ +plr-writing+ +plr-mailing+)))
                    (is-visible-to ch fch))
           (perform-goto fch (in-room-of tch) t)))
       (slog "(GC) ~a has transported ~a to ~a[~d]"
             (name-of ch)
             (name-of tch)
             (name-of (in-room-of ch))
             (number-of (in-room-of ch)))))))

(defun vnum-prototypes (ch namelist-str name-color prototype-hash)
  "Displays to CH a list of the objects or mobiles in PROTOTYPE-HASH which match NAME.  The name of the object or mobile will be in the color designated by NAME-COLOR.  Returns the number displayed."
  (let ((count 0)
        (namelist (split-sequence #\space namelist-str
                                  :remove-empty-subseqs t)))
    (with-pagination ((link-of ch))
      (send-to-char ch "~a"
                    (with-output-to-string (str)
                      (loop
                         for vnum in (sort (hash-keys prototype-hash) #'<)
                         as proto = (gethash vnum prototype-hash)
                         when (every (lambda (name)
                                       (is-alias-of name (aliases-of proto)))
                                     namelist)
                         do
                           (incf count)
                           (format str "~3d. &g[&n~5d&g] &~c~a&n~%"
                                   count
                                   vnum
                                   name-color
                                   (name-of proto))))))
    count))



(defun purge-creature (ch destroy-objects-p)
  (unless destroy-objects-p
    (loop
       for pos from 0 upto (1- +num-wears+) do
         (when (get-eq ch pos)
           (let ((obj (unequip-char ch pos :worn t)))
             (obj-to-room obj (in-room-of ch))))
         (when (get-implant ch pos)
           (let ((obj (unequip-char ch pos :implant t)))
             (obj-to-room obj (in-room-of ch)))))
    (dolist (obj (copy-list (carrying-of ch)))
      (obj-from-char obj)
      (obj-to-room obj (in-room-of ch))))

  (unless (is-npc ch)
    (setf (rentcode-of ch) :quit)
    (setf (rent-per-day-of ch) 0)
    (setf (desc-mode-of ch) 'unknown)
    (setf (rent-currency-of ch) 0)
    (setf (load-room-of ch) 0)
    (setf (login-time-of ch) (now))
    (save-player-to-xml ch))

  (extract-creature ch 'disconnecting))

(defun perform-purge (ch objs)
  (when (null objs)
    (send-to-char ch "Nothing around by that name.~%"))

  ;; Now purge the unfortunates
  (dolist (obj objs)
    (etypecase obj
      (creature
       (purge-creature obj nil))
      (obj-data
       (extract-obj obj)))))

(defcommand (ch "stat" "zone") (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (send-to-char ch "Zone #&y~d: &c~a&n~%"
                  (number-of zone)
                  (name-of zone))
    (send-to-char ch "Authored by: ~a~%" (or (author-of zone) "<none>"))
    (send-to-char ch "Rooms: [~d-~d]  Respawn Pt: [~d]  Reset mode: ~a~%"
                  (* (number-of zone) 100)
                  (top-of zone)
                  (respawn-pt-of zone)
                  (aref +reset-mode+(reset-mode-of zone)))
    (send-to-char ch "TimeFrame: [~a]  Plane: [~a]   "
                  (aref +time-frames+ (time-frame-of zone))
                  (aref +planes+ (plane-of zone)))
    (send-to-char ch "Owner: ~a  Co-Owner: ~a~%"
                  (if (owner-idnum-of zone)
                      (or (retrieve-player-name (owner-idnum-of zone))
                          (format nil "<invalid #~d>" (owner-idnum-of zone)))
                      "<none>")
                  (if (co-owner-idnum-of zone)
                      (or (retrieve-player-name (co-owner-idnum-of zone))
                          (format nil "<#~d>" (co-owner-idnum-of zone)))
                      "<none>"))
    (let ((weather (weather-of zone)))
      (send-to-char ch "Sun [~(~a~)] Sky: [~(~a~)] Moon: [~a] Pres: [~3d] Chng: [~3d]~%"
                    (sunlight-of weather)
                    (sky-of weather)
                    (gethash (moonlight-of weather) +moon-sky-types+)
                    (pressure-of weather)
                    (change-of weather)))
    (send-to-char ch "Flags: &g~a ~a&n~%"
                  (printbits (flags-of zone) +zone-flags+ "NONE")
                  (aref +zone-pk-flags+ (pk-style-of zone)))
    (when (min-lvl-of zone)
      (send-to-char ch "Target lvl/gen: [~2d/~2d - ~2d/~2d]~%"
                    (min-lvl-of zone)
                    (min-gen-of zone)
                    (max-lvl-of zone)
                    (max-gen-of zone)))
    (when (public-desc-of zone)
      (send-to-char ch "Public Description:~%~a" (public-desc-of zone)))
    (when (private-desc-of zone)
      (send-to-char ch "Private Description:~%~a" (private-desc-of zone)))

    (let ((numm 0)
          (numm-proto 0)
          (numo 0)
          (numo-proto 0)
          (nump 0)
          (numr 0)
          (numur 0)
          (nums 0)
          (av-lev 0)
          (av-lev-proto 0))
      ;; count mob stats
      (dolist (mob *characters*)
        (when (and (is-npc mob)
                   (in-room-of mob)
                   (eql (zone-of (in-room-of mob)) zone))
          (incf numm)
          (incf av-lev (level-of mob))))
      (unless (zerop numm)
        (setf av-lev (floor av-lev numm)))

      ;; count mob prototype stats
      (dolist (proto (hash-values *mobile-prototypes*))
        (when (and (<= (* (number-of zone) 100)
                       (vnum-of proto)
                       (top-of zone))
                   (is-npc proto))
          (incf numm-proto)
          (incf av-lev-proto (level-of proto))))
      (unless (zerop numm-proto)
        (setf av-lev-proto (floor av-lev-proto numm-proto)))

      ;; count object stats
      (dolist (obj *object-list*)
        (when (and (in-room-of obj)
                   (eql (zone-of (in-room-of obj)) zone))
          (incf numo)))

      ;; count object prototype stats
      (dolist (proto (hash-values *object-prototypes*))
        (when (<= (* (number-of zone) 100) (vnum-of proto) (top-of zone))
          (incf numo-proto)))

      ;; count players
      (dolist (cxn *cxns*)
        (when (and (typep cxn 'tempus-cxn)
                   (actor-of cxn)
                   (in-room-of (actor-of cxn))
                   (eql (zone-of (in-room-of (actor-of cxn))) zone))
          (incf nump)))

      ;; count room stats
      (dolist (room (world-of zone))
        (incf numr)
        (when (string= (description-of room) "")
          (incf numur))
        (incf nums (length (searches-of room))))

      (send-to-char ch "~%Zone Stats :-
  mobs in zone : ~3d, ~3d protos;   objs in zone  : ~3d,~3d protos
  players in zone: (~3d) ~3d   rooms in zone: ~3d   undescripted rooms: ~3d
  search in zone: ~d
  usage count: ~d
  Avg. Level [&g~d&n]real, [&g~d&n] proto
"
                    numm numm-proto
                    numo numo-proto
                    (num-players-of zone)
                    nump
                    numr
                    numur
                    nums
                    (enter-count-of zone)
                    av-lev
                    av-lev-proto))))

(defcommand (ch "stat" "trails") (:immortal)
  (if (trail-of (in-room-of ch))
      (with-pagination ((link-of ch))
        (let ((now (now)))
          (loop
             :for trail :in (trail-of (in-room-of ch))
             :as num :from 1
             :as timediff := (timestamp-difference now (time-of trail))
             :do (send-to-char ch "[~2d] -- Name: '~a', (~a), Idnum: [~5d]
         Time Passed: ~d minutes, ~d seconds.
         From dir: ~a, To dir: ~a, Track: [~2d]
         Flags: ~a"
                               num (name-of trail)
                               (if (find (idnum-of trail) *characters* :key #'idnum-of)
                                   "in world" "gone")
                               (idnum-of trail)
                               (floor timediff 60)
                               (mod timediff 60)
                               (if (plusp (from-dir-of trail))
                                   (aref +dirs+ (from-dir-of trail)) "NONE")
                               (if (plusp (to-dir-of trail))
                                   (aref +dirs+ (to-dir-of trail)) "NONE")
                               (track-of trail)
                               (printbits (flags-of trail) +trail-flags+ "NONE")))))
      (send-to-char ch "No trails exist within this room.~%")))

(defun find-spec-name (spec)
  "TODO: Actually find the special"
  (if spec
      "Exists"
      "None"))

(defun format-search-data (ch room search)
  (send-to-char ch "&rCommand triggers:&n ~a, &rkeywords:&n ~a~%"
                (if (string/= "" (trigger-of search))
                    (trigger-of search) "None.")
                (cond
                  ((string= "" (keywords-of search))
                   "None.")
                  ((and (logbitp (flags-of search) +search-clanpasswd+)
                        room
                        (not (clan-house-can-enter ch room)))
                   "*******")
                  (t
                    (keywords-of search))))
  (send-to-char ch " To_vict  : ~a~% To_room  : ~a~% To_remote: ~a~%"
                (or (to-vict-of search) "None")
                (or (to-room-of search) "None")
                (or (to-remote-of search) "None"))
  (send-to-char ch "Fail_chance: ~d~%" (fail-chance-of search))
  (cond
    ((= (command-of search) +search-com-door+)
     (send-to-char ch "DOOR  Room #: ~d, Direction: ~d, Mode: ~d.~%"
                   (aref (arg-of search) 0)
                   (aref (arg-of search) 1)
                   (aref (arg-of search) 2)))
    ((= (command-of search) +search-com-mobile+)
     (send-to-char ch "MOB   Vnum #: ~d (&y~a&n), to room: ~d, Max: ~d.~%"
                   (aref (arg-of search) 0)
                   (multiple-value-bind (mob existsp)
                       (gethash (aref (arg-of search) 0) *mobile-prototypes*)
                     (if existsp (name-of mob) "NULL"))
                   (aref (arg-of search) 1)
                   (aref (arg-of search) 2)))
    ((= (command-of search) +search-com-object+)
     (send-to-char ch "OBJ   Vnum #: ~d (&y~a&n), to room: ~d, Max: ~d.~%"
                   (aref (arg-of search) 0)
                   (multiple-value-bind (obj existsp)
                       (gethash (aref (arg-of search) 0) *object-prototypes*)
                     (if existsp (name-of obj) "NULL"))
                   (aref (arg-of search) 1)
                   (aref (arg-of search) 2)))
    ((= (command-of search) +search-com-remove+)
     (send-to-char ch "REMOVE  Obj Vnum #: ~d (&y~a&n), Room # : ~d, Val 2: ~d.~%"
                   (aref (arg-of search) 0)
                   (multiple-value-bind (obj existsp)
                       (gethash (aref (arg-of search) 0) *object-prototypes*)
                     (if existsp (name-of obj) "NULL"))
                   (aref (arg-of search) 1)
                   (aref (arg-of search) 2)))
    ((= (command-of search) +search-com-equip+)
     (send-to-char ch "EQUIP  ----- : ~d, Obj Vnum : ~d (&y~a&n), Pos : ~d.~%"
                   (aref (arg-of search) 0)
                   (aref (arg-of search) 1)
                   (multiple-value-bind (obj existsp)
                       (gethash (aref (arg-of search) 1) *object-prototypes*)
                     (if existsp (name-of obj) "NULL"))
                   (aref (arg-of search) 2)))
    ((= (command-of search) +search-com-give+)
     (send-to-char ch "GIVE  ----- : ~d, Obj Vnum : ~d (&y~a&n), Max : ~d.~%"
                   (aref (arg-of search) 0)
                   (aref (arg-of search) 1)
                   (multiple-value-bind (obj existsp)
                       (gethash (aref (arg-of search) 1) *object-prototypes*)
                     (if existsp (name-of obj) "NULL"))
                   (aref (arg-of search) 2)))
    ((= (command-of search) +search-com-none+)
     (send-to-char ch "NONE       ~5d        ~5d        ~5d~%"
                   (aref (arg-of search) 0)
                   (aref (arg-of search) 1)
                   (aref (arg-of search) 2)))
    ((= (command-of search) +search-com-transport+)
     (send-to-char ch "TRANS      ~5d        ~5d        ~5d~%"
                   (aref (arg-of search) 0)
                   (aref (arg-of search) 1)
                   (aref (arg-of search) 2)))
    ((= (command-of search) +search-com-spell+)
     (send-to-char ch "SPELL      ~5d        ~5d        ~5d (~a)~%"
                   (aref (arg-of search) 0)
                   (aref (arg-of search) 1)
                   (aref (arg-of search) 2)
                   (name-of (aref *spell-info* (aref (arg-of search) 2)))))
    ((= (command-of search) +search-com-damage+)
     (send-to-char ch "DAMAGE     ~5d        ~5d        ~5d (~a)~%"
                   (aref (arg-of search) 0)
                   (aref (arg-of search) 1)
                   (aref (arg-of search) 2)
                   (name-of (aref *spell-info* (aref (arg-of search) 2)))))
    ((= (command-of search) +search-com-spawn+)
     (send-to-char ch "SPAWN  Spawn_rm: ~5d   Targ_rm:~5d   Hunt: ~5d~%"
                   (aref (arg-of search) 0)
                   (aref (arg-of search) 1)
                   (aref (arg-of search) 2)))
    ((= (command-of search) +search-com-loadroom+)
     (send-to-char ch "LOADROOM  NewLoad: ~5d   MaxLevel:~5d    ~5d~%"
                   (aref (arg-of search) 0)
                   (aref (arg-of search) 1)
                   (aref (arg-of search) 2)))
    (t
     (send-to-char ch "ERROR (~d)  ~5d        ~5d       ~5d~%"
                   (command-of search)
                   (aref (arg-of search) 0)
                   (aref (arg-of search) 1)
                   (aref (arg-of search) 2)))))

(defun format-prog (prog)
  (with-output-to-string (result)
    (with-input-from-string (str prog)
      (loop
         :with line-color := nil
         :for raw-line := (read-line str nil)
         :as line := (when raw-line (string-left-trim '(#\space) raw-line))
         :as num :from 1
         :while line
         :do
         (format result "&y~3d&b] " num)
         (flet ((string-starts-with (str start)
                  (let ((start-len (length start)))
                    (and (>= (length str) start-len)
                         (string= str start :end1 start-len)))))
           (unless line-color
             (setf line-color
                   (cond
                     ((string= line "")
                      "")
                     ((string-starts-with line "-")
                      "&b")
                     ((or (string-starts-with line "*before")
                          (string-starts-with line "*handle")
                          (string-starts-with line "*after"))
                      "&c")
                     ((or (string-starts-with line "*require")
                          (string-starts-with line "*unless")
                          (string-starts-with line "*randomly")
                          (string-starts-with line "*or"))
                      "&m")
                     ((string-starts-with line "*")
                      "&y")
                     (t
                      "&n"))))
           (format result "~a~a~%" line-color line)
           (when (and (plusp (length line))
                      (char/= (char line (1- (length line))) #\\))
             (setf line-color nil)))))))

(defmethod send-stats-to-char (ch (room room-data))
  (with-pagination ((link-of ch))
    (send-to-char ch "Room name: &c~a&n~%" (name-of room))
    (send-to-char ch "Zone: [&y~3d&n], VNum: [&g~5d&n], Type: ~a, Lighting: [~d], Max: [~d]~%"
                  (number-of (zone-of room))
                  (number-of room)
                  (aref +sector-types+ (terrain-of room))
                  (light-of room)
                  (max-occupancy-of room))
    (send-to-char ch "SpecProc: ~a, Flags: ~a~%"
                  (find-spec-name (func-of room))
                  (printbits (flags-of room) +room-bits+ "NONE"))
    (when (plusp (flow-speed-of room))
      (send-to-char ch "Flow (Direction: ~a, Speed: ~d, Type: ~a (~d)).~%"
                    (aref +dirs+ (flow-dir-of room))
                    (flow-speed-of room)
                    (aref +flow-types+ (flow-kind-of room))
                    (flow-kind-of room)))
    (send-to-char ch "Description:~%~:[  None.~%~;~:*~a~]"
                  (description-of room))
    (when (sounds-of room)
      (send-to-char ch "&gSound:&n~%~a" (sounds-of room)))
    (when (ex-description-of room)
      (send-to-char ch "Extra descs:&c~{ ~a~^;~}&n~%"
                    (mapcar 'keyword-of (ex-description-of room))))
    (send-to-char ch "Chars present: &y~{~a~^, ~}&n~%"
                  (loop
                     :for c :in (people-of (in-room-of ch))
                     :when (is-visible-to c ch)
                     :collect (format nil "~a(~:[PC~;MOB~])"
                                      (name-of c) (is-npc c))))
    (send-to-char ch "Contents: &g~{~a~^, ~}&n~%"
                  (loop
                     :for o :in (contents-of (in-room-of ch))
                     :when (is-visible-to o ch)
                     :collect (name-of o)))
    (when (searches-of room)
      (send-to-char ch "SEARCHES:~%")
      (dolist (search (searches-of room))
        (format-search-data ch room search)))

    (dotimes (dir +num-dirs+)
      (when (abs-exit room dir)
        (send-to-char ch "Exit &c~5a&n:  To: [&c~5a&n], Key: [~5d], Keywrd: ~a, Type: ~a~%"
                      (aref +dirs+ dir)
                      (or (to-room-of (abs-exit room dir)) "NONE")
                      (key-of (abs-exit room dir))
                      (if (string/= "" (keyword-of (abs-exit room dir)))
                          (keyword-of (abs-exit room dir))
                          "None")
                      (printbits (exit-info-of (abs-exit room dir)) +exit-bits+ "NONE"))
        (if (string/= "" (description-of (abs-exit room dir)))
            (send-to-char ch "~a" (description-of (abs-exit room dir)))
            (send-to-char ch "  No exit description.~%"))))
    (when (prog-text-of room)
      (send-to-char ch "Prog:~%~a" (format-prog (prog-text-of room))))))

(defun where-obj (obj)
  (cond
    ((in-room-of obj)
     (in-room-of obj))
    ((in-obj-of obj)
     (where-obj (in-obj-of obj)))
    ((carried-by-of obj)
     (in-room-of (carried-by-of obj)))
    ((worn-by-of obj)
     (in-room-of (worn-by-of obj)))
    (t
     nil)))

(defmethod send-stats-to-char (ch (obj obj-data))
  (when (and (is-obj-kind obj +item-note+)
             (is-alias-of "letter" (aliases-of obj)))
    (when (and (carried-by-of obj)
               (> (level-of (carried-by-of obj)) (level-of ch)))
      (act ch :target (carried-by-of obj)
           :target-emit "$n just tried to stat your mail.")
      (send-to-char ch "You're pretty brave, bucko.~%")
      (return-from send-stats-to-char))
    (when (< (level-of ch) +lvl-god+)
      (send-to-char ch "You can't stat mail.~%")
      (return-from send-stats-to-char)))

  (with-pagination ((link-of ch))
    (send-to-char ch "Name: '&g~a&n', Aliases: ~a~%"
                  (name-of obj)
                  (aliases-of obj))
    (send-to-char ch "VNum: [&g~5d&n], Exist: [~3d/~3d], Type: ~a, SpecProc: ~a~%"
                  (vnum-of obj)
                  (number-of (shared-of obj))
                  (house-count-of (shared-of obj))
                  (aref +item-kinds+ (kind-of obj))
                  (find-spec-name (func-of (shared-of obj))))
    (send-to-char ch "L-Des: &g~a&n~%" (or (line-desc-of obj) "None"))
    (when (string/= "" (action-desc-of obj))
      (send-to-char ch "Action desc: ~a~%" (action-desc-of obj)))
    (when (ex-description-of obj)
      (send-to-char ch "Extra descs:&c~{ ~a~^;~}&n~%"
                    (mapcar 'keyword-of (ex-description-of obj))))
    (unless (line-desc-of obj)
      (send-to-char ch "**This object currently has no description**~%"))
    (when (creation-time-of obj)
      (case (creation-method-of obj)
        (:zone
         (send-to-char ch "Created by zone #~d on ~a~%"
                       (creator-of obj)
                       (creation-time-of obj)))
        (:mob
         (send-to-char ch "Loaded onto mob #~d on ~a~%"
                       (creator-of obj)
                       (creation-time-of obj)))
        (:search
         (send-to-char ch "Created by search in room #~d on ~a~%"
                       (creator-of obj)
                       (creation-time-of obj)))
        (:imm
         (send-to-char ch "Loaded by ~a on ~a~%"
                       (retrieve-player-name (creator-of obj))
                       (creation-time-of obj)))
        (:prog
         (send-to-char ch "Created by prog (mob or room #~d) on ~a~%"
                       (creator-of obj)
                       (creation-time-of obj)))
        (:player
         (send-to-char ch "Created by player ~a on ~a~%"
                       (retrieve-player-name (creator-of obj))
                       (creation-time-of obj)))
        (t
         (send-to-char ch "Created on ~a~%"
                       (creation-time-of obj)))))
    (when (plusp (unique-id-of obj))
      (send-to-char ch "Unique object id: ~d~%" (unique-id-of obj)))
    (when (plusp (owner-id-of (shared-of obj)))
      (send-to-char ch "Oedit owned by: ~a[~d]~%"
                    (or (retrieve-player-name (owner-id-of (shared-of obj)))
                        "NOONE")
                    (owner-id-of (shared-of obj))))
    (send-to-char ch "Can be worn on: ~a~%"
                  (printbits (wear-flags-of obj) +wear-bits-desc+ "NONE"))
    (unless (every #'zerop (bitvector-of obj))
      (send-to-char ch "Set char bits : ~@{~a~^ ~}~%"
                    (printbits (aref (bitvector-of obj) 0) +affected-bits+ "")
                    (printbits (aref (bitvector-of obj) 1) +affected2-bits+ "")
                    (printbits (aref (bitvector-of obj) 2) +affected3-bits+ "")))
    (send-to-char ch "Extra flags : ~a~%"
                  (printbits (extra-flags-of obj) +extra-bits+ "NONE"))
    (send-to-char ch "Extra2 flags: ~a~%"
                  (printbits (extra2-flags-of obj) +extra2-bits+ "NONE"))
    (send-to-char ch "Extra3 flags: ~a~%"
                  (printbits (extra3-flags-of obj) +extra3-bits+ "NONE"))
    (send-to-char ch "Weight: ~d, Cost: ~d, Rent: ~d, Timer: ~d~%"
                  (weight-of obj)
                  (cost-of (shared-of obj))
                  (cost-per-day-of (shared-of obj))
                  (timer-of obj))
    (let ((room (where-obj obj)))
      (when room
        (send-to-char ch "Absolute location: ~a (~d)~%"
                      (name-of room)
                      (number-of room))
        (send-to-char ch "In room: &c~d&n, In obj: &g~a&n, Carry: &g~a&n, Worn: &g~a&n, Aux: &g~a&n~%"
                      (when (in-room-of obj) (number-of (in-room-of obj)))
                      (when (in-obj-of obj) (name-of (in-obj-of obj)))
                      (when (carried-by-of obj) (name-of (carried-by-of obj)))
                      (when (worn-by-of obj) (name-of (worn-by-of obj)))
                      (when (aux-obj-of obj) (name-of (aux-obj-of obj))))))
    (send-to-char ch "Material: [&y~a&n (~d)], Maxdamage: [~d], Damage: [~d]~%"
                  (aref +material-names+ (material-of obj))
                  (material-of obj)
                  (max-dam-of obj)
                  (damage-of obj))
    ;; TODO: Add object kind specific value displays
    (send-to-char ch "Value 0-3: ~a:[~d] ~a:[~d] ~a:[~d] ~a:[~d]~%"
                  (aref +item-kind-values+ (kind-of obj) 0)
                  (aref (value-of obj) 0)
                  (aref +item-kind-values+ (kind-of obj) 1)
                  (aref (value-of obj) 1)
                  (aref +item-kind-values+ (kind-of obj) 2)
                  (aref (value-of obj) 2)
                  (aref +item-kind-values+ (kind-of obj) 3)
                  (aref (value-of obj) 3))
    (when (and (eql (proto-of (shared-of obj)) obj)
               (func-param-of (shared-of obj)))
      (send-to-char ch "Spec_param:~%~a~%" (func-param-of (shared-of obj))))
    (send-to-char ch "Affections: ~{~a~^, ~}~%"
                  (or
                   (loop
                      for aff across (affected-of obj)
                      unless (zerop (modifier-of aff))
                      collect (format nil "~@d to ~a"
                                      (modifier-of aff)
                                      (aref +apply-types+ (location-of aff))))
                   '("None")))
    (unless (eql (proto-of (shared-of obj)) obj)
      (when (contains-of obj)
        (send-to-char ch "Contents:~%~a"
                      (with-output-to-string (str)
                        (list-obj-to-char str (contains-of obj) ch :content t))))
      (unless (zerop (soilage-of obj))
        (send-to-char ch "Soilage: ~a~%"
                      (printbits (soilage-of obj) +soilage-bits+ "")))

      (unless (zerop (sigil-idnum-of obj))
        (send-to-char ch "Warding Sigil: ~a (~d), level ~d~%"
                      (retrieve-player-name (sigil-idnum-of obj))
                      (sigil-idnum-of obj)
                      (sigil-level-of obj)))
      ;; Stat tmp object affects here
      )))

(defun mobile-experience (ch)
  1)

(defun thaco (char-class level)
  (truncate
   (if (minusp char-class)
       20
       (if (< char-class +num-classes+)
           (- 20 (* level (aref +thaco-factor+ char-class)))
           (- 20 (* level (aref +thaco-factor+ +class-warrior+)))))))



(defun affected-by-sanctuary (ch attacker)
  (and (aff-flagged ch +aff-sanctuary+)
       (or (null attacker)
           (not (or
                 (and (is-evil ch)
                      (affected-by-spell attacker
                                         +spell-righteous-penetration+))
                 (and (is-good ch)
                      (affected-by-spell attacker
                                         +spell-malefic-violation+)))))))

(defun get-damage-reduction (ch &optional attacker)
  (let ((result 0))
    (when (and (is-cleric ch) (is-good ch))
      ;; good clerics get an alignment-based protection, up to 30% in
      ;; the full moon, up to 10% otherwise
      (incf result (/ (alignment-of ch)
                      (if (= (lunar-phase *lunar-day*) +moon-full+)
                          30
                          100))))
    ;; sanctuary
    (when (affected-by-sanctuary ch attacker)
      (incf result
            (cond
              ((is-vampire ch)                             0)
              ((and (not (is-neutral ch))
                    (or (is-cleric ch) (is-knight ch)))   25)
              ((or (is-cyborg ch) (is-physic ch))          8)
              (t                                          15))))
    ;; oblivity
    (when (and (aff2-flagged ch +aff2-oblivity+) (is-neutral ch))
      (incf result (/ (+ (level-of ch)
                         (* (get-skill-bonus ch +zen-oblivity+) 10)
                         (- 1000 (abs (alignment-of ch)))
                         (* (check-skill ch +zen-oblivity+) 10))
                      100)))
    ;; nopain
    (when (aff-flagged ch +aff-nopain+)
      (incf result 25))
    ;; berserk
    (when (aff2-flagged ch +aff2-berserk+)
      (incf result
            (if (is-barb ch)
                (floor (get-skill-bonus ch +skill-berserk+) 6)
                7)))
    ;; damage control
    (when (aff3-flagged ch +aff3-damage-control+)
      (incf result (/ (get-skill-bonus ch +skill-damage-control+) 5)))
    ;; alcoholics!!
    (when (> (get-condition ch +drunk+) 5)
      (incf result (get-condition ch +drunk+)))
    ;; shield of righteousness
    ;; aria of asylum
    ;; lattice hardening
    (when (affected-by-spell ch +spell-lattice-hardening+)
      (incf result (/ (get-skill-bonus ch +spell-lattice-hardening+) 6)))
    ;; stoneskin barkskin dermal hardening
    (cond
      ((affected-by-spell ch +spell-stoneskin+)
       (incf result (/ (level-of (affected-by-spell ch +spell-stoneskin+))
                       4)))
      ((affected-by-spell ch +spell-barkskin+)
       (incf result (/ (level-of (affected-by-spell ch +spell-stoneskin+))
                       6)))
      ((affected-by-spell ch +spell-dermal-hardening+)
       (incf result (/ (level-of (affected-by-spell ch +spell-stoneskin+))
                       6))))
    ;; petrification
    (when (aff2-flagged ch +aff2-petrified+)
      (incf result 75))
    ;; various other protections
    (when attacker
      (when (and (is-evil attacker) (aff-flagged ch +aff-protect-evil+))
        (incf result 8))
      (when (and (is-good attacker) (aff-flagged ch +aff-protect-good+))
        (incf result 8))
      (when (and (is-undead attacker) (aff2-flagged ch +aff2-protect-undead+))
        (incf result 8))
      (when (and (is-demon attacker) (aff2-flagged ch +aff2-prot-demons+))
        (incf result 8))
      (when (and (is-devil attacker) (aff2-flagged ch +aff2-prot-devils+))
        (incf result 8)))

    ;; armor class bonus
    (incf result (/ (abs (min 0 (+ (armor-of ch) 300))) 5))

    (setf result (/ (min result 75) 100))

    result))

(defun max-component-dam (ch)
  (max 1
       (* (level-of ch)
          80
          (con-of ch)
          (1+ (remort-gen-of ch)))))

(defmethod send-stats-to-char (ch (k creature))
  (when (and (is-npc k)
             (eql (func-of (shared-of k)) 'fate)
             (not (immortalp ch)))
    (send-to-char ch "You can't stat this mob.~%")
    (return-from send-stats-to-char))
  (with-pagination ((link-of ch))
    (send-to-char ch "~a ~a '&y~a&n'  IDNum: [~5d], AccountNum: [~5d], In room &g[&n~5d&g]&n~%"
                  (sex-of k)
                  (if (is-npc k) "MOB" "PC")
                  (name-of k)
                  (idnum-of k)
                  (unless (is-npc k) (idnum-of (account-of k)))
                  (when (in-room-of k) (number-of (in-room-of k))))
    (when (and (not (is-npc k))
               (immortal-level-p k))
      (send-to-char ch "OlcObj: [~a], OlcMob: [~a]~%"
                    (olc-obj-of k)
                    (olc-mob-of k)))

    (if (is-npc k)
        (send-to-char ch "Alias: ~a, VNum: &g[&y~5d&g]&n, Exist: [~3d]~%"
                      (aliases-of k)
                      (vnum-of k)
                      (number-of (shared-of k)))
        (send-to-char ch "Title: ~a~%"
                      (or (title-of k) "<None>")))

    (send-to-char ch "Race: ~a, Class: ~a~a/~a Gen: ~d~%"
                  (aref +player-races+ (race-of k))
                  (aref +class-names+ (char-class-of k))
                  (if (is-cyborg k)
                      (format nil "(~a)"
                              (aref +borg-subchar-class-names+
                                    (old-char-class-of k)))
                      "")
                  (if (is-remort k)
                      (aref +class-names+ (remort-char-class-of k))
                      "None")
                  (remort-gen-of k))

    (if (is-npc k)
        (let ((rexp (mobile-experience k)))
          (send-to-char ch "Lev: [&y~2d&n], XP: [&y~7d&n/&c~d&n] &r(&n~3d p&r)&n, Align: [~4d]~%"
                        (level-of k)
                        (exp-of k)
                        rexp
                        (floor (* (exp-of k) 100) rexp)
                        (alignment-of k)))
        (send-to-char ch "Lev: [&y~2d&n], XP: [&y~7d&n/&c~d&n], Align: [~4d]~%"
                      (level-of k)
                      (exp-of k)
                      (- (aref +exp-scale+ (1+ (level-of k))) (exp-of k))
                      (alignment-of k)))
    (send-to-char ch "Height:  ~d centimeters , Weight: ~d pounds.~%"
                  (height-of k)
                  (weight-of k))
    (when (not (is-npc k))
      (multiple-value-bind (hours hour-mod)
          (floor (played-time-of k) 3600)
        (send-to-char ch "Created: [~a], Last Logon: [~a], Played [~dh ~dm], Age [~d]~%"
                      (format-timestring nil (birth-time-of k)
                                         :format '((:year 4)
                                                   #\-
                                                   (:month 2)
                                                   #\-
                                                   (:day 2)))
                      (format-timestring nil (login-time-of k)
                                         :format '((:year 4)
                                                   #\-
                                                   (:month 2)
                                                   #\-
                                                   (:day 2)))
                      hours
                      (floor hour-mod 60)
                      (age-of k)))
      (send-to-char ch "Homeroom:[~d], Loadroom: [~d], Clan: &c~a&n~%"
                    (home-room-of k)
                    (load-room-of k)
                    (if (real-clan (clan-of k))
                        (name-of (real-clan (clan-of k)))
                        "NONE"))
      (send-to-char ch "Life: [~d], Thac0: [~d], Reputation: [~4d]~@[, Qpoints: [~d/~d]~]~%"
                    (life-points-of k)
                    (min (thaco (char-class-of k) (level-of k))
                         (thaco (remort-char-class-of k) (level-of k)))
                    (reputation-of k)
                    (immortal-level-p k)
                    (imm-qp-of k)
                    (qp-allowance-of k))
      (send-to-char ch "&yMobKills:&n [~4d], &rPkills:&n [~4d], &gDeaths:&n [~4d]~%"
                    (mobkills-of k)
                    (pkills-of k)
                    (deaths-of k)))
    (send-to-char ch "Str: [&c~d&n]  Int: [&c~d&n]  Wis: [&c~d&n]  Dex: [&c~d&n]  Con: [&c~d&n]  Cha: [&c~d&n]~%"
                  (str-of k)
                  (int-of k)
                  (wis-of k)
                  (dex-of k)
                  (con-of k)
                  (cha-of k))
    (send-to-char ch "Hit p.:[&g~d/~d+~d&n]  Mana p.:[&g~d/~d+~d&n]  Move p.:[&g~d/~d+~d&n]~%"
                  (hitp-of k) (max-hitp-of k) (hit-gain k)
                  (mana-of k) (max-mana-of k) (mana-gain k)
                  (move-of k) (max-move-of k) (move-gain k))
    (send-to-char ch "AC: [&y~d/10&n], Hitroll: [&y~2d&n], Damroll: [&y~2d&n], Speed: [&y~2d&n], Damage Reduction: [&y~2d&n]~%"
                  (armor-of k)
                  (hitroll-of k)
                  (damroll-of k)
                  (speed-of k)
                  (floor (* (get-damage-reduction k) 100)))
    (when (or (not (is-npc k)) (in-room-of k))
      (send-to-char ch "Pr:[&y~2d&n],Rd:[&y~2d&n],Pt:[&y~2d&n],Br:[&y~2d&n],Sp:[&y~2d&n],Ch:[&y~2d&n],Ps:[&y~2d&n],Ph:[&y~2d&n]~%"
                    (aref (saves-of k) 0)
                    (aref (saves-of k) 1)
                    (aref (saves-of k) 2)
                    (aref (saves-of k) 3)
                    (aref (saves-of k) 4)
                    (aref (saves-of k) 5)
                    (aref (saves-of k) 6)
                    (aref (saves-of k) 7)))

    (if (is-npc k)
        (send-to-char ch "Gold:[~8d], Cash:[~8d], (Total: ~d)~%"
                      (gold-of k)
                      (cash-of k)
                      (+ (gold-of k) (cash-of k)))
        (send-to-char ch "Au:[~8d], Bank:[~8d], Cash:[~8d], Enet:[~8d], (Total: ~d)~%"
                      (gold-of k)
                      (cash-of k)
                      (past-bank-of (account-of k))
                      (future-bank-of (account-of k))
                      (+ (gold-of k) (cash-of k)
                         (past-bank-of (account-of k))
                         (future-bank-of (account-of k)))))

    (cond
      ((is-npc k)
       ;; mobiles
       (send-to-char ch "Pos: ~a, Dpos: ~a, Attack: ~a"
                     (aref +position-types+ (position-of k))
                     (aref +position-types+ (default-pos-of (shared-of k)))
                     (aref +attack-hit-text+ (attack-type-of (shared-of k)) 0))
       (when (in-room-of k)
         (send-to-char ch ", &rFT&n: ~a, &yHNT&n: ~a, Timer: ~a"
                       (if (fighting-of k) "Y" "N")
                       (if (hunting-of k) "Y" "N")
                       (timer-of k)))
       (send-to-char ch "~%"))
      ((in-room-of k)
       ;; players
       (send-to-char ch "Pos: ~a, &rFT&n: ~a"
                     (aref +position-types+ (position-of k))
                     (if (fighting-of k) "Y" "N"))))

    (when (link-of k)
      (send-to-char ch ", Connected: ~a, Idle [~d]"
                    (state-of (link-of k))
                    (idle-of (link-of k))))
    (send-to-char ch "~%")

    (when (mounted-of k)
      (send-to-char ch "Mount: ~a~%" (name-of (mounted-of k))))

    (cond
      ((is-npc k)
       (send-to-char ch "NPC flags: &c~a&n~%" (printbits (mob-flags-of k) +action-bits+ "NONE"))
       (send-to-char ch "NPC flags(2): &c~a&n~%" (printbits (mob2-flags-of k) +action2-bits+ "NONE")))
      (t
       (send-to-char ch "PLR: &c~a&n~%" (printbits (plr-bits-of k) +player-bits+ "NONE"))
       (send-to-char ch "PLR2: &c~a&n~%" (printbits (plr2-bits-of k) +player2-bits+ "NONE"))
       (send-to-char ch "PRF: &c~a&n~%" (printbitarray (prefs-of k) +preference-bits+))
       (when (plr-flagged k +plr-frozen+)
         (send-to-char ch "&cFrozen by: ~a" (retrieve-player-name (freezer-id-of k)))
         (when (plusp (thaw-time-of k))
           (send-to-char ch ", will auto-thaw at ~a" (thaw-time-of k)))
         (send-to-char ch "&n~%"))))

    (when (is-npc k)
      (send-to-char ch "Mob Spec: ~a, NPC Dam: ~dd~d, Morale: ~d, Lair: ~d, Ldr: ~d~%"
                    (func-of (shared-of k))
                    (damnodice-of (shared-of k))
                    (damsizedice-of (shared-of k))
                    (morale-of (shared-of k))
                    (lair-of (shared-of k))
                    (leader-of (shared-of k)))

      (when (move-buf-of (shared-of k))
        (send-to-char ch "Move buf: ~a~%" (move-buf-of (shared-of k))))

      (when (eql k (proto-of (shared-of k)))
        (let ((param (func-param-of (shared-of k))))
          (when param
            (send-to-char ch "Spec param:~%~a~%" param)))
        (let ((param (load-param-of (shared-of k))))
          (when param
            (send-to-char ch "Load param:~%~a~%" param)))))

    (when (not (is-npc k))
      (send-to-char ch "Hunger: ~d, Thirst: ~d, Drunk: ~d~%"
                    (get-condition k +full+)
                    (get-condition k +thirst+)
                    (get-condition k +drunk+)))

    (when (and (not (is-npc k)) (plusp (quest-id-of k)))
      (send-to-char ch "Quest [~d]: '~a'~%"
                    (quest-id-of k)
                    (or (quest-name (quest-id-of k)) "None")))

    (when (and (in-room-of k) (or (master-of k) (followers-of k)))
      (send-to-char ch "Master is: ~:[<none>~*~;~a~], Followers are: ~:[<none>~;~:*~{~a~^, ~}~]~%"
                    (master-of k)
                    (and (master-of k) (name-of (master-of k)))
                    (mapcar 'name-of (followers-of k))))

    (when (plusp (aff-flags-of k))
      (send-to-char ch "AFF: &y~a&n~%" (printbits (aff-flags-of k) +affected-bits+ "NONE")))
    (when (plusp (aff2-flags-of k))
      (send-to-char ch "AFF2: &y~a&n~%" (printbits (aff2-flags-of k) +affected2-bits+ "NONE")))
    (when (plusp (aff3-flags-of k))
      (send-to-char ch "AFF3: &y~a&n~%" (printbits (aff3-flags-of k) +affected3-bits+ "NONE")))

    (when (and (eql (position-of k) +pos-sitting+)
               (aff2-flagged k +aff2-meditate+))
      (send-to-char ch "Meditation timer: [~d]~%" (meditate-timer-of k)))

    (when (is-cyborg k)
      (send-to-char ch "Broken component: [~a (~d)], Dam Count: ~d/~d~%"
                    (aref +component-names+ (broken-component-of k) (old-char-class-of k))
                    (broken-component-of k)
                    (total-dam-of k)
                    (max-component-dam k))

      (when (aff3-flagged k +aff3-self-destruct+)
        (send-to-char ch "Self-destruct Timer: [~d]~%"
                      (meditate-timer-of k))))

    (send-to-char ch "Currently speaking: &c~a&n~%"
                  (tongue-name (current-tongue-of k)))

    (when (and (not (is-npc k))
               (in-room-of k)
               (tongues-heard-of k))
      (send-to-char ch "Recently heard: ~{~a~^, ~}~%"
                    (mapcar 'name-of (tongues-heard-of k))))

    (send-to-char ch "Known Languages:~%")
    (loop
       for idnum in (sort (hash-keys *tongues*) #'<)
       as tongue = (gethash idnum *tongues*)
       when (plusp (check-tongue k idnum))
       do (send-to-char ch "&c~3d. ~30a &C~a   &Y[~3d]&n~%"
                        idnum
                        (name-of tongue)
                        (fluency-desc k idnum)
                        (check-tongue k idnum)))

    (when (and (not (is-npc k))
               (grievances-of k))
      (send-to-char ch "Grievances:~%")
      (dolist (grievance (grievances-of k))
        (send-to-char ch "&g~3d. ~a got ~d rep for ~a at ~a&n~%"
                      (player-idnum-of grievance)
                      (retrieve-player-name (player-idnum-of grievance))
                      (reputation-of grievance)
                      (kind-of grievance)
                      (time-of grievance))))

    (when (affected-of k)
      (dolist (aff (affected-of k))
        (send-to-char ch "SPL: (~3d~a) [~2d] ~a(~d) &y~24a&n "
                      (1+ (duration-of aff))
                      (if (is-instant-of aff) "sec" "hr")
                      (level-of aff)
                      (owner-of aff)
                      (spell-to-str (kind-of aff)))
        (when (plusp (modifier-of aff))
          (send-to-char ch "~d to ~a"
                        (modifier-of aff)
                        (aref +apply-types+ (location-of aff)))
          (when (plusp (bitvector-of aff))
            (send-to-char ch ", ")))

        (when (bitvector-of aff)
          (case (aff-index-of aff)
            (1
             (send-to-char ch "sets ~a" (printbits (bitvector-of aff) +affected-bits+ "NONE")))
            (2
             (send-to-char ch "sets ~a" (printbits (bitvector-of aff) +affected2-bits+ "NONE")))
            (3
             (send-to-char ch "sets ~a" (printbits (bitvector-of aff) +affected3-bits+ "NONE")))))
        (send-to-char ch "~%")))))

(defun perform-oload (ch count vnum)
  (let ((last-obj nil))
    (unless (real-object-proto vnum)
      (send-to-char ch "There is no object thang with that number.~%")
      (return-from perform-oload))

    (when (zerop count)
      (send-to-char ch "POOF!  Congratulations!  You've created nothing!~%")
      (return-from perform-oload))

    (when (> count 100)
      (send-to-char ch "You can't possibly need THAT many!~%")
      (return-from perform-oload))

    (dotimes (idx count)
      (let ((obj (read-object vnum)))
        (setf (creation-method-of obj) :imm)
        (setf (creator-of obj) (idnum-of ch))
        (obj-to-room obj (in-room-of ch))
        (setf last-obj obj)))

    (act ch :place-emit "$n makes a quaint, magical gesture with one hand.")
    (act ch :item last-obj
         :subject-emit (format nil "You create $p.~[~;~:;~:* (x~d)~]" count)
         :place-emit (format nil "$n has created $p!~[~;~:;~:* (x~d)~]" count))
    (slog "(GC) ~a oloaded ~a[~d] at ~d~[~;~:;~:* (x~d)~]"
          (name-of ch)
          (name-of last-obj)
          (vnum-of last-obj)
          (number-of (in-room-of ch))
          count)))

(defun perform-pload (ch vnum count target)
  (let ((last-obj nil))
    (unless (real-object-proto vnum)
      (send-to-char ch "There is no object thang with that number.~%")
      (return-from perform-pload))

    (when (zerop count)
      (send-to-char ch "POOF!  Congratulations!  You've created nothing!~%")
      (return-from perform-pload))

    (when (> count 100)
      (send-to-char ch "You can't possibly need THAT many!~%")
      (return-from perform-pload))

    (dotimes (idx count)
      (let ((obj (read-object vnum)))
        (setf (creation-method-of obj) :imm)
        (setf (creator-of obj) (idnum-of ch))
        (obj-to-char obj target)
        (setf last-obj obj)))

    (cond
      ((eql target ch)
       (act ch :item last-obj
            :subject-emit (format nil "You create $p.~[~;~:;~:* (x~d)~]" count)
            :place-emit "$n does something suspicious and alters reality.")
        (slog "(GC) ~a ploaded ~a[~d] onto self at ~d~[~;~:;~:* (x~d)~]"
              (name-of ch)
              (name-of last-obj)
              (vnum-of last-obj)
              (number-of (in-room-of ch))
              count))
      (t
        (act ch
             :target target
             :item last-obj
             :subject-emit (format nil "You load $p onto $N.~[~;~:;~:* (x~d)~]" count)
             :target-emit (format nil "$n causes $p to appear in your hands.~[~;~:;~:* (x~d)~]" count)
             :not-target-emit "$n does something suspicious and alters reality.")
        (slog "(GC) ~a ploaded ~a[~d] onto ~a ~a[~d] at ~d~[~;~:;~:* (x~d)~]"
              (name-of ch)
              (name-of last-obj)
              (vnum-of last-obj)
              (if (is-npc target) "MOB" "PC")
              (name-of target)
              (if (is-npc target) (vnum-of target) (idnum-of target))
              (number-of (in-room-of ch))
              count)))))

(defun perform-vis (ch)
  (let ((old-invis-level (invis-level-of ch)))
    (cond
    ((and (zerop (invis-level-of ch))
          (not (aff-flagged ch +aff-hide+))
          (not (aff-flagged ch +aff-invisible+)))
     (send-to-char ch "You are already fully visible.~%"))
    (t
     (setf (invis-level-of ch) 0)
     (dolist (tch (people-of (in-room-of ch)))
       (when (and (not (eql tch ch))
                  (is-visible-to ch tch)
                  (< (level-of tch) old-invis-level))
         (act ch :target tch :target-emit "You suddenly realize that $n is standing beside you.")))
     (send-to-char ch "You are now fully visible.~%")))))

(defun perform-invis (ch level)
  (unless (is-npc ch)
    (let ((old-invis-level (invis-level-of ch)))
      (setf (invis-level-of ch) 0)
      (dolist (tch (people-of (in-room-of ch)))
        (unless (eql ch tch)
          (cond
            ((> old-invis-level (level-of tch) level)
             (act ch :target tch
                  :target-emit "You suddenly realize that $n is standing beside you."))
            ((< old-invis-level (level-of tch) level)
             (act ch :target tch
                  :target-emit "You blink and suddenly realize that $n is gone."))))))

    (setf (invis-level-of ch) level)
    (send-to-char ch "Your invisibility level is ~d.~%" level)))

(defun describe-uptime ()
  (multiple-value-bind (days secs)
      (floor (timestamp-difference (now) *boot-time*) +seconds-per-day+)
    (multiple-value-bind (hours secs)
        (floor secs +seconds-per-hour+)
      (multiple-value-bind (mins secs)
          (floor secs +seconds-per-minute+)
        (declare (ignore secs))
        (format nil "Up since ~a: ~d~:* day~p, ~2,'0d:~2,'0d"
                (format-timestring nil *boot-time*
                                   :format +asctime-format+)
                days
                hours
                mins)))))

(defun perform-unaffect (ch target)
  (cond
    ((affected-of target)
     (send-to-char target "There is a brief flash of light!~%You feel slightly different.~%")
     (send-to-char ch "All spells removed.~%")
     (loop while (affected-of target) do
          (affect-remove target (first (affected-of target)))))
    ((eql ch target)
     (send-to-char ch "You don't have any affections!~%"))
    (t
     (send-to-char ch "~a does not have any affections!~%" (name-of target)))))

(defun perform-reroll (ch target)
  (roll-real-abils target)
  (send-to-char target "Your stats have been rerolled.~%")
  (slog "(GC) ~a has rerolled ~a" (name-of ch) (name-of target))
  (send-to-char ch "New stats: Str: ~d, Int ~d, Wis ~d, Dex ~d, Con ~d, Cha ~d~%"
                (str-of target)
                (int-of target)
                (wis-of target)
                (dex-of target)
                (con-of target)
                (cha-of target)))

(defun perform-wizutil (ch target-str func)
  (let ((vict (get-char-vis ch target-str)))
    (when (null vict)
      (let ((pid (retrieve-player-idnum target-str)))
        (when (null pid)
          (send-to-char ch "You don't see anyone like that.~%")
          (return-from perform-wizutil))

        (setf vict (load-player-from-xml pid))

        (unless vict
          (send-to-char ch "That player could not be loaded.~%")
          (return-from perform-wizutil))))

    (if (> (level-of vict) (level-of ch))
        (send-to-char ch "Hmmm...you'd better not.~%")
        (funcall func ch vict))

    (unless (is-npc vict)
      (save-player-to-xml vict))))

(defun make-plr-toggle-func (flag flag-name)
  (lambda (ch vict)
    (setf (plr-bits-of vict) (logxor (plr-bits-of vict) flag))
    (let ((state (if (plr-flagged vict flag) "ON" "OFF")))
      (send-to-char ch "~a turned ~a for ~a.~%"
                    flag-name state (name-of vict))
      (mudlog 'info t "~a turned ~a ~a for ~a"
              (name-of ch)
              flag-name
              state
              (name-of vict)))))

(defun perform-freeze (ch vict length-str)
  (when (eql ch vict)
    (send-to-char ch "Oh, yeah, THAT'S real smart...~%")
    (return-from perform-freeze))

  (let* ((match (scan #/^(\d+)([smhd])?/ length-str))
         (amt (when (regref match 1) (parse-integer (regref match 1))))
         (units (or (regref match 2) "d"))
         (time-desc "forever"))
    (cond
      ((string= length-str "forever")
       (setf (thaw-time-of vict) -1))
      ((null match)
       (send-to-char ch "Usage: freeze <target> [ XXXs | XXXm | XXXh | XXXd | forever]~%"))
      ((string= "d" units)
       (setf (thaw-time-of vict) (timestamp+ (now) amt :day))
       (setf time-desc (format nil "for ~d day~:p" amt)))
      ((string= "m" units)
       (setf (thaw-time-of vict) (timestamp+ (now) amt :minute))
       (setf time-desc (format nil "for ~d minute~:p" amt)))
      ((string= "h" units)
       (setf (thaw-time-of vict) (timestamp+ (now) amt :hour))
       (setf time-desc (format nil "for ~d hour~:p" amt)))
      ((string= "s" units)
       (setf (thaw-time-of vict) (timestamp+ (now) amt :second))
       (setf time-desc (format nil "for ~d second~:p" amt)))
      (t
       (error "Can't happen")))

    (setf (plr-bits-of vict) (logior (plr-bits-of vict) +plr-frozen+))
    (setf (freeze-level-of vict) (level-of ch))
    (setf (freezer-id-of vict) (idnum-of ch))

    (send-to-char vict "A bitter wind suddenly rises and drains every erg of heat from your body!~%You feel frozen!~%")

    (send-to-char ch "~a has been frozen ~a~%" (name-of vict) time-desc)
    (mudlog 'info t "(GC) ~a has frozen ~a ~a"
            (name-of ch)
            (name-of vict)
            time-desc)

    (when (in-room-of vict)
      (act vict :place-emit "A sudden cold wind conjured from nowhere freezes $n!"))))


(defun perform-thaw (ch vict)
  (unless (plr-flagged vict +plr-frozen+)
    (send-to-char ch "Sorry, your victim is not morbidly encased in ice at the moment.~%")
    (return-from perform-thaw))

  (when (> (freeze-level-of vict) (level-of ch))
    (send-to-char ch "Sorry, a level ~d imm froze ~a.~%"
                  (freeze-level-of vict) (name-of vict))
    (return-from perform-thaw))

  (mudlog 'info t "(GC) ~a has un-frozen ~a"
          (name-of ch)
          (name-of vict))
  (setf (plr-bits-of vict) (logandc2 (plr-bits-of vict)
                                     +plr-frozen+))
  (send-to-char vict "A fireball suddenly explodes in front of you, melting the ice!~%You feel thawed.~%")
  (send-to-char ch "Thawed.~%")
  (when (in-room-of vict)
    (act vict :place-emit "A sudden fireball conjured from nowhere thaws $n!")))

(defcommand (ch "stat" "room") (:immortal)
  (send-stats-to-char ch (in-room-of ch)))

(defcommand (ch "stat" "room" spec) (:immortal)
  (let ((room (find-target-room ch spec)))
    (when room
     (send-stats-to-char ch room))))

(defun perform-stat (ch thing)
  (let ((objs (resolve-alias ch thing (append
                                              (coerce (remove nil (equipment-of ch)) 'list)
                                              (carrying-of ch)
                                              (people-of (in-room-of ch))
                                              (contents-of (in-room-of ch))))))
    (cond
      ((rest objs)
       (send-to-char ch "You can only stat one thing at a time!~%"))
      (objs
       (send-stats-to-char ch (first objs)))
      (t
       (let ((c (get-char-vis ch thing)))
         (if c
             (send-stats-to-char ch c)
             (let ((o (get-obj-vis ch thing)))
               (if o
                   (send-stats-to-char ch o)
                   (send-to-char ch "Nothing around by that name.~%")))))))))

(defcommand (ch "stat" thing) (:immortal)
  (perform-stat ch thing))

(defcommand (ch "echo") (:immortal :dead)
  (send-to-char ch "Yes, but what?~%"))

(defcommand (ch "echo" str) (:immortal :dead)
  (let* ((mort-see (act-escape str))
         (imm-see (format nil "[$n] ~a" mort-see)))
    (dolist (tch (people-of (in-room-of ch)))
      (act ch :target tch
           :target-emit (if (> (level-of tch) (level-of ch))
                            imm-see
                            mort-see)))))

(defcommand (ch "send") (:immortal)
  (send-to-char ch "Send what to who?~%"))

(defcommand (ch "send" stuff) (:immortal)
  (declare (ignore stuff))
  (send-to-char ch "What do you want to send?~%"))

(defcommand (ch "send" name stuff) (:immortal)
  (let* ((tchs (resolve-alias ch name (mapcar 'actor-of *cxns*)))
         (tch (first tchs)))
    (cond
      ((null tchs)
       (send-to-char ch "Nobody by that name.~%"))
      (t
       (send-to-char ch "You send '~a' to ~a.~%" stuff (name-of tch))
       (send-to-char tch "~a~%" stuff)))))

(defcommand (ch "at") (:immortal)
  (send-to-char ch "Where do you want to do something?~%"))

(defcommand (ch "at" ignore) (:immortal)
  (declare (ignore ignore))
  (send-to-char ch "What do you want to do there?~%"))

(defcommand (ch "at" roomstr command) (:immortal)
  (let ((original-room (in-room-of ch))
        (target-room (find-target-room ch roomstr)))
    (when target-room
      (char-from-room ch t)
      (char-to-room ch target-room)
      (interpret-command ch command)
      ;; only return the char if they're still there
      (when (eql target-room (in-room-of ch))
        (char-from-room ch t)
        (char-to-room ch original-room)))))

(defcommand (ch "goto") (:immortal)
  (send-to-char ch "Where do you want to go today?~%"))

(defcommand (ch "goto" target) (:immortal)
  (let ((destination (find-target-room ch target)))
    (when destination
      (perform-goto ch destination t))))

(defcommand (ch "distance") (:immortal)
  (send-to-char ch "You need to supply a target to calculate the distance.~%"))

(defcommand (ch "distance" target) (:immortal)
  (let ((room (find-target-room ch target)))
    (when room
      (let ((steps (find-distance (in-room-of ch) room)))
        (if steps
            (send-to-char ch "Room ~d is ~d steps away.~%"
                          (number-of room)
                          steps)
            (send-to-char ch "There is no valid path to room ~d.~%"
                          (number-of room)))))))

(defcommand (ch "transport") (:immortal)
  (send-to-char ch "Whom do you wish to transport?~%"))

(defcommand (ch "transport" targets) (:immortal)
  (dolist (name (split-sequence #\space targets :remove-empty-subseqs t))
    (let ((tch (get-char-vis ch name)))
      (if tch
          (perform-transport ch tch)
          (send-to-char ch "You can't detect any '~a'~%" name)))))

(defcommand (ch "teleport") (:immortal)
  (send-to-char ch "Whom do you wish to teleport?~%"))

(defcommand (ch "teleport" name) (:immortal)
  (declare (ignore name))
  (send-to-char ch "Where do you wish to send this person?~%"))

(defcommand (ch "teleport" name "to" target) (:immortal)
  (let ((tch (get-char-vis ch name))
        (room (find-target-room ch target)))
    (cond
      ((null tch)
       (send-to-char ch "Nobody around by that name.~%"))
      ((null room)
       nil)
      ((eql ch tch)
       (send-to-char ch "Use 'goto' to teleport yourself.~%"))
      (t
       (send-to-char ch "Okay.~%")
       (act tch :place-emit "$n disappears in a puff of smoke.")
       (char-from-room tch t)
       (char-to-room tch room nil)
       (act tch :target ch
            :subject-emit "$N has teleported you!"
            :place-emit "$n arrives from a puff of smoke.")
       (look-at-room tch (in-room-of tch) nil)
       (slog "(GC) ~a has teleported ~a to ~a[~d]"
             (name-of ch)
             (name-of tch)
             (name-of room)
             (number-of room))))))

(defcommand (ch "vnum") (:immortal)
  (send-to-char ch "Usage: vnum { obj | mob } <name>~%"))

(defcommand (ch "vnum" other) (:immortal)
  (declare (ignore other))
  (send-to-char ch "Usage: vnum { obj | mob } <name>~%"))

(defcommand (ch "vnum" "mobiles" name) (:immortal)
  (when (zerop (vnum-prototypes ch name #\y *mobile-prototypes*))
    (send-to-char ch "No mobiles by that name.~%")))

(defcommand (ch "vnum" "objects" name) (:immortal)
  (when (zerop (vnum-prototypes ch name #\g *object-prototypes*))
    (send-to-char ch "No objects by that name.~%")))

(defcommand (ch "shutdown") (:immortal)
  (send-to-char ch "Shutting down...~%")
  (setf *shutdown* t))

(defcommand (ch "shutdown" mode) (:immortal)
  (declare (ignore mode))
  (send-to-char ch "Shutting down...~%")
  (setf *shutdown* t))

(defcommand (ch "eval" expr) (:wizard)
  "Command function to evaluate an arbitrary lisp expression."
  (handler-case
      (let* ((*standard-output* (make-string-output-stream))
             (*package* (find-package "TEMPUS"))
             (results (multiple-value-list (eval `(let ((self ,ch))
                                                    (declare (ignorable self))
                                                    ,(read-from-string expr))))))
        (cxn-write (link-of ch) "~a~{~s~%~}"
                   (string-replace "&"
                                   (get-output-stream-string *standard-output*)
                                   "&&")
                   results))
    (error (err)
        (cxn-write (link-of ch) "ERROR: ~a~%" err))))

(defcommand (ch "mload") (:wizard)
  (send-to-char ch "Usage: mload <mobile vnum number>~%"))

(defcommand (ch "mload" vnum-str) (:wizard)
  (unless (every #'digit-char-p vnum-str)
    (send-to-char ch "Usage: mload <mobile vnum number>~%")
    (return))

  (let ((vnum (parse-integer vnum-str)))
    (unless (real-mobile-proto vnum)
      (send-to-char ch "There is no mobile thang with that number.~%")
      (return))

    (let ((mob (read-mobile vnum)))
      (char-to-room mob (in-room-of ch))
      (act ch :place-emit "$n makes a quaint, magical gesture with one hand.")
      (act ch :target mob
           :subject-emit "You create $N."
           :place-emit "$n has created $N!")
      (slog "(GC) ~a mloaded ~a[~d] at ~d"
            (name-of ch)
            (name-of mob)
            (vnum-of mob)
            (number-of (in-room-of ch)))
      ;; TODO: trigger prog load here
      )))

(defcommand (ch "oload") (:wizard)
  (send-to-char ch "Usage: oload [count] <object vnum>~%"))

(defcommand (ch "oload" vnum-str) (:wizard)
  (unless (every #'digit-char-p vnum-str)
    (send-to-char ch "Usage: oload <object vnum number> [count]~%")
    (return))

  (perform-oload ch 1 (parse-integer vnum-str)))

(defcommand (ch "oload" count-str vnum-str) (:wizard)
  (unless (and (every #'digit-char-p vnum-str)
               (every #'digit-char-p count-str))
    (send-to-char ch "Usage: oload <object vnum number> [count]~%")
    (return))

  (perform-oload ch (parse-integer count-str) (parse-integer vnum-str)))

(defcommand (ch "pload") (:wizard)
  (send-to-char ch "Usage: pload [count] <object vnum> [<target char>]~%"))

(defcommand (ch "pload" vnum-str) (:wizard)
  (unless (every #'digit-char-p vnum-str)
    (send-to-char ch "Usage: pload [count] <object vnum> [<target char>]~%")
    (return))

  (perform-pload ch (parse-integer vnum-str) 1 ch))

(defcommand (ch "pload" arg1 arg2) (:wizard)
  (unless (every #'digit-char-p arg1)
    (send-to-char ch "Usage: pload [count] <object vnum> [<target char>]~%")
    (return))

  (if (every #'digit-char-p arg2)
      (perform-pload ch (parse-integer arg2) (parse-integer arg1) ch)
      (let ((targets (resolve-alias ch arg2 (append (people-of (in-room-of ch))
                                                           *characters*))))
        (cond
          ((null targets)
           (send-to-char ch "You can't find any '~a'.~%" arg2))
          ((rest targets)
           (send-to-char ch "You can only pload onto one creature!~%"))
          (t
           (perform-pload ch (parse-integer arg1) 1 (first targets)))))))

(defcommand (ch "pload" count-str vnum-str target-str) (:wizard)
  (unless (and (every #'digit-char-p count-str)
               (every #'digit-char-p vnum-str))
    (send-to-char ch "Usage: pload [count] <object vnum> [<target char>]~%")
    (return))

  (let ((targets (resolve-alias ch target-str (append (people-of (in-room-of ch))
                                                             *characters*))))
    (cond
      ((null targets)
       (send-to-char ch "You can't find any '~a'.~%" target-str))
      ((rest targets)
       (send-to-char ch "You can only pload onto one creature!~%"))
      (t
       (perform-pload ch
                      (parse-integer vnum-str)
                      (parse-integer count-str)
                      (first targets))))))

(defcommand (ch "vstat" "mobile" vnum) (:immortal)
  (unless (every #'digit-char-p vnum)
    (send-to-char ch "Usage: vstat { { obj | mob } <number> | <alias> }~%")
    (return))

  (let ((mob (real-mobile-proto (parse-integer vnum))))
    (if mob
        (send-stats-to-char ch mob)
        (send-to-char ch "There is no mobile with that vnum.~%"))))

(defcommand (ch "vstat" "object" vnum) (:immortal)
  (unless (every #'digit-char-p vnum)
    (send-to-char ch "Usage: vstat { { obj | mob } <number> | <alias> }~%")
    (return))

  (let ((obj (real-object-proto (parse-integer vnum))))
    (if obj
        (send-stats-to-char ch obj)
        (send-to-char ch "There is no object with that vnum.~%"))))

(defcommand (ch "vstat" thing) (:immortal)
  (let ((objs (resolve-alias ch thing (append
                                              (coerce (remove nil (equipment-of ch)) 'list)
                                              (carrying-of ch)
                                              (people-of (in-room-of ch))
                                              (contents-of (in-room-of ch))))))
    (cond
      ((rest objs)
       (send-to-char ch "You can only vstat one thing at a time!~%"))
      ((null objs)
       (send-to-char ch "Nothing around by that name.~%"))
      ((typep (first objs) 'player)
       (send-to-char ch "You can't vstat a player!~%"))
      ((typep (first objs) 'creature)
       (send-stats-to-char ch (real-mobile-proto (vnum-of (first objs)))))
      ((typep (first objs) 'obj-data)
       (send-stats-to-char ch (real-object-proto (vnum-of (first objs)))))
      (t
       (error "Can't happen")))))

(defcommand (ch "purge") (:immortal)
  (let ((things (append
                 (remove-if (lambda (x) (typep x 'player)) (people-of (in-room-of ch)))
                 (contents-of (in-room-of ch)))))
    (cond
      (things
       (act ch :place-emit "$n gestures... You are surrounded by scorching flames!")
       (act ch :all-emit "The world seems a little cleaner.")
       (slog "(GC) ~a has purged room ~a"
             (name-of ch)
             (name-of (in-room-of ch)))
       (perform-purge ch things))
      (t
       (send-to-char ch "The room already seems pretty clean.~%")))))

(defcommand (ch "purge" thing) (:immortal)
  (let ((things (resolve-alias ch thing (append
                                                (people-of (in-room-of ch))
                                                (contents-of (in-room-of ch))))))
    (loop
       for obj-sublist on things
       as obj = (first obj-sublist)
       as next-obj = (second obj-sublist)
       as counter from 1
       do
         (when (or (null next-obj)
                   (string/= (name-of next-obj) (name-of obj)))
           (act ch :item obj
                :all-emit (format nil "$n disintegrate$% $p.~[~;~:;~:* (x~d)~]" counter))
           (setf counter 0)))

    (perform-purge ch things)))

(defcommand (ch "advance")
    (send-to-char ch "Advance who?~%"))

(defcommand (ch "advance" name) ()
    (declare (ignore name))
    (send-to-char ch "Advance them to what level?~%"))

(defcommand (ch "advance" target-str level-str) (:immortal)
  (let* ((targets (resolve-alias ch target-str
                                        (people-of (in-room-of ch))))
         (target (first targets))
         (level (and (every #'digit-char-p level-str)
                     (parse-integer level-str))))
    (cond
      ((null level)
       (send-to-char ch "That's not a level!~%"))
      ((null targets)
       (send-to-char ch "Who do you want to advance?~%"))
      ((rest targets)
       (send-to-char ch "You can only advance one person at a time.~%"))
      ((> level (level-of ch))
       (send-to-char ch "Yeah, right.~%"))
      ((<= (level-of ch) (level-of target))
       (send-to-char ch "Maybe that's not such a great idea.~%"))
      ((is-npc target)
       (send-to-char ch "NO!  Not on NPCs!~%"))
      ((> level +lvl-grimp+)
       (send-to-char ch "~d is the highest possible level.~%" +lvl-grimp+))
      ((< level (level-of target))
       (mudlog 'info t "(GC) ~a has advanced ~a to level ~d (from ~d)"
             (name-of ch)
             (name-of target)
             level
             (level-of target))
       (send-to-char ch "You got it.~%")
       (do-start target nil)
       (setf (level-of target) level)
       (gain-exp-regardless target
                            (- (aref +exp-scale+ level) (exp-of target)))
       (save-player-to-xml target))
      (t
       (act ch :target target
            :subject-emit "You got it.~%"
            :target-emit "$n makes some strange gestures.
A strange feeling comes upon you,
Like a giant hand, light comes down
from above, grabbing your body, that
begins to pulse with colored lights
from inside.

Your head seems to be filled with demons
from another plane as your body dissolves
to the elements of time and space itself.
Suddenly a silent explosion of light
snaps you back to reality.
You feel slightly different.")
       (mudlog 'info t "(GC) ~a has advanced ~a to level ~d (from ~d)"
             (name-of ch)
             (name-of target)
             level
             (level-of target))
       (gain-exp-regardless target
                            (- (aref +exp-scale+ level) (exp-of target)))
       (save-player-to-xml target)))))

(defcommand (ch "restore") (:immortal)
  (send-to-char ch "Whom do you wish to restore?~%"))

(defcommand (ch "restore" target-str) (:immortal)
  (let* ((targets (resolve-alias ch target-str
                                        (append
                                         (people-of (in-room-of ch))
                                         *characters*))))
    (cond
      ((null targets)
       (send-to-char ch "You don't see anyone like that.~%"))
      (t
       (send-to-char ch "You got it.~%")
       (dolist (target targets)
         (act ch :target target
              :target-emit "You have been fully healed by $n!")
         (mudlog 'info t "~a has been restored by ~a" (name-of target) (name-of ch))
         (restore-creature target))))))

(defcommand (ch "invis") (:immortal)
  "Switch between totally visible and maximally invisible"
  (cond
    ((is-npc ch)
     (send-to-char ch "You can't do that!~%"))
    ((zerop (invis-level-of ch))
     (perform-invis ch (max (level-of ch) 70)))
    (t
     (perform-vis ch))))

(defcommand (ch "invis" level-str) (:immortal)
  "Set immortal invisibility level"
  (let ((level (when (every #'digit-char-p level-str)
                 (parse-integer level-str))))
    (cond
      ((is-npc ch)
       (send-to-char ch "You can't do that!~%"))
      ((null level)
       (send-to-char ch "That's not a proper invisibility level.~%"))
      ((> level (level-of ch))
       (send-to-char ch "You can't go invisible above your own level.~%"))
      ((< level 1)
       (perform-vis ch))
      (t
       (perform-invis ch level)))))

(defcommand (ch "gecho") (:immortal)
  (send-to-char ch "That must be a mistake...~%"))

(defcommand (ch "gecho" msg) (:immortal)
  (dolist (cxn *cxns*)
    (when (and (typep cxn 'tempus-cxn)
               (eql (state-of cxn) 'playing)
               (actor-of cxn)
               (not (eql (actor-of cxn) ch))
               (not (pref-flagged ch +pref-nogecho+))
               (not (plr-flagged ch +plr-olc+))
               (not (plr-flagged ch +plr-writing+)))
      (if (and (immortal-level-p (actor-of cxn))
               (> (level-of (actor-of cxn)) (level-of ch)))
          (send-to-char (actor-of cxn) "[~a-g] ~a~%" (name-of ch) msg)
          (send-to-char (actor-of cxn) "~a~%" msg))))
  (send-to-char ch "~a~%" msg))

(defcommand (ch "poofin") (:immortal)
  (setf (poofin-of ch) "")
  (send-to-char ch "You got it.~%"))

(defcommand (ch "poofin" poofin) (:immortal)
  (setf (poofin-of ch) poofin)
  (send-to-char ch "You got it.~%"))

(defcommand (ch "poofout") (:immortal)
  (setf (poofout-of ch) "")
  (send-to-char ch "You got it.~%"))

(defcommand (ch "poofout" poofout) (:immortal)
  (setf (poofout-of ch) poofout)
  (send-to-char ch "You got it.~%"))

(defcommand (ch "dc") (:coder)
  (send-to-char ch "Usage: dc <connection number> (type USERS for a list)~%"))

(defcommand (ch "dc" num-str) (:coder)
  (let* ((num (when (every #'digit-char-p num-str) (parse-integer num-str)))
         (cxn (when num (find num *cxns* :key 'cxn-fd))))
    (cond
      ((null num)
       (send-to-char ch "You must specify a connection number to disconnect.~%"))
      ((null cxn)
       (send-to-char ch "No such connection.~%"))
      ((and (actor-of cxn) (>= (level-of (actor-of cxn)) (level-of ch)))
       (send-to-char ch "Umm... maybe that's not such a good idea...~%"))
      (t
       (setf (state-of cxn) 'disconnecting)
       (send-to-char ch "Connection #~d closed.~%" num)
       (slog "(GC) Connection closed by ~a" (name-of ch))))))

(defcommand (ch "date") (:immortal)
  (send-to-char ch "~a~%"
                (format-timestring nil (now)
                                   :format local-time:+asctime-format+)))

(defcommand (ch "uptime") (:immortal)
  (send-to-char ch "~a~%" (describe-uptime)))

(defcommand (ch "last") (:immortal)
  (send-to-char ch "For whom do you wish to search?~%"))

(defcommand (ch "last" target-str) (:immortal)
  (let ((pid (retrieve-player-idnum target-str)))
    (unless pid
      (send-to-char ch "There is no such player.~%")
      (return))

    (let ((vict (load-player-from-xml pid)))
      (unless vict
        (send-to-char ch "Unable to load player record.~%")
        (slog "Unable to load character ~d for 'last' command" pid)
        (return))

      (when (and (> (level-of vict) (level-of ch))
                 (< (level-of ch) +lvl-grimp+))
        (send-to-char ch "You are not sufficiently godly for that!~%")
        (return))

      (send-to-char ch "[~5d] [~2d ~a] ~12a : ~a~%"
                    (idnum-of vict)
                    (level-of vict)
                    (char-class-name (char-class-of vict))
                    (name-of vict)
                    (format-timestring nil (login-time-of vict)))
      (when (has-mail (idnum-of vict))
        (send-to-char ch "Player has unread mail.~%")))))

(defcommand (ch "force") (:wizard)
  (send-to-char ch "You want to force who what?~%"))

(defcommand (ch "force" target) (:wizard)
  (declare (ignore target))
  (send-to-char ch "What do you want to force them to do?~%"))

(defcommand (ch "force" target "to" command) (:wizard)
  (let ((victs (resolve-alias ch target (people-of (in-room-of ch)))))
    (cond
      ((null victs)
       (send-to-char ch "You don't see any '~a' here.~%" target))
      (t
       (send-to-char ch "You got it.~%")
       (dolist (vict victs)
         (tempus::interpret-command vict command))))))

(defcommand (ch "zreset") (:immortal)
  (send-to-char ch "You must specify a zone.~%"))

(defcommand (ch "zreset" "*") (:immortal)
  (dolist (zone *zone-table*)
    (reset-zone zone)
    (send-to-zone zone "You feel a strangely refreshing breeze.~%"))
  (send-to-char ch "Reset world.~%")
  (mudlog 'info t  "(GC) ~a reset entire world" (name-of ch)))

(defcommand (ch "zreset" zone-spec) (:immortal)
  (let ((zone (cond
                ((string= zone-spec ".")
                 ;; reset current zone
                 (zone-of (in-room-of ch)))
                ((every #'digit-char-p zone-spec)
                 ;; reset specific zone
                 (real-zone (parse-integer zone-spec))))))
    (if (null zone)
        (send-to-char ch "Invalid zone number.~%")
        (progn
          (reset-zone zone)
          (send-to-zone zone "You feel a strangely refreshing breeze.~%")
          (send-to-char ch "Reset zone ~d : ~a~%"
                        (number-of zone)
                        (name-of zone))
          (act ch :place-emit "$n waves $s hand.")
          (slog "(GC) ~a reset zone ~d (~a)"
                (name-of ch)
                (number-of zone)
                (name-of zone))))))

(defcommand (ch "unaffect") (:wizard)
  (send-to-char ch "Yes, but who?~%"))

(defcommand (ch "unaffect" target-str) (:wizard)
  (perform-wizutil ch target-str 'perform-unaffect))

(defcommand (ch "reroll") (:wizard)
  (send-to-char ch "Yes, but who?~%"))

(defcommand (ch "reroll" target-str) (:wizard)
  (perform-wizutil ch target-str 'perform-reroll))

(defcommand (ch "notitle") (:wizard)
  (send-to-char ch "Yes, but who?~%"))

(defcommand (ch "notitle" target-str) (:wizard)
  (perform-wizutil ch target-str
                   (make-plr-toggle-func +plr-notitle+ "notitle")))

(defcommand (ch "nopost") (:wizard)
  (send-to-char ch "Yes, but who?~%"))

(defcommand (ch "nopost" target-str) (:wizard)
  (perform-wizutil ch target-str
                   (make-plr-toggle-func +plr-nopost+ "nopost")))

(defcommand (ch "squelch") (:wizard)
  (send-to-char ch "Yes, but who?~%"))

(defcommand (ch "squelch" target-str) (:wizard)
  (perform-wizutil ch target-str
                   (make-plr-toggle-func +plr-noshout+ "squelch")))

(defcommand (ch "freeze") (:wizard)
  (send-to-char ch "Yes, but who?~%"))

(defcommand (ch "freeze" target-str) (:wizard)
  (perform-wizutil ch target-str
                   (lambda (ch vict)
                     (perform-freeze ch vict "1d"))))

(defcommand (ch "freeze" target-str length-str) (:wizard)
  (perform-wizutil ch target-str
                   (lambda (ch vict)
                     (perform-freeze ch vict length-str))))

(defcommand (ch "thaw") (:wizard)
  (send-to-char ch "Yes, but who?~%"))

(defcommand (ch "thaw" target-str) (:wizard)
  (perform-wizutil ch target-str 'perform-thaw))

(defcommand (ch "users") (:wizard)
  (with-pagination ((link-of ch))
    (send-to-char ch " Num Account      Character     State          Idl Login@   Site~% --- ------------ ------------- --------------- -- -------- ---------------~%")
    (let ((displayed-count 0))
      (dolist (cxn *cxns*)
        (when (typep cxn 'tempus-cxn)
          (incf displayed-count)
          (send-to-char ch " ~3d ~12a ~a ~(~15a~) ~2d ~8a &g~a&n~%"
                        (cxn-fd cxn)
                        (if (account-of cxn)
                            (name-of (account-of cxn))
                            "   -   ")
                        (cond
                          ((null (actor-of cxn))
                           "      -      ")
                          ((original-actor-of cxn)
                           (format nil "&c~13a&n"
                                   (name-of (original-actor-of cxn))))
                          ((immortal-level-p (actor-of cxn))
                           (format nil "&g~13a&n"
                                   (name-of (actor-of cxn))))
                          (t
                           (format nil "~13a" (name-of (actor-of cxn)))))
                        (state-of cxn)
                        (idle-of cxn)
                        (format-timestring nil (connect-time-of cxn)
                                           :format '((:hour 2 #\0) #\:
                                                     (:min 2 #\0) #\:
                                                     (:sec 2 #\0)))
                        (peer-addr cxn))))
      (send-to-char ch "~d visible socket~:p connected.~%" displayed-count))))

(defcommand (ch "badge") (:immortal)
  (cond
    ((is-npc ch)
     (send-to-char ch "You've got no badge.~%"))
    (t
     (setf (badge-of ch) "")
     (send-to-char ch "Okay, you've got a blank badge now.~%"))))

(defcommand (ch "badge" badge) (:immortal)
  (cond
    ((is-npc ch)
     (send-to-char ch "You've got no badge.~%"))
    ((> (length badge) +max-badge-length+)
     (send-to-char ch "Sorry, badges can't be longer than ~d characters.~%"
                   +max-badge-length+))
    (t
     (setf (badge-of ch) (string-upcase badge))
     (send-to-char ch "Okay, your badge is now ~a.~%" (badge-of ch)))))

(defcommand (ch "tester") (:tester)
  (send-to-char ch "Options are:
    advance <level>
    unaffect
    reroll
    stat
    goto
    restore
    class <char_class>
    race <race>
    remort <char_class>
    maxhit <value>
    maxmana <value>
    maxmove <value>
    nohassle
    roomflags
    align
    generation
    debug
    loadroom <val>
    hunger|thirst|drunk <val>|off
    str|con|int|wis|dex|cha <val>
"))


(defcommand (ch "tester" "advance" level-str) (:tester)
  (let ((level (and (every #'digit-char-p level-str)
                    (parse-integer level-str))))
    (cond
      ((null level)
       (send-to-char ch "That's not a level!~%"))
      ((>= level +lvl-ambassador+)
       (send-to-char ch "I don't think so!~%"))
      (t
       (send-to-char ch "Your body vibrates for a moment... You feel different!~%")
       (when (< level (level-of ch))
         (do-start ch nil)
         (setf (level-of ch) level))

       (gain-exp-regardless ch
                            (- (aref +exp-scale+ level) (exp-of ch)))
       (loop
          for i from 1 upto (1- +max-skills+)
          do (setf (aref (skills-of ch) i) (if (able-to-learn ch i)
                                               (learned ch)
                                               0)))

       (setf (hitp-of ch) (max-hitp-of ch))
       (setf (mana-of ch) (max-mana-of ch))
       (setf (move-of ch) (max-move-of ch))

       (save-player-to-xml ch)))))

(defcommand (ch "tester" "unaffect") (:tester)
  (perform-unaffect ch ch))

(defcommand (ch "tester" "reroll") (:tester)
  (perform-reroll ch ch))

(defcommand (ch "tester" "stat") (:tester)
  (send-to-char ch "What do you want to stat?~%"))

(defcommand (ch "tester" "stat" thing) (:tester)
  (perform-stat ch thing))

(defcommand (ch "tester" "goto") (:tester)
  (send-to-char ch "Where do you want to go today?~%"))

(defcommand (ch "tester" "goto" target) (:tester)
  (let ((destination (find-target-room ch target)))
    (when destination
      (perform-goto ch destination t))))

(defcommand (ch "tester" "restore") (:tester)
  (restore-creature ch))

(defmacro define-tester-setter (name accessor min max)
  `(progn
     (defcommand (ch "tester" ,name) (:tester)
       (send-to-char ch ,(format nil "Set ~a to what?~~%" name)))
     (defcommand (ch "tester" ,name val) (:tester)
       (setf (,accessor ch)
             (pin (parse-integer val :junk-allowed t) ,min ,max))
       (send-to-char ch ,(format nil "~a set to ~~d~~%"
                                 (string-capitalize name))
                     (,accessor ch)))))

(define-tester-setter "alignment" alignment-of -1000 1000)
(define-tester-setter "generation" remort-gen-of 0 10)
(define-tester-setter "maxhit" max-hitp-of 1 nil)
(define-tester-setter "maxmana" max-mana-of 1 nil)
(define-tester-setter "maxmove" max-move-of 1 nil)
(define-tester-setter "str" real-str-of 3 25)
(define-tester-setter "int" real-int-of 3 25)
(define-tester-setter "wis" real-wis-of 3 25)
(define-tester-setter "dex" real-dex-of 3 25)
(define-tester-setter "con" real-con-of 3 25)
(define-tester-setter "cha" real-cha-of 3 25)

(defcommand (ch "tester" "class" class-name) (:tester)
  (let ((number (if (every #'digit-char-p class-name)
                    (parse-integer class-name)
                    (position class-name +class-names+ :test 'string-abbrev))))
    (cond
      ((null number)
       (send-to-char ch "'~a' is an invalid character class.~%" class-name))
      ((not (<= 0 number (1- (length +class-names+))))
       (send-to-char ch "~a is out of range for character classes.~%" number))
      (t
       (setf (char-class-of ch) number)
       (send-to-char ch "Class set to ~(~a~).~%" (elt +class-names+ number))))))

(defcommand (ch "tester" "remort" class-name) (:tester)
  (let ((number (if (every #'digit-char-p class-name)
                    (parse-integer class-name)
                    (position class-name +class-names+ :test 'string-abbrev))))
    (cond
      ((null number)
       (send-to-char ch "'~a' is an invalid character class.~%" class-name))
      ((not (<= 0 number (1- (length +class-names+))))
       (send-to-char ch "~a is out of range for character classes.~%" number))
      (t
       (setf (remort-char-class-of ch) number)
       (send-to-char ch "Remort class set to ~(~a~).~%" (elt +class-names+ number))))))

(defcommand (ch "tester" bad-subcmd) (:tester)
  (send-to-char ch "Invalid tester command: ~a~%" bad-subcmd))

(defcommand (ch "severtell") (:immortal)
  (send-to-char ch "Sever reply of what player?~%"))

(defcommand (ch "severtell" target-str) (:immortal)
  (let* ((targets (resolve-alias ch target-str *characters*))
         (target (first targets)))
    (cond
      ((null targets)
       (send-to-char ch "Nobody of that name here.~%"))
      ((rest targets)
       (send-to-char ch "You can only sever the replies of one person at a time.~%"))
      ((and (/= (idnum-of ch) (last-tell-from-of target))
            (/= (idnum-of ch) (last-tell-to-of target)))
       (send-to-char ch "You aren't on that person's reply buffer, pal.~%"))
      (t
       (setf (last-tell-from-of target) nil)
       (setf (last-tell-to-of target) nil)
       (send-to-char ch "Reply severed.~%")))))

(defcommand (ch "jet_stream") (:immortal)
  (let ((msg (format nil "~a has toggled jet_stream_state ~:[ON~;OFF~]"
                     (name-of ch) *jet-stream-state*)))
    (send-to-char ch "~a~%" msg)
    (slog "~a" msg))
  (setf *jet-stream-state* (not *jet-stream-state*)))