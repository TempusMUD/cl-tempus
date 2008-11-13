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
          (char-from-room ch))
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
                       (can-see-creature tch ch))
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
       (char-from-room tch)
       (char-to-room tch (in-room-of ch) nil)
       (act ch :target tch
            :subject-emit "$N arrives from a puff of smoke."
            :target-emit "$n has transported you!"
            :not-target-emit "$N arrives from a puff of smoke.")
       (dolist (fch (followers-of tch))
         (when (and (eql was-in (in-room-of fch))
                    (immortal-level-p fch)
                    (not (plr-flagged ch (logior +plr-olc+ +plr-writing+ +plr-mailing+)))
                    (can-see-creature fch ch))
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
                                       (is-name name (aliases-of proto)))
                                     namelist)
                         do
                           (incf count)
                           (format str "~3d. &g[&n~5d&g] &~c~a&n~%"
                                   count
                                   vnum
                                   name-color
                                   (name-of proto))))))
    count))

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
      (send-to-char ch "Sun [~(~a~)] Sky: [~(~a~)] Moon: [~a (~d)] Pres: [~3d] Chng: [~3d]~%"
                    (sunlight-of weather)
                    (sky-of weather)
                    (aref +moon-sky-types+ (moonlight-of weather))
                    (moonlight-of weather)
                    (pressure-of weather)
                    (change-of weather)))
    (send-to-char ch "Flags: &g~a ~a&n~%"
                  (printbits (flags-of zone) +zone-flags+)
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
                               (printbits (flags-of trail) +trail-flags+)))))
      (send-to-char ch "No trails exist within this room.~%")))

(defun find-spec-name (spec)
  "TODO: Actually find the special"
  (if spec
      "Exists"
      "None"))

(defun clan-house-can-enter (ch room)
  (declare (ignore ch room))
  t)

(defun format-search-data (ch room search)
  (send-to-char ch "&rCommand triggers:&n ~a, &rkeywords:&n ~a~%"
                (if (string/= "" (command-keys-of search))
                    (command-keys-of search) "None.")
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
                   (name-of (aref (aref (arg-of search) 2) *spell-info*))))
    ((= (command-of search) +search-com-damage+)
     (send-to-char ch "DAMAGE     ~5d        ~5d        ~5d (~a)~%"
                   (aref (arg-of search) 0)
                   (aref (arg-of search) 1)
                   (aref (arg-of search) 2)
                   (name-of (aref (aref (arg-of search) 2) *spell-info*))))
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
                  (printbits (flags-of room) +room-bits+))
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
                     :when (can-see-creature ch c)
                     :collect (format nil "~a(~:[PC~;MOB~])"
                                      (name-of c) (is-npc c))))
    (send-to-char ch "Contents: &g~{~a~^, ~}&n~%"
                  (loop
                     :for o :in (contents-of (in-room-of ch))
                     :when (can-see-object ch o)
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
                      (printbits (exit-info-of (abs-exit room dir)) +exit-bits+))
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
             (is-name "letter" (aliases-of obj)))
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
                  (printbits (wear-flags-of obj) +wear-bits-desc+))
    (unless (every #'zerop (bitvector-of obj))
      (send-to-char ch "Set char bits : ~@{~a~^ ~}~%"
                    (printbits (aref (bitvector-of obj) 0) +affected-bits+)
                    (printbits (aref (bitvector-of obj) 1) +affected2-bits+)
                    (printbits (aref (bitvector-of obj) 2) +affected3-bits+)))
    (send-to-char ch "Extra flags : ~a~%"
                  (printbits (extra-flags-of obj) +extra-bits+))
    (send-to-char ch "Extra2 flags: ~a~%"
                  (printbits (extra2-flags-of obj) +extra2-bits+))
    (send-to-char ch "Extra3 flags: ~a~%"
                  (printbits (extra3-flags-of obj) +extra3-bits+))
    (send-to-char ch "Weight: ~d, Cost: ~d, Rent: ~d, Timer: ~d~%"
                  (weight-of obj)
                  (cost-of (shared-of obj))
                  (cost-per-day-of (shared-of obj))
                  (timer-of obj))
    (let ((room (where-obj obj)))
      (send-to-char ch "Absolute location: ~a (~d)~%"
                    (name-of room)
                    (number-of room)))
    (send-to-char ch "In room: &c~d&n, In obj: &g~a&n, Carry: &g~a&n, Worn: &g~a&n, Aux: &g~a&n~%"
                  (when (in-room-of obj) (number-of (in-room-of obj)))
                  (when (in-obj-of obj) (name-of (in-obj-of obj)))
                  (when (carried-by-of obj) (name-of (carried-by-of obj)))
                  (when (worn-by-of obj) (name-of (worn-by-of obj)))
                  (when (aux-obj-of obj) (name-of (aux-obj-of obj))))
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
    (when (eql (proto-of (shared-of obj)) obj)
      (send-to-char ch "Spec_param:~%~a~%" (func-param-of obj)))
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
      (send-to-char ch "Contents:~%~a"
                    (if (contains-of obj)
                        (with-output-to-string (str)
                          (list-obj-to-char str (contains-of obj) ch :content t))
                        "None"))
      (unless (zerop (soilage-of obj))
        (send-to-char ch "Soilage: ~a~%"
                      (printbits (soilage-of obj) +soilage-bits+)))

      (unless (zerop (sigil-idnum-of obj))
        (send-to-char ch "Warding Sigil: ~a (~d), level ~d~%"
                      (retrieve-player-name (sigil-idnum-of obj))
                      (sigil-idnum-of obj)
                      (sigil-level-of obj)))
      ;; Stat tmp object affects here
      )))

(defcommand (ch "stat" "room") (:immortal)
  (send-stats-to-char ch (in-room-of ch)))

(defcommand (ch "stat" "room" spec) (:immortal)
  (let ((room (find-target-room ch spec)))
    (when room
     (send-stats-to-char ch room))))

(defcommand (ch "stat" thing) (:immortal)
  (let ((objs (get-matching-objects ch thing (append
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
  (let* ((tchs (get-matching-objects ch name (mapcar 'actor-of *cxns*)))
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
      (char-from-room ch)
      (char-to-room ch target-room)
      (interpret-command ch command)
      ;; only return the char if they're still there
      (when (eql target-room (in-room-of ch))
        (char-from-room ch)
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
       (char-from-room tch)
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

(defcommand (ch "force") (:wizard)
  (send-to-char ch "You want to force who what?~%"))

(defcommand (ch "force" target) (:wizard)
  (declare (ignore target))
  (send-to-char ch "What do you want to force them to do?~%"))

(defcommand (ch "force" target "to" command) (:wizard)
  (let ((victs (get-matching-objects ch target (people-of (in-room-of ch)))))
    (cond
      ((null victs)
       (send-to-char ch "You don't see any '~a' here.~%" target))
      (t
       (send-to-char ch "You got it.~%")
       (dolist (vict victs)
         (tempus::interpret-command vict command))))))
