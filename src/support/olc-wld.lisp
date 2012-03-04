(in-package #:tempus)

(defparameter +rset-params+
  '(("name" :type string :slot name :desc "room name")
    ("title" :type string :slot name :desc "room title")
    ("description" :type text :slot description :desc "room description")
    ("sector" :type enumerated :slot terrain :desc "room sector type" :table +sector-types+)
    ("flags" :type bitflag :slot flags :desc "room" :table +room-flags+)
    ("sound" :type text :slot sounds :desc "room sound")
    ("flow" :func perform-rset-flow)
    ("occupancy" :type number :slot max-occupancy :desc "room occupancy" :min 1 :max 256)
    ("special" :func perform-rset-special :desc "room special")
    ("specparam" :type text :slot func-param :desc "room specparam")
    ("prog" :type text :slot prog-text :desc "room prog")))

(defun perform-create-room (ch zone room-num)
  (when (check-can-edit ch zone +zone-rooms-approved+)
    (let ((room (make-instance 'room-data
                               :number room-num
                               :name "A Fresh Blank Room"
                               :zone zone)))
      (setf (gethash room-num *rooms*) room)
      (setf (world-of zone) (sort (cons room (world-of zone)) #'< :key 'number-of)))
    (send-to-char ch "Room ~d successfully created.~%" room-num)))

(defun perform-destroy-room (ch room)
  (when (check-can-edit ch (zone-of room) +zone-rooms-approved+)
    (dolist (tch (people-of room))
      (send-to-char tch "The room in which you exist is suddenly removed from reality!~%")
      (char-from-room tch nil)
      (cond
        ((is-npc tch)
         (extract-creature tch 'disconnecting))
        (t
         (char-to-room (real-room 3001) nil)
         (look-at-room tch (in-room-of tch) nil)
         (act tch :place-emit "$n appears from a void in reality.~%"))))
    (loop while (contents-of room) do
         (extract-obj (first (contents-of room))))
    (dolist (clan (hash-values *clans*))
      (remove-clan-room room clan))
    (setf (world-of (zone-of room)) (delete room (world-of (zone-of room))))
    (remhash (number-of room) *rooms*)
    (send-to-char ch "Room eliminated.~%")))

(defun perform-clear-room (ch room)
  (when (check-can-edit ch (zone-of room) +zone-rooms-approved+)
    (with-slots (name description terrain sounds prog-text prog-obj prog-marker prog-state ex-description
                      dir-option searches affects trail flags find-path-index max-occupancy light
                      flow-dir flow-speed flow-kind func func-param)
        room
      (setf name "A Blank Room")
      (setf description nil)
      (setf sounds nil)
      (setf prog-text nil)
      (setf prog-obj nil)
      (setf prog-marker nil)
      (setf prog-state nil)
      (setf ex-description nil)
      (setf searches nil)
      (setf affects nil)
      (setf trail nil)
      (setf flow-dir nil)
      (setf flow-speed nil)
      (setf flow-kind nil)
      (setf func nil)
      (setf func-param nil)
      (setf max-occupancy nil)
      (setf flags 0)
      (setf find-path-index 0)
      (setf light 0))
    (send-to-char ch "Room fully cleared.~%")))

(defun save-room (ouf room)
  (format ouf "#~d~%" (number-of room))
  (format ouf "~a~~~%~a~~~%" (name-of room) (description-of room))
  (format ouf "~d ~a ~d~%"
          (number-of (zone-of room))
          (bits-to-asciiflag (flags-of room))
          (terrain-of room))
  (dotimes (dir +num-dirs+)
    (let ((exit (aref (dir-option-of room) dir)))
      (when exit
        (format ouf "D~d~%~a~~~%~a~~~%~a ~d ~d~%"
                dir
                (description-of exit)
                (keyword-of exit)
                (bits-to-asciiflag (logandc2 (exit-info-of exit)
                                             (logior +ex-closed+ +ex-locked+)))
                (key-of exit)
                (to-room-of exit)))))
  (dolist (exd (ex-description-of room))
    (format ouf "E~%~a~~~%~a~~~%" (keyword-of exd) (description-of exd)))
  (when (prog-text-of room)
    (format ouf "R~%~a~~~%" (prog-text-of room)))
  (dolist (search (searches-of room))
    (format ouf "Z~%~a~~~%~a~~~%~a~~~%~a~~~%~a~~~%~d ~d ~d ~d ~d ~d~%"
            (trigger-of search)
            (keywords-of search)
            (to-vict-of search)
            (to-room-of search)
            (to-remote-of search)
            (command-of search)
            (aref (arg-of search) 0)
            (aref (arg-of search) 1)
            (aref (arg-of search) 2)
            (flags-of search)
            (fail-chance-of search)))
  (when (sounds-of room)
    (format ouf "L~%~a~~~%" (sounds-of room)))
  (when (plusp (flow-speed-of room))
    (format ouf "F~%~d ~d ~d~%"
            (flow-dir-of room)
            (flow-speed-of room)
            (flow-kind-of room)))
  (when (max-occupancy-of room)
    (format ouf "O ~d~%" (max-occupancy-of room)))
  (when (func-param-of room)
    (format ouf "P~%~a~~~%" (func-param-of room)))
  (format ouf "S~%"))

(defun save-zone-rooms (ch zone)
  (let ((context nil))
    (handler-case
        (progn
          (setf context "world index")
          (update-index-file (tempus-path "lib/world/wld/index") (number-of zone) "wld")

          (setf context "world file")
          (with-open-file (ouf (tempus-path "lib/world/wld/~d.wld" (number-of zone))
                               :direction :output
                               :if-exists :rename-and-delete
                               :if-does-not-exist :create)
            (dolist (room (world-of zone))
              (save-room ouf room))
            (format ouf "$~~~%"))

          (slog "OLC: ~a rsaved ~d" (name-of ch) (number-of zone))
          t)
      (error (err)
        (slog "Error while saving ~a: ~a!" context err)
        nil))))

(defun perform-room-mimic (src-room dst-room mode)
  (when (or (= mode 0) (= mode 1))
    (setf (name-of dst-room) (name-of src-room)))
  (when (or (= mode 0) (= mode 2))
    (setf (sounds-of dst-room) (sounds-of src-room)))
  (when (or (= mode 0) (= mode 3))
    (setf (description-of dst-room) (description-of src-room)))
  (when (or (= mode 0) (= mode 4))
    (setf (terrain-of dst-room) (terrain-of src-room)))
  (when (or (= mode 0) (= mode 5))
    (setf (flags-of dst-room) (flags-of src-room)))
  (when (or (= mode 0) (= mode 6))
    (setf (ex-description-of dst-room) (copy-list (ex-description-of src-room)))))

(defun save-room-special-assignments ()
  (with-open-file (ouf (tempus-path "lib/etc/spec_ass_wld")
                       :direction :output
                       :if-exists :rename-and-delete
                       :if-does-not-exist :create)
    (dolist (room (hash-values *rooms*))
      (format ouf "~6d ~20s ## ~a~%"
              (number-of room)
              (find-special-by-func (func-of room))
              (name-of room)))))

(defun perform-set-exit-toroom (ch room dir to-room-num one-way-p)
  (let ((target-room (real-room to-room-num)))
    (when (null target-room)
      (cond
        ((check-can-edit ch (zone-containing-number to-room-num) +zone-rooms-approved+)
         (perform-create-room ch (zone-containing-number to-room-num) to-room-num)
         (send-to-char ch "The destination room has been created.~%"))
        (t
         (send-to-char ch "That destination room does not exist.~%")
         (return-from perform-set-exit-toroom))))

    (when (null (aref (dir-option-of room) dir))
      (setf (aref (dir-option-of room) dir) (make-instance 'room-direction-data)))
    (setf (to-room-of (aref (dir-option-of room) dir)) to-room-num)

    (cond
      (one-way-p
       (send-to-char ch "To room set.~%"))
      ((not (can-edit-zone ch (zone-of target-room) +zone-rooms-approved+))
       (send-to-char ch "To room set.  Unable to create return exit.~%"))
      ((null (aref (dir-option-of target-room) (aref +rev-dir+ dir)))
       (setf (aref (dir-option-of target-room) (aref +rev-dir+ dir))
             (make-instance 'room-direction-data :to-room (number-of room)))
       (send-to-char ch "To room set.  Return exit created from to room.~%"))
      (t
       (setf (to-room-of (aref (dir-option-of target-room) (aref +rev-dir+ dir)))
             (number-of room))
       (send-to-char ch "To room set.  Return exit set in to room.~%")))))

(defun perform-rlist (ch vnum-start vnum-end)
  (with-pagination ((link-of ch))
    (loop
       with index = 1
       for vnum from vnum-start upto vnum-end
       as room = (real-room vnum)
       when room
       do
         (send-to-char ch "~4d. &g[&n~5d&g]&c ~40a &n~:[ (nodesc)~;~]~:[~; (prog)~]~%"
                       index vnum
                       (name-of room)
                       (find-if (complement #'cl-ppcre::whitespacep) (description-of room))
                       (prog-text-of room))
         (incf index))))

(defun perform-rset-flow (ch room input)
  (cond
    ((string= "remove" input)
     (setf (flow-speed-of (in-room-of ch)) 0)
     (setf (flow-kind-of (in-room-of ch)) 0)
     (setf (flow-dir-of (in-room-of ch)) 0)
     (send-to-char ch "Flow removed from room.~%"))
    (t
     (let* ((inputs (ppcre:split "\\s+" input))
            (dir (position (first inputs) +dirs+ :test 'string-abbrev))
            (speed (parse-integer (second inputs) :junk-allowed t))
            (kind (if (every #'digit-char-p (third inputs))
                      (parse-integer (third inputs) :junk-allowed t)
                      (position (third inputs) +flow-types+ :test 'string-abbrev))))
       (cond
         ((null dir)
          (send-to-char ch "Invalid flow direction.~%"))
         ((null speed)
          (send-to-char ch "Invalid flow speed.~%"))
         ((minusp speed)
          (send-to-char ch "Flow speed must be a positive number.~%"))
         ((null kind)
          (send-to-char ch "Invalid flow kind.~%"))
         (t
          (setf (flow-dir-of room) dir)
          (setf (flow-speed-of room) speed)
          (setf (flow-kind-of room) kind)
          (send-to-char ch "Flow state set.~%")))))))

(defun perform-rset-special (ch room special-name)
  (let ((special (find special-name (hash-keys *special-funcs*) :test 'string-abbrev)))
    (cond
      ((null special)
       (send-to-char ch "That is not a valid special.~%"))
      ((not (logtest (gethash special *special-flags*) +spec-rm+))
       (send-to-char ch "This special is not for rooms.~%"))
      ((and (logtest (gethash special *special-flags*) +spec-res+)
            (not (security-is-member ch "OLCWorldWrite")))
       (send-to-char ch "This special is reserved.~%"))
      (t
       (setf (func-of room) (gethash special *special-funcs*))
       (save-room-special-assignments)
       (send-to-char ch "Room special set.~%")))))

(defun create-room-extradesc (ch keywords)
  (cond
    ((find keywords (ex-description-of (in-room-of ch))
           :test #'string-abbrev
           :key 'keyword-of)
     (send-to-char ch "~
An extra description already exists with that keyword.
Use the 'olc rexdesc remove' command to remove it, or the
'olc rexdesc edit' command to change it.
"))
    (t
     (setf (plr-bits-of ch) (logior (plr-bits-of ch) +plr-olc+))
     (act ch :place-emit "$n begins to write an extra description.~%")
     (start-text-editor (link-of ch)
                        (in-room-of ch)
                        "an extradesc"
                        ""
                        (lambda (cxn room buf)
                          (push (make-instance 'extra-descr-data
                                               :keyword keywords
                                               :description buf)
                                (ex-description-of room))
                          (setf (plr-bits-of (actor-of cxn))
                                (logandc2 (plr-bits-of (actor-of cxn)) +plr-olc+))
                          (setf (state-of cxn) 'playing))
                        (lambda (cxn room)
                          (declare (ignore room))
                          (setf (plr-bits-of (actor-of cxn))
                                (logandc2 (plr-bits-of (actor-of cxn)) +plr-olc+))
                          (setf (state-of cxn) 'playing))))))

(defun edit-room-extradesc (ch keywords)
  (let ((exd (find keywords (ex-description-of (in-room-of ch))
                   :test #'string-abbrev
                   :key 'keyword-of)))
    (cond
      ((null exd)
       (send-to-char ch "No such description.~%"))
      (t
       (setf (plr-bits-of ch) (logior (plr-bits-of ch) +plr-olc+))
       (act ch :place-emit "$n begins to write an extra description.~%")
       (start-text-editor (link-of ch)
                          exd
                          "an extradesc"
                          ""
                          (lambda (cxn exd buf)
                            (setf (description-of exd) buf)
                            (setf (plr-bits-of (actor-of cxn))
                                  (logandc2 (plr-bits-of (actor-of cxn)) +plr-olc+))
                            (setf (state-of cxn) 'playing))
                          (lambda (cxn exd)
                            (declare (ignore exd))
                            (setf (plr-bits-of (actor-of cxn))
                                  (logandc2 (plr-bits-of (actor-of cxn)) +plr-olc+))
                            (setf (state-of cxn) 'playing)))))))

(defun add-key-to-room-extradesc (ch keyword more-keywords)
  (let ((exd (find keyword (ex-description-of (in-room-of ch))
                   :test #'string-abbrev
                   :key 'keyword-of)))
    (cond
      ((null exd)
       (send-to-char ch "No such description.~%"))
      (t
       (setf (keyword-of exd) (format nil "~a ~a" (keyword-of exd) more-keywords))
       (send-to-char ch "Keywords added.~%")))))

(defun remove-room-extradesc (ch keywords)
  (let ((exd (find keywords (ex-description-of (in-room-of ch))
                   :test #'string-abbrev
                   :key 'keyword-of)))
    (cond
      ((null exd)
       (send-to-char ch "No such description.~%"))
      (t
       (setf (ex-description-of (in-room-of ch))
             (delete exd (ex-description-of (in-room-of ch))))
       (send-to-char ch "Description removed.~%")))))

(defcommand (ch "olc" "create" "room") (:immortal)
  (send-to-char ch "Create a room with what vnum?~%"))

(defcommand (ch "olc" "create" "room" "next") (:immortal)
  (let* ((zone (zone-of (in-room-of ch)))
         (room-num (loop for num from (* (number-of zone) 100) upto (top-of zone)
                      when (null (real-room num)) do (return num)
                      finally (return nil))))
    (cond
      ((not (check-can-edit ch zone +zone-rooms-approved+))
       nil)
      ((null room-num)
       (send-to-char ch "No allocatable rooms found in zone.~%"))
      (t
       (perform-create-room ch zone room-num)))))

(defcommand (ch "olc" "create" "room" number) (:immortal)
  (let* ((room-num (parse-integer number :junk-allowed t))
         (zone (and room-num (zone-containing-number room-num))))
    (cond
      ((not (check-can-edit ch zone +zone-rooms-approved+))
       nil)
      ((null room-num)
       (send-to-char ch "You must enter a room number or 'next'.~%"))
      ((null zone)
       (send-to-char ch "No zone for the room to be in.~%"))
      ((real-room room-num)
       (send-to-char ch "That room already exists.~%"))
      (t
       (perform-create-room ch zone room-num)))))

(defcommand (ch "olc" "destroy" "room") (:immortal)
  (send-to-char ch "You must enter a room number.~%"))

(defcommand (ch "olc" "destroy" "room" number) (:immortal)
  (let* ((room-num (parse-integer number :junk-allowed t))
         (room (and room-num (real-room room-num))))
    (cond
      ((not (check-can-edit ch (zone-of room) +zone-rooms-approved+))
       nil)
      ((null room-num)
       (send-to-char ch "You must enter an actual number.~%"))
      ((null room)
       (send-to-char ch "That room does not exist.~%"))
      (t
       (perform-destroy-room ch room)))))

(defcommand (ch "olc" "clear" "room") (:immortal)
  (perform-clear-room ch (in-room-of ch)))

(defcommand (ch "olc" "rsave") (:immortal)
  (when (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+)
    (if (save-zone-rooms ch (zone-of (in-room-of ch)))
        (send-to-char ch "You save the rooms for zone #~d (~a).~%"
                      (number-of (zone-of (in-room-of ch)))
                      (name-of (zone-of (in-room-of ch))))
        (send-to-char ch "The rooms for zone #~d (~a) could not be saved.~%"
                      (number-of (zone-of (in-room-of ch)))
                      (name-of (zone-of (in-room-of ch)))))))

(defcommand (ch "olc" "rmimic" room what) (:immortal)
  (when (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+)
    (let* ((room-id (parse-integer room :junk-allowed t))
           (room (real-room room-id))
           (what-num (position what '("all" "title" "sounds" "description" "sector" "flags" "exdesc")
                               :test 'string-abbrev)))
      (cond
        ((or (null room-id) (null room))
         (send-to-char ch "You must specify a room number.~%"))
        ((null what-num)
         (send-to-char ch "Usage: olc rmimic room <number> (all|sounds|description|sector|flags|exdesc|title)~%"))
        (t
         (perform-room-mimic room (in-room-of ch) what-num)
         (send-to-char ch "Okay, done mimicing.~%"))))))

(defcommand (ch "olc" "rset") (:immortal)
  (with-pagination ((link-of ch))
    (send-to-char ch "Valid rset commands:~%~{  &y~a&n~%~}"
                  (remove-duplicates
                   (mapcan (lambda (cmd)
                             (when (and (string= "olc" (first (command-info-pattern cmd)))
                                        (string= "rset" (second (command-info-pattern cmd)))
                                        (stringp (third (command-info-pattern cmd))))
                               (list (third (command-info-pattern cmd)))))
                           *commands*)
                   :test #'string=))))

(defcommand (ch "olc" "rset" param) (:immortal)
  (when (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+)
    (perform-set ch (in-room-of ch) t +rset-params+ param nil)))

(defcommand (ch "olc" "rset" param value) (:immortal)
  (when (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+)
    (perform-set ch (in-room-of ch) t +rset-params+ param value)))

(defcommand (ch "olc" "rset" "sounds" "remove") (:immortal)
  (when (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+)
    (setf (sounds-of (in-room-of ch)) nil)
    (send-to-char ch "Sounds removed from room.~%")))

(defcommand (ch "olc" "exit") (:immortal)
  (send-to-char ch "Usage: olc exit <direction> <parameter> [<values>]~%"))

(defcommand (ch "olc" "exit" direction) (:immortal)
  (declare (ignore direction))
  (send-to-char ch "Options are: description, doorflags, toroom, keynumber, keywords, remove.~%"))

(defcommand (ch "olc" "exit" direction "description") (:immortal)
  (when (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+)
    (let ((dir (position direction +dirs+ :test 'string-abbrev)))
      (cond
        ((null dir)
         (send-to-char ch "That's an invalid direction.~%"))
        (t
         (when (null (aref (dir-option-of (in-room-of ch)) dir))
           (setf (aref (dir-option-of (in-room-of ch)) dir) (make-instance 'room-direction-data)))
         (let ((exit (aref (dir-option-of (in-room-of ch)) dir)))
           (if (description-of exit)
               (act ch :place-emit "$n begins to edit an exit description.")
               (act ch :place-emit "$n begins to create an exit description."))
           (setf (plr-bits-of ch) (logior (plr-bits-of ch) +plr-olc+))
           (start-text-editor (link-of ch)
                              exit
                              "an exit description"
                              (description-of exit)
                              (lambda (cxn target buf)
                                (setf (description-of target) buf)
                                (setf (plr-bits-of (actor-of cxn))
                                      (logandc2 (plr-bits-of (actor-of cxn)) +plr-olc+))
                                (setf (state-of cxn) 'playing))
                              (lambda (cxn target)
                                (declare (ignore target))
                                (setf (plr-bits-of (actor-of cxn))
                                      (logandc2 (plr-bits-of (actor-of cxn)) +plr-olc+))
                                (setf (state-of cxn) 'playing)))))))))

(defcommand (ch "olc" "exit" direction "doorflags" plus-or-minus flag-names) (:immortal)
  (when (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+)
    (let ((dir (position direction +dirs+ :test 'string-abbrev)))
      (cond
        ((null dir)
         (send-to-char ch "You must enter a proper direction for the exit!~%"))
        (t
         (when (null (aref (dir-option-of (in-room-of ch)) dir))
           (setf (aref (dir-option-of (in-room-of ch)) dir) (make-instance 'room-direction-data)))
         (perform-set-flags ch plus-or-minus (ppcre:split "\\s+" flag-names)
                            +exit-flags+ "exit"
                            "olc exit <direction> doorflags +/- <flags>"
                            (lambda ()
                              (exit-info-of (aref (dir-option-of (in-room-of ch)) dir)))
                            (lambda (val)
                              (setf (exit-info-of (aref (dir-option-of (in-room-of ch)) dir)) val))))))))

(defcommand (ch "olc" "exit" direction "toroom" room-num) (:immortal)
  (when (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+)
    (let ((dir (position direction +dirs+ :test 'string-abbrev))
          (to-room-num (parse-integer room-num :junk-allowed t)))
      (cond
        ((null dir)
         (send-to-char ch "That's an invalid direction.~%"))
        ((null to-room-num)
         (send-to-char ch "You must supply a valid room number.~%"))
        (t
         (perform-set-exit-toroom ch (in-room-of ch) dir room-num nil))))))

(defcommand (ch "olc" "exit" direction "toroom" room-num "oneway") (:immortal)
  (when (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+)
    (let ((dir (position direction +dirs+ :test 'string-abbrev))
          (to-room-num (parse-integer room-num :junk-allowed t)))
      (cond
        ((null dir)
         (send-to-char ch "That's an invalid direction.~%"))
        ((null to-room-num)
         (send-to-char ch "You must supply a valid room number.~%"))
        (t
         (perform-set-exit-toroom ch (in-room-of ch) dir room-num t))))))

(defcommand (ch "olc" "exit" direction "keynumber" vnum) (:immortal)
  (when (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+)
    (let ((dir (position direction +dirs+ :test 'string-abbrev))
          (keynum (parse-integer vnum :junk-allowed t)))
      (cond
        ((null dir)
         (send-to-char ch "That's an invalid direction.~%"))
        ((null keynum)
         (send-to-char ch "You must supply a key number.~%"))
        (t
         (when (null (aref (dir-option-of (in-room-of ch)) dir))
           (setf (aref (dir-option-of (in-room-of ch)) dir) (make-instance 'room-direction-data)))
         (setf (key-of (aref (dir-option-of (in-room-of ch)) dir)) keynum)
         (send-to-char ch "Keynumber set.~%"))))))

(defcommand (ch "olc" "exit" direction "keywords") (:immortal)
  (declare (ignore direction))
  (send-to-char ch "What keywords?~%"))

(defcommand (ch "olc" "exit" direction "keywords" keywords) (:immortal)
  (when (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+)
    (let ((dir (position direction +dirs+ :test 'string-abbrev)))
      (cond
        ((null dir)
         (send-to-char ch "That's an invalid direction.~%"))
        (t
         (when (null (aref (dir-option-of (in-room-of ch)) dir))
           (setf (aref (dir-option-of (in-room-of ch)) dir) (make-instance 'room-direction-data)))
         (setf (keyword-of (aref (dir-option-of (in-room-of ch)) dir)) keywords)
         (send-to-char ch "Keywords set.~%"))))))

(defcommand (ch "olc" "exit" direction "remove") (:immortal)
  (when (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+)
    (let ((dir (position direction +dirs+ :test 'string-abbrev)))
      (cond
        ((null dir)
         (send-to-char ch "That's an invalid direction.~%"))
        ((null (aref (dir-option-of (in-room-of ch)) dir))
         (send-to-char ch "No exit there to remove.~%"))
        (t
         (setf (aref (dir-option-of (in-room-of ch)) dir) nil)
         (send-to-char ch "Exit removed.~%"))))))

(defcommand (ch "olc" "rexdesc") (:immortal)
  (send-to-char ch "Usage: olc rexdesc (create|remove|edit|addkey) <keyword>~%"))

(defcommand (ch "olc" "rexdesc" "create") (:immortal)
  (send-to-char ch "Which extra description would you like to create?~%"))

(defcommand (ch "olc" "rexdesc" "create" keywords) (:immortal)
  (when (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+)
    (create-room-extradesc ch keywords)))

(defcommand (ch "olc" "rexdesc" "remove") (:immortal)
  (send-to-char ch "Which extra description would you like to remove?~%"))

(defcommand (ch "olc" "rexdesc" "remove" keywords) (:immortal)
  (when (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+)
    (remove-room-extradesc ch keywords)))

(defcommand (ch "olc" "rexdesc" "edit") (:immortal)
  (send-to-char ch "Which extra description would you like to edit?~%"))

(defcommand (ch "olc" "rexdesc" "edit" keywords) (:immortal)
  (when (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+)
    (edit-room-extradesc ch keywords)))

(defcommand (ch "olc" "rexdesc" "addkey") (:immortal)
  (send-to-char ch "Which extra description would you like to add keys to?~%"))

(defcommand (ch "olc" "rexdesc" "addkey" keyword) (:immortal)
  (declare (ignore keyword))
  (send-to-char ch "What?? How about some keywords to add...~%"))

(defcommand (ch "olc" "rexdesc" "addkey" keyword more-keywords) (:immortal)
  (when (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+)
    (add-key-to-room-extradesc ch keyword more-keywords)))

(defcommand (ch "rlist") (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (perform-rlist ch (* (number-of zone) 100) (top-of zone))))

(defcommand (ch "rlist" junk) (:immortal)
  (declare (ignore junk))
  (send-to-char ch "Usage: rlist [<start vnum> <end vnum>]~%"))

(defcommand (ch "rlist" start end) (:immortal)
  (let ((start-num (parse-integer start :junk-allowed t))
        (end-num (parse-integer end :junk-allowed t)))
    (if (or (null start-num) (null end-num))
        (send-to-char ch "Usage: rlist [<start vnum> <end vnum>]~%")
        (perform-rlist ch (min start-num end-num) (max start-num end-num)))))