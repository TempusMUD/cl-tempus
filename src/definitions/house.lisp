(in-package :tempus)

(defclass house ()
  ((idnum :accessor idnum-of :initarg :idnum)
   (owner-idnum :accessor owner-idnum-of :initarg :owner-idnum :initform 0)
   (kind :accessor kind-of :initarg :kind :initform 0)
   (created-time :accessor created-time-of :initarg :created-time :initform 0)
   (landlord :accessor landlord-of :initarg :landlord :initform 0)
   (rate :accessor rate-of :initarg :rate :initform 0)
   (rent-overflow :accessor rent-overflow-of :initarg :rent-overflow :initform 0)
   (rooms :accessor rooms-of :initform nil)
   (guests :accessor guests-of :initform nil)
   (repossessions :accessor repossessions-of :initform nil)))

(defconstant +invalid+ 0)
(defconstant +private+ 1)
(defconstant +public+ 2)
(defconstant +rental+ 3)
(defconstant +clan+ 4)

(defparameter +max-house-guests+ 50)
(defparameter +max-house-items+ 50)
(defparameter +house-kinds+ #("invalid" "private" "public" "rental" "clan"))
(defvar *houses* nil)

(defun house-pathname (idnum)
  (make-pathname :name (format nil "~4,'0d" idnum)
                 :type "dat"
                 :defaults (tempus-path (format nil "lib/players/housing/~d/"
                                          (mod idnum 10)))))

(defun add-house-room (house room-num)
  (let ((room (real-room room-num)))
    (when room
      (setf (flags-of room) (logior (flags-of room) +room-house+))))
  (pushnew room-num (rooms-of house)))

(defun remove-house-room (house room-num)
  (let ((room (real-room room-num)))
    (when room
      (setf (flags-of room) (logandc2 (flags-of room) +room-house+))))
  (setf (rooms-of house) (delete room-num (guests-of house))))

(defun add-house-guest (house guest-idnum)
  (pushnew guest-idnum (guests-of house)))

(defun remove-house-guest (house guest-idnum)
  (setf (guests-of house) (delete guest-idnum (guests-of house))))

(defun house-guest? (house idnum)
  (member idnum (guests-of house)))

(defun house-owner? (house ch)
  (cond
    ((member (kind-of house) (list +private+ +public+ +rental+))
     (eql (idnum-of ch) (owner-idnum-of house)))
    ((eql (kind-of house) +clan+)
     (let* ((clan (real-clan (owner-idnum-of house))))
       (and clan
            (clan-member-p ch clan)
            (plr-flagged ch +plr-clan-leader+))))
    (t
     nil)))

(defun clear-repo-notes (house)
  (setf (repossessions-of house) nil))

(defun serialize-house (house)
  `("house"
    (("id" ,(idnum-of house))
     ("owner" ,(owner-idnum-of house))
     ("type" ,(aref +house-kinds+ (kind-of house)))
     ("created" ,(created-time-of house))
     ("landlord" ,(landlord-of house))
     ("rate" ,(rate-of house))
     ("rentOverflow" ,(rent-overflow-of house)))
    ,@(loop for room-id in (rooms-of house)
           collect `("room" (("number" ,room-id))
                            ,@(loop for obj in (contents-of (real-room room-id))
                                   collect (serialize-object obj))))

    ,@(loop for guest in (guests-of house)
           collect `("guest" (("id" ,guest))))
    ,@(loop for repo-note in (repossessions-of house)
           collect `("repossession" (("note" ,repo-note))))))

(defun unserialize-house (xml)
  (let ((house (make-instance 'house
                              :idnum (xml-attr xml "id" :numeric t)
                              :owner-idnum (xml-attr xml "owner" :numeric t :default 0)
                              :kind (let ((num (position (xml-attr xml "type")
                                                         +house-kinds+
                                                         :test #'string-equal)))
                                      (or num +invalid+))
                              :created-time (xml-attr xml "created" :numeric t)
                              :landlord (xml-attr xml "landlord" :numeric t :default 0)
                              :rate (xml-attr xml "rate" :numeric t :default 0)
                              :rent-overflow (xml-attr xml "rentOverflow" :numeric t :default 0))))
    (dolist (node (cddr xml))
      (when (consp node)
        (string-case (first node)
          ("room"
           (let* ((room-num (xml-attr node "number" :numeric t))
                  (room (real-room room-num)))
             (add-house-room house room-num)
             (when room
               (dolist (content-node (cddr node))
                 (when (and (consp content-node)
                            (string= (first content-node) "object"))
                 (unserialize-object nil nil room content-node)))
               (extract-unrentables (contents-of room)))))
          ("guest"
           (add-house-guest house (xml-attr node "id" :numeric t)))
          ("repossession"
           (push (xml-attr node "note") (repossessions-of house))))))
    ;; maintain ordering of rooms, guests, and repossessions
    (setf (rooms-of house) (nreverse (rooms-of house)))
    (setf (guests-of house) (nreverse (guests-of house)))
    (setf (repossessions-of house) (nreverse (repossessions-of house)))
    house))

(defun load-house (path)
  (let ((xml (cxml:parse-file path (cxml-xmls:make-xmls-builder))))
    (dolist (node (cddr xml))
      (when (and (consp node)
                 (string= (first node) "house"))
        (return-from load-house (unserialize-house node))))))

(defun housing-load ()
  (slog "Loading player houses")
  (setf *houses* nil)
  (dotimes (i 10)
    (dolist (path (directory (tempus-path "lib/players/housing/~d/*.dat" i)))
      (let ((house (load-house path)))
        (push house *houses*)))))

(defun can-enter-house (ch room-id)
  (let ((house (find-house-by-room room-id)))
    (or (null house)
        (authorized? ch 'enter-houses)
        (and (is-npc ch)
             (aff-flagged ch +aff-charm+)
             (master-of ch)
             (can-enter-house (master-of ch) room-id))
        (and (is-pc ch)
             (or
              (eql (kind-of house) +public+)
              (and (or (eql (kind-of house) +rental+)
                       (eql (kind-of house) +private+))
                   (or (and (account-of ch)
                            (eql (idnum-of (account-of ch))
                                 (owner-idnum-of house)))
                       (house-guest? house (idnum-of ch))))
              (and (eql (kind-of house) +clan+)
                   (let ((clan (real-clan (owner-idnum-of house))))
                     (or (null clan)
                         (clan-member-p ch clan)))))))))

(defun save-house (house)
  (with-open-file (ouf (house-pathname (idnum-of house))
                       :direction :output
                       :if-exists :rename-and-delete)
    (let ((sink (cxml:make-character-stream-sink ouf :canonical nil)))
      (cxml-xmls:map-node sink `("housefile" nil ,(serialize-house house))
                          :include-namespace-uri nil))))

(defun find-house-by-idnum (idnum)
  (find idnum *houses* :key 'idnum-of))

(defun find-house-by-room (room)
  (find-if (lambda (house)
             (find room (rooms-of house)))
           *houses*))

(defun find-house-by-owner (owner-idnum)
  (find-if (lambda (house)
             (and (eql (kind-of house) +private+)
                  (eql owner-idnum (owner-idnum-of house))))
           *houses*))

(defun notify-house-repossession (house ch)
  (when (repossessions-of house)
    (let ((note (read-object +mail-obj-vnum+)))
      (setf (action-desc-of note)
            (format nil "~
The following items were sold at auction to cover your back rent:

~{~a~%~}

Sincerely,
    The management~%"
                    (repossessions-of house)))
      (setf (plrtext-len-of note) (1+ (length (action-desc-of note))))
      (obj-to-char note ch)
      (send-to-char ch "The TempusMUD Landlord gives you a letter detailing your bill.~%")
      (clear-repo-notes house)
      (save-house house))))

(defun create-house (owner first-room last-room)
  (check-type owner integer)
  (check-type first-room integer)
  (check-type last-room integer)
  (let ((new-house (make-instance 'house
                                  :idnum (1+ (loop for house in *houses*
                                                maximize (idnum-of house)))
                                  :owner-idnum owner
                                  :kind +private+
                                  :created-time (get-universal-time)
                                  :landlord 0
                                  :rate 0
                                  :rent-overflow 0)))
    (loop
       for room-num from first-room upto last-room
       as room = (real-room room-num)
       when room do
         (add-house-room new-house room-num))
    (push new-house *houses*)
    (save-house new-house)
    new-house))

(defun destroy-house (house-id)
  (setf *houses* (delete house-id *houses* :key 'idnum-of))
  (ignore-errors (delete-file (house-pathname house-id))))

(defun save-houses ()
  (when *houses*
    (slog "HOUSE: Saving ~d houses." (length *houses*))
    (mapc 'save-house *houses*)))

(defun room-rent-cost (room)
  (let* ((objs (remove-if 'unrentable? (find-all-objects (contents-of room))))
         (obj-count (length objs))
         (rent-sum (reduce #'+ objs :key 'cost-per-day-of)))
    (values
     (if (> obj-count +max-house-items+)
         (* rent-sum (1+ (floor obj-count +max-house-items+)))
         rent-sum)
     obj-count)))

(defun house-rent-cost (house)
  (let ((obj-rent (reduce #'+ (mapcar 'room-rent-cost
                                      (mapcar 'real-room (rooms-of house))))))
    (if (eql (kind-of house) +rental+)
        (+ obj-rent (* (rate-of house) (length (rooms-of house))))
        obj-rent)))

(defun reconcile-clan-collection (house cost)
  (let ((clan (real-clan (owner-idnum-of house))))
    (when clan
      (let ((transferred (alexandria:clamp cost 0 (bank-of clan))))
        (decf (bank-of clan) transferred)
        (postmodern:execute (:update 'clans :set 'bank (bank-of clan) :where (:= 'idnum (idnum-of clan))))
        (- cost transferred)))))

(defun reconcile-private-collection (house cost)
  (let ((account (account-by-idnum (owner-idnum-of house))))
    (when account
      ;; First we get as much gold as we can
      (cond
        ((< cost (past-bank-of account))
         (withdraw-past-bank account cost)
         (setf cost 0))
        (t
         (decf cost (past-bank-of account))
         (withdraw-past-bank account (past-bank-of account))))
      ;; If they didn't have enough, try credits
      (cond
        ((< cost (future-bank-of account))
         (withdraw-future-bank account cost)
         (setf cost 0))
        (t
         (decf cost (future-bank-of account))
         (withdraw-future-bank account (future-bank-of account))))
      cost)))

(defun reconcile-rent-collection (house cost)
  (alexandria:switch ((kind-of house))
    (+public+
     0)
    (+rental+
     (reconcile-private-collection house cost))
    (+private+
     (reconcile-private-collection house cost))
    (+clan+
     (reconcile-clan-collection house cost))
    (t
     0)))

(defun collect-house-rent (house cost)
  (cond
    ((< cost (rent-overflow-of house))
     (slog "HOUSE: [~d] Previous repossessions covering ~d rent." (idnum-of house))
     (decf (rent-overflow-of house)
           cost))
    (t
     (decf cost (rent-overflow-of house))
     (reconcile-rent-collection house cost))))

(defun count-housed-objects (house)
  (loop
     for room-id in (rooms-of house)
     as room = (real-room room-id)
     when room do
       (dolist (obj (find-all-objects (contents-of room)))
         (when (shared-of obj)
           (incf (house-count-of (shared-of obj)))))))

(defun update-housing ()
  (alexandria:maphash-values
   (lambda (obj)
     (setf (house-count-of (shared-of obj)) 0))
   *object-prototypes*)
  (dolist (house *houses*)
    (collect-house-rent house
                        (floor (house-rent-cost house) (* 24 60)))
    (count-housed-objects house)))

(defun list-house-guests (ch house)
  (cond
    ((null (guests-of house))
     (send-to-char ch "No guests defined.~%"))
    (t
     (send-to-char ch "    Guests:~%~a"
                   (print-columns-to-string (term-width-of (account-of ch)) 15
                                            (mapcar 'retrieve-player-name (guests-of house)))))))

(defun house-list-contents (ch obj-list indent)
  (dolist (obj obj-list)
    (send-to-char ch "~vt&g~a&n~50t~6d Au~[~:;    (contains ~:*~d)~]~%"
                  indent
                  (name-of obj)
                  (cost-per-day-of obj)
                  (length (contains-of obj)))
    (house-list-contents ch (contains-of obj) (+ indent 2))))

(defun house-show-room (ch room show-contents?)
  (send-to-char ch "Room~:[~*~; &g[&n~d&g]&n~]: &c~a&n~%"
                (pref-flagged ch +pref-holylight+)
                (number-of room)
                (name-of room))
  (when show-contents?
    (house-list-contents ch (contents-of room) 3)))

(defun house-show-room-brief (ch name cost count)
  (send-to-char ch "&c~30a      ~:[&n~;&r~]~5d&n   ~10d~%"
                name
                (> count +max-house-items+)
                count
                cost))

(defun list-house-rooms (ch house show-contents?)
  (with-pagination ((link-of ch))
    (dolist (room-num (rooms-of house))
      (let ((room (real-room room-num)))
        (house-show-room ch room show-contents?)))))

(defun list-house-rooms-brief (ch rooms)
  (with-pagination ((link-of ch))
    (send-to-char ch "-- House Room --------------------- Items ------- Cost~%")
    (let ((total-cost 0)
          (total-count 0))
      (dolist (room rooms)
        (multiple-value-bind (cost count)
            (room-rent-cost room)
          (house-show-room-brief ch (name-of room) cost count)
          (incf total-cost cost)
          (incf total-count count)))
      (when (rest rooms)
        (send-to-char ch "- Totals -------------------------- ~5d   ~10d~%" total-count total-cost)))))

(defun display-house (ch house)
  (send-to-char ch "&yHouse[&n~4d&y]  Type:&n ~a  &yLandlord: &n~a~%"
                (idnum-of house)
                (aref +house-kinds+ (kind-of house))
                (or (retrieve-player-name (landlord-of house))
                    "none"))
  (alexandria:switch ((kind-of house))
    (+private+
     (let ((account (account-by-idnum (owner-idnum-of house))))
       (send-to-char ch "&yOwner:&n ~a [~d] &c~a&n~%"
                     (name-of account)
                     (idnum-of account)
                     (email-of account))))
    (+rental+
     (let ((account (account-by-idnum (owner-idnum-of house))))
       (send-to-char ch "&yRenter:&n ~a [~d] &c~a&n~%"
                     (name-of account)
                     (idnum-of account)
                     (email-of account))))
    (+clan+
     (let ((clan (real-clan (owner-idnum-of house))))
       (if clan
           (send-to-char ch "&yOwned by clan:&n ~a [~d]~%"
                         (name-of clan)
                         (idnum-of clan))
           (send-to-char ch "&yOwned by clan:&n NONE~%")))))
  (send-to-char ch "&yCreated:&n ~a~%"
                (format-timestring nil
                                   (unix-to-timestamp (created-time-of house))
                                   :format +asctime-format+))
  (list-house-guests ch house)
  (list-house-rooms ch house nil)
  (when (repossessions-of house)
    (send-to-char ch "&cRepossession Notifications:&n~%~{    ~a~%~}"
                  (repossessions-of house))))


(defun display-houses (ch houses)
  (with-pagination ((link-of ch))
    (send-to-char ch "~
ID   Size Owner  Landlord   Type Rooms
---- ---- ------ ---------- ---- ----------------------------------------------
")
    (dolist (house (sort (copy-list houses) #'< :key 'idnum-of))
      (send-to-char ch "~4d ~4d ~6d ~10a ~4a ~{~d~^, ~}~%"
                    (idnum-of house)
                    (length (rooms-of house))
                    (owner-idnum-of house)
                    (or (retrieve-player-name (landlord-of house))
                        "none")
                    (if (<= 1 (kind-of house) 4)
                        (aref #("" "PRIV" " PUB" "RENT" "CLAN") (kind-of house))
                        " ERR")
                    (sort (copy-list (rooms-of house)) #'<)))))

(defun hedit-house (ch)
  (let ((house (find-house-by-room (number-of (in-room-of ch)))))
    (cond
      ((null house)
       (send-to-char ch "You must be in your house to edit it.~%")
       nil)
      ((not (authorized? ch 'edit-house :house house))
       (send-to-char ch "Only the owner can edit the house.~%")
       nil)
      ((eql (kind-of house) +rental+)
       (send-to-char ch "You cannot edit rental houses.~%")
       nil)
      (t
       house))))

(defcommand (ch "hedit") ()
  (let ((house (hedit-house ch)))
    (when house
       (send-to-char ch "~
HEDIT ( house edit ) usage:
hedit title <title>
hedit desc
hedit extradesc <create|edit|addkey> <keywords>
hedit sound
hedit save
hedit show  [brief] [.] ( lists rooms and contents )"))))

(defcommand (ch "hedit" "title") ()
  (let ((house (hedit-house ch)))
    (when house
      (send-to-char ch "You have to set it to something, moran!~%"))))

(defcommand (ch "hedit" "title" title) ()
  (let ((house (hedit-house ch)))
    (when house
      (cond
        ((> (length title) 80)
         (send-to-char ch "That's a bit long, don't you think?~%"))
        (t
         (setf (name-of (in-room-of ch)) title)
         (send-to-char ch "Okay, room title changed.~%"))))))

(defcommand (ch "hedit" "desc") ()
  (let ((house (hedit-house ch)))
    (when house
      (perform-set ch (in-room-of ch) t +rset-params+ "desc" nil))))

(defcommand (ch "hedit" "sound" "remove") ()
  (let ((house (hedit-house ch)))
    (when house
      (act ch :place-emit "The room suddenly seems strangely quiet.")
      (setf (sounds-of (in-room-of ch)) nil))))

(defcommand (ch "hedit" "sound") ()
  (let ((house (hedit-house ch)))
    (when house
      (perform-set ch (in-room-of ch) t +rset-params+ "sound" nil))))

(defcommand (ch "hedit" "extradesc") ()
  (let ((house (hedit-house ch)))
    (when house
      (send-to-char ch "Usage: hedit extradesc <create | remove | edit | addkey> <keyword> [new keywords]~%"))))

(defcommand (ch "hedit" "extradesc" "create") ()
  (let ((house (hedit-house ch)))
    (when house
      (send-to-char ch "Which extradesc do you want to create?~%"))))

(defcommand (ch "hedit" "extradesc" "create" keywords) ()
  (let ((house (hedit-house ch)))
    (when house
      (create-room-extradesc ch keywords))))

(defcommand (ch "hedit" "extradesc" "edit") ()
  (let ((house (hedit-house ch)))
    (when house
      (send-to-char ch "Which extradesc do you want to edit?~%"))))

(defcommand (ch "hedit" "extradesc" "edit" keywords) ()
  (let ((house (hedit-house ch)))
    (when house
      (edit-room-extradesc ch keywords))))

(defcommand (ch "hedit" "extradesc" "addkey") ()
  (let ((house (hedit-house ch)))
    (when house
      (send-to-char ch "To which extradesc do you want to add a key?~%"))))

(defcommand (ch "hedit" "extradesc" "addkey" keyword) ()
  (declare (ignore keyword))
  (let ((house (hedit-house ch)))
    (when house
      (send-to-char ch "What keywords do you wish to add to that extradesc?~%"))))

(defcommand (ch "hedit" "extradesc" "addkey" keyword new-keywords) ()
  (let ((house (hedit-house ch)))
    (when house
      (add-key-to-room-extradesc ch keyword new-keywords))))

(defcommand (ch "hedit" "extradesc" "remove") ()
  (let ((house (hedit-house ch)))
    (when house
      (send-to-char ch "Which extradesc do you want to remove?~%"))))

(defcommand (ch "hedit" "extradesc" "remove" keywords) ()
  (let ((house (hedit-house ch)))
    (when house
      (remove-room-extradesc ch keywords))))

(defcommand (ch "hedit" "save") ()
  (let ((house (hedit-house ch)))
    (when house
      (save-zone-rooms ch (zone-of (in-room-of ch)))
      (send-to-char ch "Your house modifications have been saved.~%")
      (wait-state ch (rl-sec 8)))))

(defcommand (ch "hedit" "show") ()
  (let ((house (hedit-house ch)))
    (when house
      (list-house-rooms ch house t))))

(defcommand (ch "hedit" "show" ".") ()
  (let ((house (hedit-house ch)))
    (when house
      (house-show-room ch (in-room-of ch) t))))

(defcommand (ch "hedit" "show" "brief") ()
  (let ((house (hedit-house ch)))
    (when house
      (list-house-rooms-brief ch (mapcar 'real-room (rooms-of house))))))

(defcommand (ch "hedit" "show" "brief" ".") ()
  (let ((house (hedit-house ch)))
    (when house
      (list-house-rooms-brief ch (list (in-room-of ch))))))

(defcommand (ch "house") ()
  (let ((house (find-house-by-room (number-of (in-room-of ch)))))
    (cond
      ((null house)
       (send-to-char ch "You must be in your house to set guests.~%"))
      ((not (authorized? ch 'edit-house :house house))
       (send-to-char ch "Only the owner can set guests.~%"))
      (t
       (list-house-guests ch house)))))

(defcommand (ch "house" guest-name) ()
  (let ((house (find-house-by-room (number-of (in-room-of ch))))
        (player-id (retrieve-player-idnum guest-name)))
    (cond
      ((null house)
       (send-to-char ch "You must be in your house to set guests.~%"))
      ((not (authorized? ch 'edit-house :house house))
       (send-to-char ch "Only the owner can set guests.~%"))
      ((null player-id)
       (send-to-char ch "No such player.~%"))
      ((eql (retrieve-player-account player-id) (owner-idnum-of house))
       (send-to-char ch "They already own the house.~%"))
      ((house-guest? house player-id)
       (remove-house-guest house player-id)
       (send-to-char ch "Guest removed.~%"))
      ((>= (length (guests-of house))
           +max-house-guests+)
       (send-to-char ch "Sorry, you have the maximum number of guests already.~%"))
      (t
       (add-house-guest house player-id)
       (save-house house)
       (send-to-char ch "Guest added.~%")))))

(defun can-hcontrol? (ch)
  (cond
    ((authorized? ch 'edit-house)
     t)
    (t
     (send-to-char ch "You aren't able to edit houses!~%")
     nil)))

(defcommand (ch "hcontrol") (:immortal)
  (when (can-hcontrol? ch)
     (send-to-char ch "Usage:
  hcontrol find <'owner' | 'guest' | 'landlord'> <name|id>
  hcontrol destroy <house#>
  hcontrol add <house#> <room#>
  hcontrol delete <house#> <room#>
  hcontrol set <house#> <rate|owner|type|landlord> <'value'|public|private|rental>
  hcontrol show [house#]
  hcontrol build <player name|account#> <first room#> <last room#>
  hcontrol save/recount
  hcontrol where
  hcontrol reload [house#] (Use with caution!)
")))

(defcommand (ch "hcontrol" "build") (:immortal)
  (when (can-hcontrol? ch)
    (send-to-char ch "Usage: hcontrol build <player name|account#> <first room#> <last room#>~%")))


(defcommand (ch "hcontrol" "build" junk) (:immortal)
  (declare (ignore junk))
  (when (can-hcontrol? ch)
    (send-to-char ch "Usage: hcontrol build <player name|account#> <first room#> <last room#>~%")))

(defcommand (ch "hcontrol" "build" owner-id first-room last-room) (:immortal)
  (when (can-hcontrol? ch)
    (let ((owner nil))
      (cond
        ((every #'digit-char-p owner-id)
         (let ((id (parse-integer owner-id)))
           (cond
             ((zerop id)
              (send-to-char ch "Warning, creating house with no owner.~%"))
             ((null (account-by-idnum id))
              (send-to-char ch "Invalid account id ~d~%" id)
              (return-from do-hcontrol-build-owner-id-first-room-last-room))
             (t
              (setf owner (account-by-idnum id))))))
        ((not (player-name-exists owner-id))
         (send-to-char ch "Unknown player ~a.~%" owner-id)
         (return-from do-hcontrol-build-owner-id-first-room-last-room))
        (t
         (setf owner (account-by-idnum
                      (retrieve-player-account
                       (retrieve-player-idnum owner-id))))))

      (let ((other-house (find-house-by-owner (idnum-of owner))))
        (when other-house
          (send-to-char ch "Account ~a already owns house ~d.~%"
                        (idnum-of owner)
                        (idnum-of other-house))
          (return-from do-hcontrol-build-owner-id-first-room-last-room)))

      (let ((first-room-id (parse-integer first-room :junk-allowed t))
            (last-room-id (parse-integer last-room :junk-allowed t)))
        (cond
          ((null first-room-id)
           (send-to-char ch "The beginning room must be a number.~%"))
          ((null last-room-id)
           (send-to-char ch "The top room must be a number.~%"))
          ((> first-room-id last-room-id)
           (send-to-char ch "Top room number is less than beginning room number.~%"))
          ((null (real-room first-room-id))
           (send-to-char ch "Beginning room doesn't exist.~%"))
          ((null (real-room last-room-id))
           (send-to-char ch "Ending room doesn't exist.~%"))
          (t
           (let ((new-house (create-house (idnum-of owner) first-room-id last-room-id)))
             (send-to-char ch "House ~d built.  Mazel tov!~%" (idnum-of new-house))
             (setf (landlord-of new-house)
                   (idnum-of ch))
             (save-house new-house)
             (slog "HOUSE: ~a created house ~d for account ~d with first room ~d"
                   (name-of ch)
                   (idnum-of new-house)
                   (idnum-of owner)
                   first-room-id))))))))

(defcommand (ch "hcontrol" "destroy") (:immortal)
  (when (can-hcontrol? ch)
    (send-to-char ch "Usage: hcontrol destroy <house#>~%")))

(defcommand (ch "hcontrol" "destroy" house-id) (:immortal)
  (when (can-hcontrol? ch)
    (let* ((house-idnum (parse-integer house-id :junk-allowed t))
           (house (and house-idnum
                       (find-house-by-idnum house-idnum))))
      (cond
        ((null house-idnum)
         (send-to-char ch "Invalid house id.~%"))
        ((null house)
         (send-to-char ch "House ~d has already been destroyed.~%" house-idnum))
        ((not (authorized? ch 'edit-house :house house))
         (send-to-char ch "You cannot edit that house.~%"))
        (t
         (destroy-house house)
         (send-to-char ch "House ~d destroyed.~%" house-idnum))))))

(defcommand (ch "hcontrol" "add") (:immortal)
  (when (can-hcontrol? ch)
    (send-to-char ch "Usage: hcontrol add <house#> <room#>~%")))

(defcommand (ch "hcontrol" "add" junk) (:immortal)
  (declare (ignore junk))
  (when (can-hcontrol? ch)
    (send-to-char ch "Usage: hcontrol add <house#> <room#>~%")))

(defcommand (ch "hcontrol" "add" house-id room-id) (:immortal)
  (when (can-hcontrol? ch)
    (let* ((house (resolve-house ch house-id))
           (room (resolve-room ch room-id)))
      (anaphora:acond
        ((null house)
         (send-to-char ch "House ~d does not exist.~%" (idnum-of house)))
        ((null room)
         (send-to-char ch "Room ~d does not exist.~%" (number-of room)))
        ((not (authorized? ch 'edit-house :house house))
         (send-to-char ch "You cannot edit that house.~%"))
        ((find-house-by-room (number-of room))
         (send-to-char ch "Room ~d is already part of house ~d.~%" (number-of room)
                       (idnum-of anaphora:it)))
        (t
         (add-house-room house (number-of room))
         (save-house house)
         (send-to-char ch "Room ~d added to house ~d.~%" (number-of room) (idnum-of house)))))))

(defcommand (ch "hcontrol" "delete") (:immortal)
  (when (can-hcontrol? ch)
    (send-to-char ch "Usage: hcontrol delete <house#> <room#>~%")))

(defcommand (ch "hcontrol" "delete" junk) (:immortal)
  (declare (ignore junk))
  (when (can-hcontrol? ch)
    (send-to-char ch "Usage: hcontrol delete <house#> <room#>~%")))

(defcommand (ch "hcontrol" "delete" house-id room-id) (:immortal)
  (when (can-hcontrol? ch)
    (let* ((house (resolve-house ch house-id))
           (room (resolve-room ch room-id))
           (room-house (find-house-by-room (number-of room))))
      (cond
        ((null house)
         (send-to-char ch "House ~a does not exist.~%" house-id))
        ((null room)
         (send-to-char ch "Room ~a does not exist.~%" room-id))
        ((not (authorized? ch 'edit-house :house house))
         (send-to-char ch "You cannot edit that house.~%"))
        ((null room-house)
         (send-to-char ch "Room ~d isn't a room of any house!~%" (number-of room)))
        ((not (eql room-house house))
         (send-to-char ch "Room ~d belongs to house ~d!~%" (number-of room) (idnum-of room-house)))
        ((endp (rest (rooms-of house)))
         (send-to-char ch "Room ~d is the last room of house ~d.  Destroy it instead.~%"))
        (t
         (remove-house-room house (number-of room))
         (save-house house)
         (send-to-char ch "Room ~d added to house ~d.~%" (number-of room) (idnum-of house)))))))

(defcommand (ch "hcontrol" "set") (:immortal)
  (when (can-hcontrol? ch)
    (send-to-char ch "Usage: hcontrol set <house#> <rate|owner|type|landlord> <'value'|public|private|rental>~%")))

(defcommand (ch "hcontrol" "set" junk) (:immortal)
  (declare (ignore junk))
  (when (can-hcontrol? ch)
    (send-to-char ch "Usage: hcontrol set <house#> <rate|owner|type|landlord> <'value'|public|private|rental>~%")))

(defcommand (ch "hcontrol" "set" house-id "owner" owner-id) (:immortal)
  (when (can-hcontrol? ch)
    (let* ((house (resolve-house ch house-id))
           (owner (if (eql (kind-of house) +clan+)
                      (resolve-clan owner-id)
                      (resolve-account owner-id))))
      (cond
        ((null house)
         (send-to-char ch "No such house '~a'.~%" house-id))
        ((null owner)
         (send-to-char ch "No such ~:[account~;clan~] '~a'.~%"
                       (eql (kind-of house) +clan+)
                       owner-id))
        ((not (authorized? ch 'edit-house :house house))
         (send-to-char ch "You cannot edit that house.~%"))
        ((find-house-by-owner (idnum-of owner))
         (send-to-char ch "~:[Account~;Clan~] ~d already owns a house.~%" (idnum-of owner)))
        (t
         (setf (owner-idnum-of house) (idnum-of owner))
         (save-house house)
         (send-to-char ch "Owner set to ~:[account~;clan~] ~d.~%"
                       (eql (kind-of house) +clan+)
                       (idnum-of owner)))))))

(defcommand (ch "hcontrol" "set" house-id "rate" rate) (:immortal)
  (when (can-hcontrol? ch)
    (let ((house (resolve-house ch house-id))
          (amount (parse-integer rate :junk-allowed t)))
      (cond
        ((null house)
         (send-to-char ch "No such house '~a'.~%" house-id))
        ((or (null amount)
             (minusp amount))
         (send-to-char ch "Invalid rate '~a'.~%" rate))
        (t
         (setf (rate-of house) amount)
         (save-house house)
         (send-to-char ch "House ~d rental rate set to ~d/day.~%"
                       (idnum-of house)
                       amount))))))

(defcommand (ch "hcontrol" "set" house-id "type" kind) (:immortal)
  (when (can-hcontrol? ch)
    (let ((house (resolve-house ch house-id))
          (kind-num (position kind +house-kinds+ :test #'string-equal)))
      (cond
        ((null house)
         (send-to-char ch "No such house '~a'.~%" house-id))
        ((or (null kind-num)
             (zerop kind-num))
         (send-to-char ch "Kind '~a' was not public, private, rental, or clan.~%" kind))
        (t
         (setf (kind-of house) kind-num)
         (setf (owner-idnum-of house) 0)
         (save-house house)
         (send-to-char ch "House ~d type set to ~a and owner reset to 0.~%"
                       (idnum-of house)
                       (aref +house-kinds+ kind-num)))))))

(defcommand (ch "hcontrol" "set" house-id "landlord" player-id) (:immortal)
  (when (can-hcontrol? ch)
    (let* ((house (resolve-house ch house-id))
           (landlord (resolve-player-idnum player-id)))
      (cond
        ((null house)
         (send-to-char ch "No such house '~a'.~%" house-id))
        ((null landlord)
         (send-to-char ch "No such player '~a'.~%" player-id))
        ((not (authorized? ch 'edit-house :house house))
         (send-to-char ch "You cannot edit that house.~%"))
        (t
         (setf (landlord-of house) landlord)
         (save-house house)
         (send-to-char ch "Landlord of house ~d set to ~a.~%"
                       (idnum-of house)
                       (retrieve-player-name landlord)))))))

(defcommand (ch "hcontrol" "show") (:immortal)
  (when (can-hcontrol? ch)
    (display-houses ch *houses*)))

(defcommand (ch "hcontrol" "show" house-id) (:immortal)
  (when (can-hcontrol? ch)
    (let* ((house (resolve-house ch house-id)))
      (cond
        ((null house)
         (send-to-char ch "No such house '~a'.~%" house-id))
        ((not (authorized? ch 'edit-house :house house))
         (send-to-char ch "You cannot edit that house.~%"))
        (t
         (display-house ch house))))))

(defcommand (ch "hcontrol" "find") (:immortal)
  (when (can-hcontrol? ch)
    (send-to-char ch "Usage: hcontrol find <'owner' | 'clan' | 'guest' | 'landlord'> <name|id>")))

(defcommand (ch "hcontrol" "find" junk) (:immortal)
  (declare (ignore junk))
  (when (can-hcontrol? ch)
    (send-to-char ch "Usage: hcontrol find <'owner' | 'clan' | 'guest' | 'landlord'> <name|id>")))

(defun hcontrol-find-matcher (ch kind name)
  (alexandria:switch (kind :test #'string-equal)
    ("owner"
     (let ((account (resolve-account name)))
       (cond
         (account
          (lambda (house)
            (and (or (eql (kind-of house) +private+)
                     (eql (kind-of house) +rental+))
                 (= (owner-idnum-of house) (idnum-of account)))))
         (t
          (send-to-char ch "No such account ~a.~%" name)
          nil))))
    ("clan"
     (let ((clan (resolve-clan name)))
       (cond
         (clan
          (let ((idnum (idnum-of clan)))
            (lambda (house)
              (and (eql (kind-of house)
                        +clan+)
                   (= (owner-idnum-of house) idnum)))))
         (t
          (send-to-char ch "No such clan ~a.~%" name)
          nil))))
    ("guest"
     (let ((idnum (resolve-player-idnum name)))
       (cond
         (idnum
          (lambda (house)
            (member idnum (guests-of house))))
         (t
          (send-to-char ch "No such player ~a.~%" name)
          nil))))
    ("landlord"
     (let ((idnum (resolve-player-idnum name)))
       (cond
         (idnum
          (lambda (house)
            (= idnum (landlord-of house))))
         (t
          (send-to-char ch "No such player ~a.~%" name)
          nil))))))

(defcommand (ch "hcontrol" "find" kind name) (:immortal)
  (when (can-hcontrol? ch)
    (let ((matcher (hcontrol-find-matcher ch kind name)))
      (when matcher
        (let ((matches (remove-if-not matcher *houses*)))
          (if matches
              (display-houses ch matches)
              (send-to-char ch "No houses found.~%")))))))

(defcommand (ch "hcontrol" "where") (:immortal)
  (when (can-hcontrol? ch)
    (let ((house (find-house-by-room (number-of (in-room-of ch)))))
      (if house
          (send-to-char ch "You are in house id ~d, owned by ~d.~%"
                        (idnum-of house)
                        (owner-idnum-of house))
          (send-to-char ch "You are not in a house.~%")))))

(defcommand (ch "hcontrol" "reload") (:immortal)
  (when (can-hcontrol? ch)
    (send-to-char ch "Usage: hcontrol reload <house-id>~%")))

(defcommand (ch "hcontrol" "reload" house-id) (:immortal)
  (when (can-hcontrol? ch)
    (let ((house (resolve-house ch house-id)))
      (cond
        ((null house)
         (send-to-char ch "No such house '~a'.~%" house-id))
        (t
         (loop
              for room-id in (rooms-of house)
              as room = (real-room room-id)
              when room do
              (loop while (contents-of room) do
                   (extract-obj (first (contents-of room)))))
         (destroy-house (idnum-of house))
         (push (load-house (house-pathname (idnum-of house))) *houses*)
         (send-to-char ch "Reload complete.  It might even have worked.~%"))))))

(defcommand (ch "hcontrol" "save") (:immortal)
  (when (can-hcontrol? ch)
    (save-houses)
    (send-to-char ch "Saved.~%")))
