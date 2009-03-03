(in-package :tempus)

(defclass dynamic-text-file ()
  ((filename :accessor filename-of :initarg :filename :initform "")
   (history :accessor history-of :initarg :history :initform (make-array 10))
   (perms :accessor perms-of :initarg :perms :initform (make-array 5))
   (level :accessor level-of :initarg :level :initform 0)
   (lock-idnum :accessor lock-idnum-of :initarg :lock-idnum :initform nil)
   (buffer :accessor buffer-of :initarg :buffer :initform nil)
   (tmp-buffer :accessor tmp-buffer-of :initarg :tmp-buffer :initform nil)))

(defmethod print-object ((dyntext dynamic-text-file) stream)
  (print-unreadable-object (dyntext stream :type t)
    (format stream "~s" (filename-of dyntext))))

(defvar *dyntext-files* nil)

(defun boot-dynamic-text ()
  (setf *dyntext-files* nil)
  (dolist (path (directory (tempus-path "lib/text/dyn/control/*.dyn")))
    (let ((new-dyn-file (make-instance 'dynamic-text-file)))
      (with-open-file (inf path :direction :input :element-type '(unsigned-byte 8))
        ;; get the filename
        (let ((buf (make-array 1024 :element-type '(unsigned-byte 8))))
          (read-sequence buf inf)
          (setf (filename-of new-dyn-file)
                (babel:octets-to-string buf :encoding :latin-1
                                        :end (position 0 buf))))
        ;; pull the edit history
        (dotimes (idx 10)
          (let ((buf (make-array 8 :element-type '(unsigned-byte 8))))
            (read-sequence buf inf)
            (setf (aref (history-of new-dyn-file) idx)
                  (list (logior (ash (aref buf 3) 24)
                                (ash (aref buf 2) 16)
                                (ash (aref buf 1) 8)
                                (aref buf 0))
                        (unix-to-timestamp
                         (logior (ash (aref buf 7) 24)
                                 (ash (aref buf 6) 16)
                                 (ash (aref buf 5) 8)
                                 (aref buf 4)))))))

        ;; pull the special access perms
        (dotimes (idx 5)
          (let ((buf (make-array 4 :element-type '(unsigned-byte 8))))
            (read-sequence buf inf)
            (setf (aref (perms-of new-dyn-file) idx)
                  (logior (ash (aref buf 3) 24)
                          (ash (aref buf 2) 16)
                          (ash (aref buf 1) 8)
                          (aref buf 0)))))

        ;; pull the minimum level
        (let ((buf (make-array 4 :element-type '(unsigned-byte 8))))
          (read-sequence buf inf)
          (setf (level-of new-dyn-file)
                (logior (ash (aref buf 3) 24)
                        (ash (aref buf 2) 16)
                        (ash (aref buf 1) 8)
                        (aref buf 0)))))

      ;; Now read the actual text file in
      (setf (buffer-of new-dyn-file)
            (snarf-file (tempus-path (format nil "lib/text/~a"
                                             (filename-of new-dyn-file)))))

      (push new-dyn-file *dyntext-files*)

      (slog "dyntext BOOTED ~a" (pathname-name path)))))

(defun create-dyntext-backup (dyntext)
  (ensure-directories-exist (tempus-path "lib/text/dyn/text_backup/"))
  (let ((num (loop
                for existing-bak in (directory
                                     (tempus-path "lib/text/dyn/text_backup/~a.*"
                                                  (filename-of dyntext)))
                maximize (parse-integer (pathname-type existing-bak)))))
    (with-open-file (ouf (tempus-path "lib/text/dyn/text_backup/~a.~d"
                                      (filename-of dyntext)
                                      (1+ num))
                         :direction :output
                         :if-does-not-exist :create)
      (write-string (buffer-of dyntext) ouf))))

(defun save-dyntext-buffer (dyntext)
  (with-open-file (ouf (tempus-path "lib/text/~a" (filename-of dyntext))
                       :direction :output
                       :if-exists :rename-and-delete
                       :if-does-not-exist :create)
    (write-string (buffer-of dyntext) ouf)))

(defun reload-dyntext-buffer (dyntext)
  (setf (buffer-of dyntext)
        (snarf-file (tempus-path "lib/text/~a" (filename-of dyntext)))))

(defun save-dyntext-control (dyntext)
  (with-open-file (ouf (tempus-path "lib/text/dyn/control/~a.dyn" (filename-of dyntext))
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :rename-and-delete
                       :if-does-not-exist :create)
    (let ((buf (babel:string-to-octets (filename-of dyntext))))
      (adjust-array buf 1024 :initial-element 0)
      (write-sequence buf ouf))

    (dotimes (idx (length (history-of dyntext)))
      (let* ((idnum (first (aref (history-of dyntext) idx)))
             (time (timestamp-to-unix (second (aref (history-of dyntext) idx))))
             (buf (make-array 8 :element-type '(unsigned-byte 8)
                              :initial-contents
                              (list
                               (ldb (byte 24 8) idnum)
                                   (ldb (byte 8 16) idnum)
                                   (ldb (byte 8 8) idnum)
                                   (ldb (byte 8 0) idnum)
                                   (ldb (byte 8 24) time)
                                   (ldb (byte 8 16) time)
                                   (ldb (byte 8 8) time)
                                   (ldb (byte 8 0) time)))))
        (write-sequence buf ouf)))

    (dotimes (idx (length (perms-of dyntext)))
      (let* ((perm (aref (perms-of dyntext) idx))
             (buf (make-array 4 :element-type '(unsigned-byte 8)
                             :initial-contents
                             (list (ldb (byte 8 24) perm)
                                   (ldb (byte 8 16) perm)
                                   (ldb (byte 8 8) perm)
                                   (ldb (byte 8 0) perm)))))
        (write-sequence buf ouf)))


      (let ((buf (make-array 4 :element-type '(unsigned-byte 8)
                             :initial-contents
                             (list (ldb (byte 8 24) (level-of dyntext))
                                   (ldb (byte 8 16) (level-of dyntext))
                                   (ldb (byte 8 8) (level-of dyntext))
                                   (ldb (byte 8 0) (level-of dyntext))))))
        (write-sequence buf ouf))

      (dotimes (i 28)
        (write-byte 0 ouf))))

(defun push-update-to-history (idnum dyntext)
  (loop for idx from (1- (length (history-of dyntext))) downto 1 do
       (setf (aref (history-of dyntext) idx)
             (aref (history-of dyntext) (1- idx))))
  (setf (aref (history-of dyntext) 0) (list idnum (now))))

(defun show-dynedit-options (ch)
  (send-to-char ch "~
show      [ <filename> [old|new|perms|last] ]
set       <filename> <level> <arg>
add       <filename> <idnum>
remove    <filename> <idnum>
edit      <filename>
abort     <filename>
update    <filename>
prepend   <filename> (prepends the old text to the new)
append    <filename> (appends the old text to the new)
reload    <filename>
"))

(defun show-dyntext (ch dyntext)
    (with-pagination ((link-of ch))
      (let* ((colors "nrgybmcw")
             (color1 (random-elt colors))
             (color2 (random-elt colors))
             (color3 (random-elt colors)))
        (send-to-char ch "   &~c=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
   &~c::::::::  :::::::::  ::::::::::::  :::::::::  :::   :::  ::::::::
     :::    :::        :::  ::  :::  :::    ::  :::   :::  :::
    :::    :::::::    :::  ::  :::  :::::::::  :::   :::  ::::::::      &~c~:(~a~)
   &~c:::    :::        :::  ::  :::  :::        :::   :::       :::
  :::    :::::::::  :::  ::  :::  :::        :::::::::  ::::::::
 &~c=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-&n
"
                      color1 color2 color3 (filename-of dyntext)
                      color2 color1))
      (send-to-char ch "~a was last updated on ~a~%"
                    (filename-of dyntext)
                    (format-timestring nil (second (aref (history-of dyntext) 0))
                                       :format +rfc-1123-format+))

      (send-to-char ch "~a" (buffer-of dyntext))))

(defun check-dyntext-updates (ch reconnectingp)
  (dolist (dyntext *dyntext-files*)
    (when (and (or (string/= (filename-of dyntext) "inews")
                   (immortal-level-p ch))
               (string/= (filename-of dyntext) "fate" :end1 4)
               (string/= (filename-of dyntext) "arenalist")
               (timestamp> (second (aref (history-of dyntext) 0))
                           (login-time-of ch)))
      (if reconnectingp
          (send-to-char ch "&y [ The ~a file was updated while you were disconnected ]&n~%"
                        (filename-of dyntext))
          (send-to-char ch "&y [ The ~a file has been updated since your last visit ]&n~%"
                        (filename-of dyntext))))))

(defcommand (ch "dynedit") (:immortal)
  (show-dynedit-options ch))

(defcommand (ch "dynedit" "set" filename "level" level-str) (:immortal)
  (let ((dyntext (find filename *dyntext-files* :test #'string= :key 'filename-of))
        (level (parse-integer level-str :junk-allowed t)))
    (cond
      ((null dyntext)
       (send-to-char ch "Sorry, couldn't find that dynamic text.~%"))
      ((null level)
       (send-to-char ch "That's not a valid level.~%"))
      (t
       (setf (level-of dyntext) level)
       (save-dyntext-control dyntext)
       (send-to-char ch "Level of ~a dyntext set to ~d.~%" filename level)))))

(defcommand (ch "dynedit" "show") (:immortal)
  (send-to-char ch "DYNTEXT LIST:~%~{   ~a~%~}"
                (sort (mapcar 'filename-of *dyntext-files*) #'string<)))

(defcommand (ch "dynedit" "show" filename) (:immortal)
  (let ((dyntext (find filename *dyntext-files* :test #'string= :key 'filename-of)))
    (cond
      ((null dyntext)
       (send-to-char ch "Sorry, couldn't find the text '~a'.~%" filename))
      (t
       (let ((lock-desc (if (lock-idnum-of dyntext)
                            (format nil "~a (~d)"
                                    (retrieve-player-name (lock-idnum-of dyntext))
                                    (lock-idnum-of dyntext))
                            "NONE")))
         (send-to-char ch "~
DYNTEXT: filename: '~a'
             last: ~a (~d) @ ~a
            level: ~d
             lock: ~a
              old: ~:[NO ~;YES~] (Len: ~d)
              new: ~:[NO ~;YES~] (Len: ~d)
"
                       (filename-of dyntext)
                       (retrieve-player-name (first (aref (history-of dyntext) 0)))
                       (first (aref (history-of dyntext) 0))
                       (format-timestring nil (second (aref (history-of dyntext) 0)))
                       (level-of dyntext)
                       lock-desc
                       (buffer-of dyntext)
                       (if (buffer-of dyntext) (length (buffer-of dyntext)) 0)
                       (tmp-buffer-of dyntext)
                       (if (tmp-buffer-of dyntext) (length (tmp-buffer-of dyntext)) 0)))))))

(defcommand (ch "dynedit" "show" filename "old") (:immortal)
  (let ((dyntext (find filename *dyntext-files* :test #'string= :key 'filename-of)))
    (cond
      ((null dyntext)
       (send-to-char ch "Sorry, couldn't find that dynamic text.~%"))
      (t
       (if (buffer-of dyntext)
           (with-pagination ((link-of ch))
             (send-to-char ch "~a" (buffer-of dyntext)))
           (send-to-char ch "There is no old text buffer.~%"))))))

(defcommand (ch "dynedit" "show" filename "new") (:immortal)
  (let ((dyntext (find filename *dyntext-files* :test #'string= :key 'filename-of)))
    (cond
      ((null dyntext)
       (send-to-char ch "Sorry, couldn't find that dynamic text.~%"))
      (t
       (if (tmp-buffer-of dyntext)
           (with-pagination ((link-of ch))
             (send-to-char ch "~a" (tmp-buffer-of dyntext)))
           (send-to-char ch "There is no new text buffer.~%"))))))

(defcommand (ch "dynedit" "show" filename "perms") (:immortal)
  (let ((dyntext (find filename *dyntext-files* :test #'string= :key 'filename-of)))
    (cond
      ((null dyntext)
       (send-to-char ch "Sorry, couldn't find that dynamic text.~%"))
      (t
       (send-to-char ch "Permissions defined:~%")
       (dotimes (idx (length (perms-of dyntext)))
         (send-to-char ch "~3d.] (~5d) ~a~%"
                       idx
                       (aref (perms-of dyntext) idx)
                       (retrieve-player-name (aref (perms-of dyntext) idx))))))))

(defcommand (ch "dynedit" "show" filename "last") (:immortal)
  (let ((dyntext (find filename *dyntext-files* :test #'string= :key 'filename-of)))
    (cond
      ((null dyntext)
       (send-to-char ch "Sorry, couldn't find that dynamic text.~%"))
      (t
       (send-to-char ch "Last edits:~%")
       (dotimes (idx (length (history-of dyntext)))
         (send-to-char ch "~3d.] (~5d) ~a @ ~a~%"
                       idx
                       (first (aref (history-of dyntext) idx))
                       (retrieve-player-name (first (aref (perms-of dyntext) idx)))
                       (second (aref (history-of dyntext) idx))))))))

(defcommand (ch "dynedit" "add") (:immortal)
  (send-to-char ch "Dynedit add requires a filename.~%"))

(defcommand (ch "dynedit" "add" filename) (:immortal)
  (send-to-char ch "Who do you want to add to the permissions of ~a?~%" filename))

(defcommand (ch "dynedit" "add" filename name) (:immortal)
  (let* ((dyntext (find filename *dyntext-files* :test #'string= :key 'filename-of))
         (idnum (retrieve-player-idnum name))
         (idx (position-if #'zerop (perms-of dyntext))))
    (cond
      ((null dyntext)
       (send-to-char ch "Sorry, couldn't find that dynamic text.~%"))
      ((null idnum)
       (send-to-char ch "Sorry, couldn't find that player.~%"))
      ((null idx)
       (send-to-char ch "The permission list is full.~%"))
      (t
       (setf (aref (perms-of dyntext) idx) idnum)
       (save-dyntext-control dyntext)
       (send-to-char ch "User added to the permission list.~%")))))

(defcommand (ch "dynedit" "remove") (:immortal)
  (send-to-char ch "Dynedit remove requires a filename.~%"))

(defcommand (ch "dynedit" "remove" filename) (:immortal)
  (send-to-char ch "Who do you want to remove from the permissions of ~a?~%" filename))

(defcommand (ch "dynedit" "remove" filename name) (:immortal)
  (let* ((dyntext (find filename *dyntext-files* :test #'string= :key 'filename-of))
         (idnum (retrieve-player-idnum name))
         (idx (when idnum (position idnum (perms-of dyntext)))))
    (cond
      ((null dyntext)
       (send-to-char ch "Sorry, couldn't find that dynamic text.~%"))
      ((null idnum)
       (send-to-char ch "Sorry, couldn't find that player.~%"))
      ((null idx)
       (send-to-char ch "That player isn't in the permission list.~%"))
      (t
       (setf (aref (perms-of dyntext) idx) 0)
       (save-dyntext-control dyntext)
       (send-to-char ch "User removed from the permission list.~%")))))

(defcommand (ch "dynedit" "edit") (:immortal)
  (send-to-char ch "Dynedit edit requires a filename.~%"))

(defun dynedit-edit-ok (ch dyntext)
  (or (= (idnum-of ch) 1)
      (< (level-of dyntext) (level-of ch))
      (find (idnum-of ch) (perms-of dyntext))))

(defcommand (ch "dynedit" "edit" filename) (:immortal)
  (let* ((dyntext (find filename *dyntext-files* :test #'string= :key 'filename-of)))
    (cond
      ((null dyntext)
       (send-to-char ch "Sorry, couldn't find that dynamic text.~%"))
      ((not (dynedit-edit-ok ch dyntext))
       (send-to-char ch "You cannot edit this file.~%"))
      ((and (lock-idnum-of dyntext)
            (/= (lock-idnum-of dyntext) (idnum-of ch)))
       (send-to-char ch "That file is already locked by ~a.~%"
                     (retrieve-player-name (lock-idnum-of dyntext))))
      (t
       (setf (lock-idnum-of dyntext) (idnum-of ch))
       (start-text-editor (link-of ch) dyntext "a dynamic text file"
                          (tmp-buffer-of dyntext)
                          (lambda (cxn dyntext buf)
                            (setf (tmp-buffer-of dyntext) buf)
                            (setf (state-of cxn) 'playing))
                          (lambda (cxn)
                            (setf (state-of cxn) 'playing)))))))

(defcommand (ch "dynedit" "abort") (:immortal)
  (send-to-char ch "Dynedit abort requires a filename.~%"))

(defcommand (ch "dynedit" "abort" filename) (:immortal)
  (let* ((dyntext (find filename *dyntext-files* :test #'string= :key 'filename-of)))
    (cond
      ((null dyntext)
       (send-to-char ch "Sorry, couldn't find that dynamic text.~%"))
      ((not (dynedit-edit-ok ch dyntext))
       (send-to-char ch "You cannot edit this file.~%"))
      ((and (lock-idnum-of dyntext)
            (/= (lock-idnum-of dyntext) (idnum-of ch)))
       (send-to-char ch "That file is already locked by ~a.~%"
                     (retrieve-player-name (lock-idnum-of dyntext))))
      (t
       (setf (lock-idnum-of dyntext) nil)
       (setf (tmp-buffer-of dyntext) nil)
       (send-to-char ch "Buffer edit aborted.~%")))))

(defcommand (ch "dynedit" "update") (:immortal)
  (send-to-char ch "Dynedit update requires a filename.~%"))

(defcommand (ch "dynedit" "update" filename) (:immortal)
  (let* ((dyntext (find filename *dyntext-files* :test #'string= :key 'filename-of)))
    (cond
      ((null dyntext)
       (send-to-char ch "Sorry, couldn't find that dynamic text.~%"))
      ((not (dynedit-edit-ok ch dyntext))
       (send-to-char ch "You cannot edit this file.~%"))
      ((and (lock-idnum-of dyntext)
            (/= (lock-idnum-of dyntext) (idnum-of ch)))
       (send-to-char ch "That file is already locked by ~a.~%"
                     (retrieve-player-name (lock-idnum-of dyntext))))
      ((null (tmp-buffer-of dyntext))
       (send-to-char ch "There is no need to update this file.~%"))
      (t
       (create-dyntext-backup dyntext)
       (setf (lock-idnum-of dyntext) nil)
       (setf (buffer-of dyntext)
             (format nil
                     "~%-- ~:(~a~) UPDATE (~a) -----------------------------------~%~%~a"
                     (filename-of dyntext)
                     (format-timestring nil (now)
                                        :format '(:year #\-
                                                  (:month 2 #\0) #\-
                                                  (:day 2 #\0)))
                     (tmp-buffer-of dyntext)))
       (setf (tmp-buffer-of dyntext) nil)
       (save-dyntext-buffer dyntext)
       (push-update-to-history (idnum-of ch) dyntext)
       (save-dyntext-control dyntext)
       (send-to-char ch "Updated and saved successfully.~%")))))

(defcommand (ch "dynedit" "prepend") (:immortal)
  (send-to-char ch "Dynedit prepend requires a filename.~%"))

(defcommand (ch "dynedit" "prepend" filename) (:immortal)
  (let* ((dyntext (find filename *dyntext-files* :test #'string= :key 'filename-of)))
    (cond
      ((null dyntext)
       (send-to-char ch "Sorry, couldn't find that dynamic text.~%"))
      ((not (dynedit-edit-ok ch dyntext))
       (send-to-char ch "You cannot edit this file.~%"))
      ((and (lock-idnum-of dyntext)
            (/= (lock-idnum-of dyntext) (idnum-of ch)))
       (send-to-char ch "That file is already locked by ~a.~%"
                     (retrieve-player-name (lock-idnum-of dyntext))))
      ((null (buffer-of dyntext))
       (send-to-char ch "There is nothing in the old buffer to prepend.~%"))
      (t
       (setf (tmp-buffer-of dyntext)
             (concatenate 'string (buffer-of dyntext)
                          (tmp-buffer-of dyntext)))
       (send-to-char ch "Old buffer prepended to new buffer.~%")))))

(defcommand (ch "dynedit" "append") (:immortal)
  (send-to-char ch "Dynedit append requires a filename.~%"))

(defcommand (ch "dynedit" "append" filename) (:immortal)
  (let* ((dyntext (find filename *dyntext-files* :test #'string= :key 'filename-of)))
    (cond
      ((null dyntext)
       (send-to-char ch "Sorry, couldn't find that dynamic text.~%"))
      ((not (dynedit-edit-ok ch dyntext))
       (send-to-char ch "You cannot edit this file.~%"))
      ((and (lock-idnum-of dyntext)
            (/= (lock-idnum-of dyntext) (idnum-of ch)))
       (send-to-char ch "That file is already locked by ~a.~%"
                     (retrieve-player-name (lock-idnum-of dyntext))))
      ((null (buffer-of dyntext))
       (send-to-char ch "There is nothing in the old buffer to append.~%"))
      (t
       (setf (tmp-buffer-of dyntext)
             (concatenate 'string (tmp-buffer-of dyntext)
                          (buffer-of dyntext)))
       (send-to-char ch "Old buffer appended to new buffer.~%")))))

(defcommand (ch "dynedit" "reload") (:immortal)
  (send-to-char ch "Dynedit reload requires a filename.~%"))

(defcommand (ch "dynedit" "reload" filename) (:immortal)
  (let* ((dyntext (find filename *dyntext-files* :test #'string= :key 'filename-of)))
    (cond
      ((null dyntext)
       (send-to-char ch "Sorry, couldn't find that dynamic text.~%"))
      ((not (dynedit-edit-ok ch dyntext))
       (send-to-char ch "You cannot edit this file.~%"))
      ((and (lock-idnum-of dyntext)
            (/= (lock-idnum-of dyntext) (idnum-of ch)))
       (send-to-char ch "That file is already locked by ~a.~%"
                     (retrieve-player-name (lock-idnum-of dyntext))))
      (t
       (reload-dyntext-buffer dyntext)
       (send-to-char ch "Buffer reloaded.~%")))))

(defcommand (ch "news") ()
  (show-dyntext ch (find "news" *dyntext-files* :test #'string= :key 'filename-of)))

(defcommand (ch "inews") (:immortal)
  (show-dyntext ch (find "inews" *dyntext-files* :test #'string= :key 'filename-of)))