(in-package #:tempus)

(defclass editor ()
  ((target :accessor target-of :initarg :target)
   (target-desc :accessor target-desc-of :initarg :target-desc)
   (old-buffer :accessor old-buffer-of :initarg :old-buffer)
   (buffer :accessor buffer-of :initarg :buffer :initform '())
   (state :accessor state-of :initform 'active)))

(defclass text-editor (editor)
  ((finalizer :accessor finalizer-of :initarg :finalizer)
   (cancel-func :accessor cancel-func-of :initarg :cancel-func)))

(defclass mail-editor (editor)
  ((recipients :accessor recipients-of :initarg :recipients :initform nil)
   (attachments :accessor attachments-of :initarg :attachments :initform nil)))

(defclass board-editor (editor)
  ((idnum :accessor idnum-of :initarg :idnum)
   (board-name :accessor board-name-of :initarg :board-name)
   (subject :accessor subject-of :initarg :subject)))

(defclass poll-editor (editor)
  ((header :accessor header-of :initarg :header)))

(defclass file-editor (editor)
  ((path :accessor path-of :initarg :path)
   (loadedp :accessor loadedp :initarg :loadedp)))

(defun send-line-number (cxn num)
  "Send the given line number to the actor."
    (cxn-write cxn "&n~3d&b] &n" num))

(defun send-editor-header (cxn)
  (cxn-write cxn "&C     *&Y TUNED &B] ~
                        &nTerminate with @ on a new line. ~
                        &&H for help&C                 *&n~%     &C0")
  (dotimes (idx 7)
    (cxn-write cxn "&B---------&C~d" (1+ idx)))
  (cxn-write cxn "&n~%"))

(defmethod refresh-screen ((editor editor) cxn &optional (start-line 0) count)
  "Sends the contents of the buffer to the screen"
  (loop for line in (nthcdr start-line (buffer-of editor))
       as line-num from 1
       while (or (null count) (<= line-num (+ start-line count)))
       do
       (send-line-number cxn line-num)
       (cxn-write cxn "~a~%" line)))

(defmethod editor-command ((editor editor) cxn cmd arg)
  "Handles all the commands that the editor is capable of."
  (case cmd
    (#\h
     (cxn-write cxn"~
&C     *&B------------------------ &YH E L P &B------------------------&C*
            &YS - &nSubstitute            &YF - &nFind
            &YE - &nSave && Exit           &YQ - &nQuit (Cancel)
            &YL - &nReplace Line          &YD - &nDelete Line
            &YI - &nInsert Line           &YR - &nRefresh Screen
            &YC - &nClear Buffer          &YU - &nUndo Changes
&C     *&B---------------------------------------------------------&C*
"))
    (#\c
     (setf (buffer-of editor) nil))
    ((#\l #\i #\d)
     (with-words arg (param &rest new-line)
       (cond
         ((or (null param) (notevery #'digit-char-p param))
          (cxn-write cxn "You must specify a numeric line number.~%"))
         (t
          (let ((line-num (parse-integer param)))
            (cond
              ((> line-num (length (buffer-of editor)))
               (cxn-write cxn
                          "There are only ~d lines of text!~%"
                          (length (buffer-of editor))))
              (t
               (case cmd
                 (#\l
                  (setf (nth (1- line-num) (buffer-of editor))    new-line)
                  (cxn-write cxn "Line ~d replaced.~%" line-num))
                 (#\i
                  (setf (buffer-of editor)
                        (nconc (subseq (buffer-of editor) 0 (1- line-num))
                               (list new-line)
                               (subseq (buffer-of editor) (1- line-num))))
                  (cxn-write cxn
                             "New line inserted before line ~d.~%"
                             line-num))
                 (#\d
                  (setf (buffer-of editor)
                        (nconc (subseq (buffer-of editor) 0 (1- line-num))
                               (subseq (buffer-of editor) line-num)))
                  (cxn-write cxn "Line ~d deleted.~%" line-num))))))))))
    (#\r
     (refresh-screen editor cxn))
    (#\u
     (setf (buffer-of editor) (split-sequence #\newline (old-buffer-of editor)))
     (refresh-screen editor cxn)
     (cxn-write cxn "Reverted back to previous.~%"))
    (#\e
     (setf (state-of editor) 'finishing))
    (#\q
     (setf (state-of editor) 'aborting))
    (#\s
     (let* ((matching-delims "()[]<>{}")
            (start-delim (find #\space arg :test-not #'char=))
            (end-delim-pos (position start-delim matching-delims))
            (end-delim (if end-delim-pos
                           (char matching-delims (1+ end-delim-pos))
                           start-delim))
            (start1 (position start-delim arg))
            (end1 (and start1 (position end-delim arg :start (1+ start1))))
            (start2 (and end1 (position start-delim arg :start end1)))
            (end2 (and start2 (position end-delim arg :start (1+ start2)))))
       (cond
         ((= (1+ start1) end1)
          (cxn-write cxn "You can't replace a zero-length search pattern.~%"))
         ((and start1 end1 start2 end2)
          (let ((search-str (subseq arg (1+ start1) end1))
                (replace-str (subseq arg (1+ start2) end2)))
            (setf (buffer-of editor)
                  (loop for old-line in (buffer-of editor)
                     collect (string-replace search-str old-line replace-str)))
            (cxn-write cxn
                       "All instances of [~a] have been replaced with [~a].~%"
                       search-str
                       replace-str)))
         (t
          (cxn-write cxn "The subsitution could not be made.~%")))))
    (#\f
     (loop for line in (buffer-of editor)
        as linenum from 1
        when (search arg line)
        do
          (send-line-number cxn linenum)
          (cxn-write cxn "~a~%" line))
     (cxn-write cxn "~%"))
    (t
     (cxn-write cxn "No such editor command.~%"))))

(defmethod editor-prompt ((editor editor) cxn)
  (cxn-write cxn "&n~3d&B] &n"
             (1+ (length (buffer-of editor)))))

(defmethod editor-start ((editor editor) cxn)
  (send-editor-header cxn)
  (refresh-screen editor cxn))

(defmethod editor-finish ((editor text-editor) cxn buf)
  (funcall (finalizer-of editor) cxn (target-of editor) buf))

(defmethod editor-abort ((editor text-editor) cxn)
  (funcall (cancel-func-of editor) cxn (target-of editor)))

(defmethod editor-input ((editor editor) cxn line)
  (let ((editor (mode-data-of cxn)))
    (cond
      ((zerop (length line))
       (setf (buffer-of editor) (nconc (buffer-of editor) (list line))))
      ((eql (char line 0) #\&)
       (editor-command editor cxn
                       (char-downcase (char line 1))
                       (string-trim '(#\space) (subseq line 2))))
      ((eql (char line 0) #\@)
       (setf (state-of editor) 'finished))
      ((eql (char line 0) #\.)
       (refresh-screen editor cxn))
      (t
       (setf (buffer-of editor) (nconc (buffer-of editor) (list line)))))

    (case (state-of editor)
      (finished
       (editor-finish editor cxn (format nil "~{~a~%~}" (buffer-of editor))))
      (aborting
       (editor-abort editor cxn)))))

(defparameter +mail-obj-vnum+ 1204)

(defmethod editor-start ((editor mail-editor) cxn)
  (send-editor-header cxn)
  (cxn-write cxn "     &yTo&b:&c ~{~a~^, ~}&n~%~%"
             (mapcar 'retrieve-player-name (recipients-of editor))))

(defmethod editor-command ((editor mail-editor) cxn cmd arg)
  (case cmd
    (#\h
     (cxn-write cxn"~
&C     *&B------------------------ &YH E L P &B------------------------&C*
            &YS - &nSubstitute            &YF - &nFind
            &YE - &nSave && Exit          &YQ - &nQuit (Cancel)
            &YL - &nReplace Line          &YD - &nDelete Line
            &YI - &nInsert Line           &YR - &nRefresh Screen
            &YC - &nClear Buffer          &YU - &nUndo Changes
            &YA - &nAdd Recipient         &YZ - &nZap Recipient
            &YT - &nList Recipients
&C     *&B---------------------------------------------------------&C*
"))
    (#\a
     (let* ((names (split-sequence #\space arg :remove-empty-subseqs t))
            (idnums (mapcar 'retrieve-player-idnum names)))
       (if (null names)
           (cxn-write cxn "You were going to add some recipients?~%")
           (loop
              for name in names
              for idnum in idnums do
                (cond
                  ((null idnum)
                   (cxn-write cxn "Player '~a' was not found.~%" name))
                  (t
                   (cxn-write cxn "Added ~a to recipients.~%"
                              (retrieve-player-name idnum))
                   (setf (recipients-of editor) (remove-duplicates
                                                 (append (recipients-of editor)
                                                         (list idnum))))))))))
    (#\z
     (let* ((names (split-sequence #\space arg :remove-empty-subseqs t))
            (idnums (mapcar 'retrieve-player-idnum names)))
       (if (null names)
           (cxn-write cxn "You were going to remove some recipients?~%")
           (loop
              for name in names
              for idnum in idnums do
                (cond
                  ((null idnum)
                   (cxn-write cxn "Player '~a' was not found.~%" name))
                  ((not (member idnum (recipients-of editor)))
                   (cxn-write cxn "~a wasn't a recipient.~%"
                              (retrieve-player-name idnum)))
                  (t
                   (cxn-write cxn "Removed ~a from recipients.~%"
                              (retrieve-player-name idnum))
                   (setf (recipients-of editor) (delete idnum (recipients-of editor)))))))))
    (#\t
     (cxn-write cxn "     &yTo&b:&c ~{~a~^, ~}&n~%" (mapcar 'retrieve-player-name (recipients-of editor))))
    (t
     (call-next-method))))

(defmethod editor-finish ((editor mail-editor) cxn buf)
  (cond
    ((zerop (length buf))
     (cxn-write cxn "Why would you send a blank message?~%"))
    (t
     (let ((mail (read-object +mail-obj-vnum+)))
       (setf (creation-method-of mail) :unknown)
       (setf (action-desc-of mail)
             (format nil " * * * *  Tempus Mail System  * * * *~%Date: ~a~%  To: ~{~a~^, ~}~%From: ~a~%~%~a"
                     (format-timestring nil (now)
                                        :format '(:short-month #\space
                                                  (:day 2 #\space) #\space
                                                  :hour #\:
                                                  (:min 2) #\:
                                                  (:sec 2) #\space
                                                  :year))
                     (mapcar 'retrieve-player-name (recipients-of editor))
                     (name-of (actor-of cxn))
                     buf))

       (when (and (actor-of cxn)
                  (immortal-level-p (actor-of cxn)))
         (act (actor-of cxn)
              :place-emit "$n postmarks and dispatches $s mail."))

       (cxn-write cxn "Message sent!~%")

       (dolist (recipient (recipients-of editor))
         (let* ((mail-path (mail-pathname recipient))
                (old-mail (ignore-errors
                            (when (probe-file mail-path)
                              (with-open-file (inf mail-path)
                                (cddr (xmls:parse inf)))))))
           (with-open-file (ouf mail-path :direction :output
                                :if-exists :rename-and-delete
                                :if-does-not-exist :create)
             (write-string
              (xmls:toxml `("objects" nil ,@old-mail ,(serialize-object mail)))
              ouf)))

         (let ((target (gethash recipient *character-map*)))
           (when target
             (send-to-char target "A strange voice in your head says, 'You have new mail.'~%")))))))

  (setf (state-of cxn) 'playing))

(defun start-text-editor (cxn target target-desc old-buffer finalizer cancel-func)
  (let ((split-buffer (butlast (split-sequence #\newline old-buffer))))
    (setf (mode-data-of cxn) (make-instance 'text-editor
                                            :target target
                                            :target-desc target-desc
                                            :old-buffer old-buffer
                                            :buffer split-buffer
                                            :finalizer finalizer
                                            :cancel-func cancel-func))
    (setf (state-of cxn) 'editing)))

(defun start-mail-editor (cxn recipients)
  (setf (mode-data-of cxn) (make-instance 'mail-editor
                                          :recipients recipients))
  (setf (state-of cxn) 'editing))
