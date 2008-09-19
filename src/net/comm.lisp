(in-package :tempus)

(defconstant +num-save-cmds+ 30)

;; local globals
(defparameter *descriptor-list* nil)
(defparameter *buffer-pool* nil)
(defparameter *buf-largecount* 0)         ; # of large buffers which exist
(defparameter *buf-overflows* 0)          ; # of overflows of output
(defparameter *buf-switches* 0)           ; # of switches from small to large buf
(defparameter *circle-shutdown* nil)      ; clean shutdown
(defparameter *circle-reboot* nil)        ; reboot the game after a shutdown
(defparameter *no-specials* nil)          ; Suppress ass. of special routines
(defparameter *avail-descs* 0)            ; max descriptors available
(defparameter *tics* 0)                   ; for extern checkpointing
(defparameter *log-cmds* nil)             ; log cmds
(defparameter *shutdown-count* -1)        ; shutdown countdown
(defparameter *shutdown-idnum* -1)        ; idnum of person calling shutdown
(defparameter *shutdown-mode* 'none)  ; what type of shutdown
(defparameter *suppress-output* nil)
(defparameter *avail-descs* 0)          ; available descriptors

(defparameter *last-cmd* (make-array +num-save-cmds+));

(defun main (&key
             (port *default-port*)
             (dir *default-dir*)
             (no-nameserver t)
             wizlock
             minimud
             check-only
             no-rent
             no-newbies
             no-specials
             no-olc
             no-zresets
             log-all
             production)
  (when wizlock
    (setf *restrict* 50)
    (slog "Wizlocked to level 50"))
  (when minimud
    (setf *mini-mud* t)
    (setf *no-rent-check* t)
    (slog "Running in minimized mode & with no rent check and olc lock."))
  (when check-only
    (slog "Syntax check mode enabled."))
  (when no-rent
    (setf *no-rent-check* t)
    (slog "Quick boot mode -- rent check suppressed."))
  (when no-newbies
    (setf *restrict* t)
    (slog "Restricting game -- no new players allowed."))
  (when no-specials
    (setf *no-specials* t)
    (slog "Suppressing assignment of special routines."))
  (when no-olc
    (setf *olc-lock* t)
    (slog "Locking olc."))
  (when no-zresets
    (setf *no-initial-zreset* t)
    (slog "Bypassing initial zone resets."))
  (when no-nameserver
    (setf *nameserver-is-slow* t)
    (slog "Disabling nameserver."))
  (when log-all
    (setf *log-cmds* t)
    (slog "Enabling log_cmds."))
  (when production
    (setf *production-mode* t)
    (slog "Running in production mode"))

  (let ((*default-pathname-defaults* (merge-pathnames dir)))
    (slog "Using ~a as data directory."
          (truename *default-pathname-defaults*))

    (verify-environment)

    (cond
      (check-only
       (boot-world)
       (slog "Done.")
       (return-from main 0))
      (t
       (slog "Running game on port ~d." port)
       (init-game port))))
  (values))

(defun init-game (port)
  (setf *random-state* (make-random-state t))
  (boot-db)

  (slog "Testing internal integrity")
  ;; FIXME: write these (if applicable)
  #+nil (tmp-string-test)
  #+nil (verify-tempus-integrity)

  (slog "Opening mother connection.")
  (cxn-listen port 'tempus-cxn)

  (slog "Signal trapping.")

  (slog "Entering game loop.")

  (game-loop)

  (slog "Closing all sockets.")
  (close-all-cxns)
  (disconnect-toplevel)

  (when *circle-reboot*
    (slog "Rebooting.")
    (sb-ext:quit :unix-status 52))

  (slog "Normal termination of game."))

(defun game-loop ()
  "The main loop of the program, it iterates here until *shutdown* is non-NIL."
  (setf *shutdown* nil)

  (loop for pulse from 1
        while (not *shutdown*) do
        (when (> pulse 1000)
          (setf pulse 0))
        (sb-sys:ignore-interrupt sb-unix:sigpipe)
        (let ((start-time (get-internal-real-time)))
               (cxn-update-input)
               (cxn-handle-commands)
               (cxn-update-output)
               (force-output)
               (let ((elapsed (/ (- (get-internal-real-time) start-time)
                                 internal-time-units-per-second)))
                 (when (< elapsed 0.1)
                   (sleep (- 0.1 elapsed)))))))

(defun get-first-printed-char (str)
  "Returns the position of the first character that isn't a terminal
control code."
  (loop for idx = 0 then (+ 2 idx)
		while (and (< idx (length str))
				   (eql (char str idx) #\&))
		finally (when (< idx (length str)) (return idx))))

(defun send-to-char (ch fmt &rest args)
  (when (link-of ch)
    (let* ((str (format nil "~?" fmt args))
           (first-char-pos (get-first-printed-char str)))
      (cxn-write (link-of ch) "~a"
                 (if first-char-pos
                     (string-upcase str
                                    :start first-char-pos
                                    :end (1+ first-char-pos))
                     str)))))

(defun verify-environment ()
  (let ((+player-subdirs+ '("character" "equipment" "housing" "mail" "corpses")))
    (dolist (subdir +player-subdirs+)
      (dotimes (idx 10)
        (let ((path (format nil "players/~a/~d/" subdir idx)))
          (ensure-directories-exist path))))))
