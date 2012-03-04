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
       (sb-thread:make-thread (lambda ()
                                (slog "Running game on port ~d." port)
                                (init-game port))
                              :name "TEMPUS"))))
  (values))

(defun init-game (port)
  (setf *random-state* (make-random-state t))
  (boot-db)

  (slog "Testing internal integrity")
  ;; FIXME: write these (if applicable)
  #+nil (tmp-string-test)
  #+nil (verify-tempus-integrity)

  (slog "Opening mother connection.")
  (setf *event-base* (make-instance 'event-base))
  (cxn-listen port 'tempus-cxn)

  (slog "Signal trapping.")

  (slog "Entering game loop.")
  (unwind-protect
       (game-loop)
    (progn
      (slog "Closing all sockets.")
      (close-all-cxns)
      (postmodern:disconnect-toplevel)))

  (when *circle-reboot*
    (slog "Rebooting.")
    (sb-ext:quit :unix-status 52))

  (slog "Normal termination of game."))

(defun game-loop ()
  "The main loop of the program."

  (sb-sys:ignore-interrupt sb-unix:sigpipe)

  (add-timer *event-base* (lambda () (weather-and-time)) 60)
  (add-timer *event-base* (lambda () (update-creatures)) 1)
  (add-timer *event-base* (lambda () (mobile-activity)) 4)
  (add-timer *event-base* (lambda () (perform-violence)) 7/10)
  (add-timer *event-base* (lambda () (mobile-specs)) 2)
  (add-timer *event-base* (lambda () (update-rooms 'update-room-flows)) 1)
  (add-timer *event-base* (lambda () (update-rooms 'update-room-affects)) 5)
  (add-timer *event-base* (lambda () (update-rooms 'update-room-ambience)) 4)
  (when *production-mode*
    (add-timer *event-base* (lambda () (update-housing)) 60))

  (event-dispatch *event-base*))

(defun send-to-room (room fmt &rest args)
  (dolist (ch (people-of room))
    (send-to-char ch "~?" fmt args)))

(defun send-to-zone (zone fmt &rest args)
  (dolist (cxn *cxns*)
    (when (and (typep cxn 'tempus-cxn)
               (eql (state-of cxn) 'playing)
               (actor-of cxn)
               (eql (zone-of (in-room-of (actor-of cxn))) zone)
               (awakep (actor-of cxn))
               (not (plr-flagged (actor-of cxn) +plr-olc+))
               (not (plr-flagged (actor-of cxn) +plr-writing+))
               (not (plr-flagged (actor-of cxn) +plr-mailing+)))
      (send-to-char (actor-of cxn) "~?" fmt args))))

(defun send-to-outside (zone fmt &rest args)
  (dolist (cxn *cxns*)
    (when (and (typep cxn 'tempus-cxn)
               (eql (state-of cxn) 'playing)
               (actor-of cxn)
               (eql (zone-of (in-room-of (actor-of cxn))) zone)
               (awakep (actor-of cxn))
               (not (room-flagged (in-room-of (actor-of cxn)) +room-indoors+))
               (not (plr-flagged (actor-of cxn) +plr-olc+))
               (not (plr-flagged (actor-of cxn) +plr-writing+))
               (not (plr-flagged (actor-of cxn) +plr-mailing+)))
      (send-to-char (actor-of cxn) "~?" fmt args))))

(defun send-to-clerics (align fmt &rest args)
  (dolist (cxn *cxns*)
    (when (and (typep cxn 'tempus-cxn)
               (eql (state-of cxn) 'playing)
               (actor-of cxn)
               (or (not (eql align :evil)) (is-evil (actor-of cxn)))
               (or (not (eql align :good)) (is-good (actor-of cxn)))
               (is-cleric (actor-of cxn))
               (awakep (actor-of cxn))
               (not (plr-flagged (actor-of cxn) +plr-olc+))
               (not (plr-flagged (actor-of cxn) +plr-writing+))
               (not (plr-flagged (actor-of cxn) +plr-mailing+)))
      (send-to-char (actor-of cxn) "~?" fmt args))))

(defun verify-environment ()
  (let ((+player-subdirs+ '("character" "equipment" "housing" "mail" "corpses")))
    (dolist (subdir +player-subdirs+)
      (dotimes (idx 10)
        (let ((path (format nil "players/~a/~d/" subdir idx)))
          (ensure-directories-exist path))))))
