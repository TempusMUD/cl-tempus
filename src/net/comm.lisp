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
(defparameter *scheck* nil)                 ; for syntax checking mode
(defparameter *log-cmds* nil)             ; log cmds
(defparameter *shutdown-count* -1)        ; shutdown countdown
(defparameter *shutdown-idnum* -1)        ; idnum of person calling shutdown
(defparameter *shutdown-mode* 'none)  ; what type of shutdown
(defparameter *suppress-output* nil)
(defparameter *avail-descs* 0)          ; available descriptors

(defparameter *last-cmd* (make-array +num-save-cmds+));

(defun main (argc argv)
  (let ((port *default-port*)
        (dir *default-dir*)
        (pos 1))
    (tmp-string-init)
    (acc-string-init)
    (loop while (and (< pos argc) (char= (char (elt argv pos) 1) #\-)) do
          (case (char (elt argv pos) 2)
            (#\b
             (setf *restrict* 50)
             (slog "Wizlock 50"))
            (#\d
             (cond
               ((> (length (elt argv pos)) 3)
                (setf dir (subseq (elt argv pos) 3)))
               ((< (incf pos) argc)
                (setf dir (elt argv pos)))
               (t
                (slog "Directory arg expected after option -d.")
                (return-from main))))
            (#\m
             (setf *mini-mud* t)
             (setf *no-rent-check* t)
             (slog "Running in minimized mode & with no rent check and olc lock."))
            (#\c
             (setf *scheck* t)
             (slog "Syntax check mode enabled."))
            (#\q
             (setf *no-rent-check* t)
             (slog "Quick boot mode -- rent check suppressed."))
            (#\r
             (setf *restrict* t)
             (slog "Restricting game -- no new players allowed."))
            (#\s
             (setf *no-specials* t)
             (slog "Suppressing assignment of special routines."))
            (#\o
             (setf *olc-lock* t)
             (slog "Locking olc."))
            (#\z
             (setf *no-initial-zreset* t)
             (slog "Bypassing initial zone resets."))
            (#\n
             (setf *nameserver-is-slow* t)
             (slog "Disabling nameserver."))
            (#\l
             (setf *log-cmds* t)
             (slog "Enabling log_cmds."))
            (#\p
             (setf *production-mode* t)
             (slog "Running in production mode"))
            (t
             (errlog "Unknown option -~c in argument string." (char (elt argv pos) 2)))))

    (when (< pos argc)
      (cond
        ((not (digit-char-p (char (elt argv pos) 1)))
         (format *error-output*
                 "Usage: ~a [-c] [-m] [-q] [-r] [-s] [-d pathname] [port #]~%"
                 (elt argv 0))
         (return-from main))
        ((<= (setf port (parse-integer (elt argv pos))) 1024)
         (format *error-output* "Illegal port number ~d.~%" (elt argv 0))
         (return-from main))))

    (let ((*default-pathname-defaults* (merge-pathnames dir)))
      (slog "Using ~a as data directory."
            (truename *default-pathname-defaults*))

      (verify-environment)

      (cond
        (*scheck*
         (boot-world)
         (slog "Done.")
         (return-from main 0))
        (t
         (slog "Running game on port ~d." port)
         (init-game port))))
    0))

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
  (slog "Entering game loop")
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

(defun verify-environment ()
  (let ((+player-subdirs+ '("character" "equipment" "housing" "mail" "corpses")))
    (dolist (subdir +player-subdirs+)
      (dotimes (idx 10)
        (let ((path (format nil "players/~a/~d/" subdir idx)))
          (ensure-directories-exist path))))))
