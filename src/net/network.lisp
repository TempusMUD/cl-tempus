(in-package #:tempus)

(cl-interpol:enable-interpol-syntax)

(defconstant +cxn-buffer-size+ 64556
  "Size of buffer used for input")
(defvar *production-mode* nil
  "Should be set to T if running in production")
(defvar *cxns* nil
  "List of established active connections")
(defvar *event-base* nil
  "IOLib multiplexer")
(defvar *listener* nil
  "Socket used for listening on a port")

;;;
;;; data cxn class - cxn used to recieve line-delimited data and
;;; transmit in a non-blocking manner
;;;
(defclass data-cxn ()
  ((socket :accessor socket-of :initarg :socket)
   (timer :accessor timer-of :initform nil)
   (peer-addr :accessor peer-addr-of :initarg :peer-addr)
   (connected :accessor connectedp :type symbol :initform t)
   (input-buf :accessor input-buf-of :type (or null simple-string) :initform nil)
   (input-len :accessor input-len-of :type fixnum :initform 0)
   (output-buf :accessor output-buf-of :initform nil)
   (output-tail :accessor output-tail-of :initform nil)
   (commands :accessor commands-of :type list :initform '()))
  (:documentation
   "Class used for maintaining a non-blocking network connection which
   handles line-delimited input."))

(defgeneric cxn-write (cxn fmt &rest args)
  (:documentation "Sends formatted output to a network connection"))
(defgeneric cxn-close (cxn &key abort)
  (:documentation "Closes an active network connection"))
(defgeneric handle-accept (cxn)
  (:documentation "Initializes a new network connection"))
(defgeneric handle-flush (cxn)
  (:documentation "Sends output stored in output queue"))

(defun cxn-fill-input-buf (cxn)
  "Reads input from the cxn and stores into the cxn's input-buf."
  (let* ((buf (make-array +cxn-buffer-size+ :element-type '(unsigned-byte 8))))
    (handler-case
        (progn
          (multiple-value-bind (buf bytes-read)
              (receive-from (socket-of cxn)
                            :buffer buf
                            :start 0
                            :end +cxn-buffer-size+)
            (when (zerop bytes-read)
              (error 'end-of-file))

            (unless (eql (aref buf 0) #xff)
              ;; ignore telnet sequences
              (setf (input-buf-of cxn)
                    (concatenate 'string (input-buf-of cxn)
                                 (babel:octets-to-string buf :end bytes-read)))
              (incf (input-len-of cxn) bytes-read))))
      (socket-connection-reset-error ()
        (cxn-close cxn))
      (end-of-file ()
        (cxn-close cxn)))))

(defun queue-commands (cxn)
  "Finds all the line breaks in the cxn's input buffer and pushes
lines onto the command queue.  The rest is kept in the input buffer."
  (setf (commands-of cxn)
        (nconc (commands-of cxn)
               (loop
                  for end-line = (position #\newline (input-buf-of cxn)
                                           :end (input-len-of cxn))
                  while (and (connectedp cxn) end-line)
                  collect (string-right-trim
                           '(#\space #\return #\newline #\tab)
                           (subseq (input-buf-of cxn) 0 end-line))
                  do
                  (setf (input-buf-of cxn)
                        (subseq (input-buf-of cxn) (1+ end-line)))
                  (decf (input-len-of cxn) (1+ end-line))))))

(defun do-throttled-command (cxn)
  "Performs the next queued command.  If any commands are left to be
performed, schedules a timer to handle the next one."
  (when (commands-of cxn)
    (handle-command cxn)
    (setf (timer-of cxn)
          (add-timer *event-base* (lambda ()
                                    (setf (timer-of cxn) nil)
                                    (do-throttled-command cxn))
                     0.1 :one-shot t))))

(defun make-tempus-read-handler (cxn)
  "Creates a handler function which responds to data on the cxn.
Fills the input buffer, queues any lines, and performs the command
unless there is already an active command queue handler."
  (lambda (fd event exception)
    (declare (ignore fd event exception))
    (cxn-fill-input-buf cxn)
    (when (connectedp cxn)
      (queue-commands cxn)
      ;; Once we finish a single command, if there are multiple
      ;; commands in the queue, we need to set up a timer that will
      ;; wait for the time when it's ok to look for a command again.
      (when (null (timer-of cxn))
        (do-throttled-command cxn)))))

(defun make-tempus-acceptor (cxn-type)
  "Creates a function to handle the acceptance of a new connection."
  (lambda (fd event exception)
    (declare (ignore fd event exception))
    (let ((new-socket (accept-connection *listener* :wait t)))
      (setf (external-format-of new-socket) '(:utf-8 :eol-style :crlf))
      (when new-socket
        (let ((new-cxn (make-instance cxn-type
                                      :socket new-socket
                                      :peer-addr (address-to-string (remote-name new-socket)))))
          (set-io-handler *event-base*
                          (socket-os-fd new-socket)
                          :read
                          (make-tempus-read-handler new-cxn))
          (push new-cxn *cxns*)
          (handle-accept new-cxn))))))

(defun cxn-listen (port cxn-type)
  "Sets *LISTENER* to a socket on the given port"
  (when *listener*
    (close *listener*))
  (setf *listener* (make-socket :connect :passive
                                :address-family :internet
                                :type :stream
                                :ipv6 nil))
  (bind-address *listener* +ipv4-unspecified+ :port port :reuse-address t)
  (listen-on *listener* :backlog 10)
  (set-io-handler *event-base*
                    (socket-os-fd *listener*)
                    :read
                    (make-tempus-acceptor cxn-type)))

(defmethod handle-flush ((cxn data-cxn))
  "Handler for a write-ready event.  Sends elements in the write queue
one by one until it blocks or the write queue is empty.  If the write
queue becomes empty, the write-ready fd handler is removed.  Finishes
closing the socket if it was being shutdown."
  (handler-case
      (loop
         for buf = (pop (output-buf-of cxn))
         while buf
         when (plusp (length buf))
         do (let ((wrote-bytes (send-to (socket-of cxn) buf :dont-wait t)))
              (when (/= (length buf)
                        wrote-bytes)
                (push (subseq buf wrote-bytes)
                      (output-buf-of cxn))
                (return-from handle-flush))))
    (socket-connection-reset-error ()
      (cxn-close cxn))
    (end-of-file ()
      (cxn-close cxn)))
  ;; At this point, we've flushed all output, so remove the output io
  ;; handler
  (remove-fd-handlers *event-base* (socket-os-fd (socket-of cxn)) :write t)
  (setf (output-tail-of cxn) nil)
  ;; If we're disconnecting, go ahead and finish the disconnect
  (unless (connectedp cxn)
    (shutdown (socket-of cxn) :read t :write t)
    (close (socket-of cxn))))

(defun schedule-cxn-output (cxn)
  "Adds a write event IO handler which flushes the output buffer of
the connection."
  (unless (iolib.multiplex::fd-monitored-p *event-base*
                                           (socket-os-fd (socket-of cxn))
                                           :write)
    (set-io-handler *event-base*
                    (socket-os-fd (socket-of cxn))
                    :write
                    (lambda (fd event exception)
                      (declare (ignore fd event exception))
                      (handle-flush cxn)))))

(defun cxn-write-octets (cxn octets)
  "Appends a vector of octets to the output queue, and schedules a
write operation when the socket is ready"
  (when (connectedp cxn)
    ;; Append the output to the queue
    (cond
      ((output-buf-of cxn)
       (setf (cdr (output-tail-of cxn)) (cons octets nil))
       (setf (output-tail-of cxn) (cdr (output-tail-of cxn))))
      (t
       (setf (output-buf-of cxn) (list octets))
       (setf (output-tail-of cxn) (output-buf-of cxn))))

    (schedule-cxn-output cxn)))

(defun cxn-write-string (cxn str)
  "Writes a string to the given connection.  Newlines are converted to
CRLF sequences on the fly."
  (cxn-write-octets cxn
                    (babel:string-to-octets
                     (ppcre:regex-replace-all #?/\n/ str #?"\r\n"))))

(defmethod cxn-write ((cxn data-cxn) fmt &rest args)
  "Writes a formatted string to the given connction."
  (cxn-write-string cxn (format nil "~?" fmt args)))

(defun cxn-broadcast (not-cxn fmt &rest args)
  "Sends a formatted message to all connections except the one in
NOT-CXN."
  (declare (type simple-string fmt))
  (let ((str (format nil "~?" fmt args)))
    (dolist (cxn *cxns*)
      (unless (eql cxn not-cxn)
        (cxn-write-string cxn str)))))

(defmethod cxn-close ((cxn data-cxn) &key abort)
  "Closes a data connection.  Will attempt to send remaining output
unless ABORT is T."
  (when (iolib.multiplex::fd-monitored-p *event-base*
                                         (socket-os-fd (socket-of cxn))
                                         :read)
    (remove-fd-handlers *event-base* (socket-os-fd (socket-of cxn))
                        :read t))
  (when (timer-of cxn)
    (remove-timer *event-base* (timer-of cxn))
    (setf (timer-of cxn) nil))
  (setf (connectedp cxn) nil)
  (cond
    ((and (not abort)
          (output-buf-of cxn))
     ;; We still have output to send, shutdown only half the
     ;; connection until we send it
     (shutdown (socket-of cxn) :read t))
    (t
     ;; Nothing more to do, so tear down the whole connection
     (when (iolib.multiplex::fd-monitored-p *event-base*
                                            (socket-os-fd (socket-of cxn))
                                            :write)
       (remove-fd-handlers *event-base* (socket-os-fd (socket-of cxn)) :write t))
     (shutdown (socket-of cxn) :read t :write t)
     (close (socket-of cxn))))
  (setf *cxns* (delete cxn *cxns*)))

(defun close-all-cxns ()
  "Closes listener and any established connections."
  (when *listener*
    (close *listener*))
  (dolist (cxn (copy-list *cxns*))
    (cxn-close cxn :abort t))
  (setf *cxns* nil))

(defclass tempus-cxn (data-cxn)
  ((state :accessor state-of :initform 'login :type symbol)
   (connect-time :accessor connect-time-of :initform (local-time:now))
   (need-prompt :accessor need-prompt-p :initform t)
   (account :accessor account-of :initform nil)
   (actor :accessor actor-of :initform nil)
   (page-buf :accessor page-buf-of :initform "")
   (wait :accessor wait-of :initform 0)
   (idle :accessor idle-of :initform 0)
   (mode-data :accessor mode-data-of :initform nil)
   (original-actor :accessor original-actor-of :initform nil)
   (snooping :accessor snooping-of :initform nil)
   (snooped-by :accessor snooped-by-of :initform nil)))

