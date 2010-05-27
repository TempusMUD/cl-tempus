(in-package #:tempus)

(defconstant +cxn-buffer-size+ 64556)
(defvar *rebooting* nil)
(defvar *shutdown* nil)
(defvar *cxns* nil)
(defvar *event-base* nil)
(defvar *listener* nil)

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
   (commands :accessor commands-of :type list :initform '())))

(defgeneric cxn-write (cxn fmt &rest args))
(defgeneric cxn-close (cxn))
(defgeneric handle-accept (cxn))
(defgeneric handle-flush (cxn))

(defun cxn-fill-input-buf (cxn)
  "Reads input from the cxn, stored into the cxn's input-buf."
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

(defun make-tempus-read-handler (new-cxn)
  (lambda (fd event exception)
    (declare (ignore fd event exception))
    (cxn-fill-input-buf new-cxn)
    (when (connectedp new-cxn)
      (queue-commands new-cxn))
    ;; Once we finish a single command, if there are multiple
    ;; commands in the queue, we need to set up a timer that will
    ;; wait for the time when it's ok to look for a command again.
    (when (and (connectedp new-cxn)
               (null (timer-of new-cxn)))
      (handle-command new-cxn)
      (when (and (commands-of new-cxn)
                 (null (timer-of new-cxn)))
        (setf (timer-of new-cxn)
              (add-timer *event-base*
                         (lambda ()
                           (handle-command new-cxn)
                           (when (null (commands-of new-cxn))
                             (remove-timer *event-base* (timer-of new-cxn))
                             (setf (timer-of new-cxn) nil)))
                         0.1))))))

(defun make-tempus-acceptor (cxn-type)
  (lambda (fd event exception)
    (declare (ignore fd event exception))
    (let ((new-socket (accept-connection *listener* :wait t)))
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

(defun cxn-queue-output (cxn str)
  (let ((buf (babel:string-to-octets str)))
    (cond
      ((output-buf-of cxn)
       (setf (cdr (output-tail-of cxn)) (cons buf nil))
       (setf (output-tail-of cxn) (cdr (output-tail-of cxn))))
      (t
       (setf (output-buf-of cxn) (list buf))
       (setf (output-tail-of cxn) (output-buf-of cxn)))))

  (unless (iolib.multiplex::fd-monitored-p *event-base*
                                           (socket-os-fd (socket-of cxn))
                                           :write)
    (set-io-handler *event-base*
                    (socket-os-fd (socket-of cxn))
                    :write
                    (lambda (fd event exception)
                      (declare (ignore fd event exception))
                      (handle-flush cxn)))))

(defmethod handle-flush ((cxn data-cxn))
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
  (setf (output-tail-of cxn) nil))

(defmethod cxn-close ((cxn data-cxn))
  (remove-fd-handlers *event-base* (socket-os-fd (socket-of cxn))
                      :read t :write t :error t)
  (when (timer-of cxn)
    (remove-timer *event-base* (timer-of cxn)))
  (shutdown (socket-of cxn) :read t :write t)
  (close (socket-of cxn))
  (setf (connectedp cxn) nil)
  (setf *cxns* (delete cxn *cxns*)))

(defun cxn-listen (port cxn-type)
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



(defun prepare-output (str)
  (cl-ppcre:regex-replace-all "\\n" str "
"))

(defmethod cxn-write ((cxn data-cxn) fmt &rest args)
  (let ((str (prepare-output (format nil "~?" fmt args))))
    (cxn-queue-output cxn str)))

(defun queue-commands (cxn)
  "Finds all the line breaks in the cxn's input buffer and pushes lines onto the command queue.  The rest is kept in the input buffer."
  (loop
   for end-line = (position #\newline (input-buf-of cxn) :end (input-len-of cxn))
   while (and (connectedp cxn) end-line)
   do (push
       (string-right-trim
        '(#\space #\return #\newline #\tab)
        (subseq (input-buf-of cxn) 0 end-line))
       (commands-of cxn))
   (setf (input-buf-of cxn)
         (subseq (input-buf-of cxn) (1+ end-line)))
   (decf (input-len-of cxn) (1+ end-line))))

(defun close-all-cxns ()
  (when *listener*
    (close *listener*))
  (dolist (cxn *cxns*)
    (cxn-close cxn))
  (setf *cxns* nil))

(defun cxn-broadcast (not-cxn fmt &rest args)
  (declare (type simple-string fmt))
  (let ((str (format nil "~?" fmt args)))
    (dolist (cxn *cxns*)
      (unless (eql cxn not-cxn)
        (cxn-write cxn "~a" str)))))

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

