(in-package #:tempus)

(defconstant +cxn-buffer-size+ 64556)
(defvar *rebooting* nil)
(defvar *shutdown* nil)
(defvar *cxns* nil)

(defun portable-nonblock (cxn)
  #+sbcl (setf (sb-bsd-sockets:non-blocking-mode (cxn-socket cxn)) t)
  #+cmu (let ((fd (cxn-fd cxn)))
		  (unix:unix-fcntl (cxn-fd cxn)
						   1
						   (logior (unix:unix-fcntl fd) 0 0) #o4000)))

;;;
;;; cxn class - base class for network connections
;;;
(defclass cxn ()
	((fd :accessor cxn-fd :initarg :fd :type fixnum)
	 (peer-addr :accessor peer-addr)
#+sbcl	(socket :accessor cxn-socket :initarg :socket :type sb-bsd-sockets:socket)
	(connected :accessor cxn-connected :type symbol :initform t)))

(defmethod cxn-write ((cxn cxn) fmt &rest args) (declare (ignorable args)) t)
(defmethod cxn-commands ((cxn cxn)) nil)
(defmethod handle-input ((cxn cxn)) (error "base input called"))
(defmethod handle-accept ((cxn cxn)) t)
(defmethod handle-flush ((cxn cxn)) t)
(defmethod handle-close ((cxn cxn)) t)
(defmethod handle-error ((cxn cxn)) 
	(setf (cxn-connected cxn) nil))

;;;
;;; listener cxn class - class for listening on a socket and constructing
;;;                      more cxns
;;;
(defclass listener-cxn (cxn)
	((accept-type :accessor listener-accept-type :initarg :accept-type :type symbol)))

(defmethod handle-input ((cxn listener-cxn))
  #+sbcl (let* ((new-socket (sb-bsd-sockets:socket-accept (cxn-socket cxn)))
				(new-cxn (make-instance (listener-accept-type cxn)
										:fd (sb-bsd-sockets:socket-file-descriptor new-socket)
										:socket new-socket)))

		   (portable-nonblock new-cxn)
		   (let ((addr (sb-bsd-sockets:socket-peername new-socket)))
			 (setf (peer-addr new-cxn)
				   (format nil "~d.~d.~d.~d"
						   (aref addr 0)
						   (aref addr 1)
						   (aref addr 2)
						   (aref addr 3))))
		   (push new-cxn *cxns*)
		   (handle-accept new-cxn))
  #+cmu (let* ((new-fd (extensions:accept-tcp-connection (cxn-fd cxn)))
			   (new-cxn (make-instance (listener-accept-type cxn) :fd new-fd)))

		  (portable-nonblock new-cxn)
		  (push new-cxn *cxns*)
		  (handle-accept new-cxn)))

;;;
;;; data cxn class - cxn used to transmit line-delimited data
;;;
(defclass data-cxn (cxn)
  ((input-buf :accessor cxn-input-buf :type (or null simple-string) :initform nil)
   (input-len :accessor cxn-input-len :type fixnum :initform 0)
   (output-buf :accessor cxn-output-buf :initform nil)
   (output-tail :accessor cxn-output-tail :initform nil)
   (commands :accessor cxn-commands :type list :initform '())))

(defmethod cxn-queue-output ((cxn data-cxn) str)
  (cond
    ((cxn-output-buf cxn)
     (setf (cdr (cxn-output-tail cxn)) (cons str nil))
     (setf (cxn-output-tail cxn) (cdr (cxn-output-tail cxn))))
    (t
     (setf (cxn-output-buf cxn) (list str))
     (setf (cxn-output-tail cxn) (cxn-output-buf cxn)))))

(defmethod cxn-write ((cxn data-cxn) fmt &rest args)
  (let ((str (prepare-output (format nil "~?" fmt args))))
    (cxn-queue-output cxn str)))

(defmethod handle-input ((cxn data-cxn))
  (cxn-read cxn)
  (when (cxn-connected cxn)
	(queue-commands cxn)))

(defmethod handle-flush ((cxn data-cxn))
  (loop
     for str = (pop (cxn-output-buf cxn))
     while str 
     when (plusp (length str))
     do (let ((buf (map '(vector (unsigned-byte 8)) #'char-code str)))
          (multiple-value-bind (count err)
              (sb-unix:unix-write (cxn-fd cxn) buf 0 (length str))

            (unless (zerop err)
              (error "write() returned error ~a!" err))

            (cond
              ((zerop count)
               (setf (cxn-connected cxn) nil)
               (return-from handle-flush))
              ((= count (length str))
               nil)
              (t
               (push (subseq str count) (cxn-output-buf cxn))
               (return-from handle-flush))))))
  (setf (cxn-output-tail cxn) nil))
	
(defun cxn-listen (port accept-type)
  "Sets up a listener socket on port with the accept-handler accept."
  #+sbcl (let* ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol (sb-bsd-sockets:get-protocol-by-name "tcp")))
				(new-listener
				 (make-instance 'listener-cxn
								:fd (sb-bsd-sockets:socket-file-descriptor s)
								:socket s
								:accept-type accept-type)))
		   (setf (sb-bsd-sockets:sockopt-reuse-address (cxn-socket new-listener)) t)
		   (sb-bsd-sockets:socket-bind (cxn-socket new-listener) #(0 0 0 0) port)
		   (sb-bsd-sockets:socket-listen (cxn-socket new-listener) 10)
		   (push new-listener *cxns*)
		   new-listener)
  #+cmu (let ((new-listener
			   (make-instance 'listener-cxn
							  :fd (extensions:create-inet-listener port :stream
																   :reuse-address t
																   :backlog 10)
							  :accept-type accept-type)))

		  (push new-listener *cxns*)
		  new-listener))

(defun cxn-poll (read-cxns write-cxns except-cxns)
  (declare (optimize (speed 3) (safety 0))
		   (ftype (function (cxn) fixnum) cxn-fd)
           (sb-ext:muffle-conditions sb-ext:compiler-note))

  (let ((max-fd -1))
	(declare (integer max-fd))
	(sb-alien:with-alien ((read-set (sb-alien:struct sb-unix:fd-set))
                          (write-set (sb-alien:struct sb-unix:fd-set))
                          (except-set (sb-alien:struct sb-unix:fd-set)))

	  ;; Add all the cxn fds to the proper fdsets, and 
	  (macrolet ((add-cxns-to-fd-set (cxn-list set)
				   (let ((max (gensym)) (cxn (gensym)))
					 `(progn
					   (sb-unix:fd-zero ,set)
					   (let ((,max
							  (loop for ,cxn in ,cxn-list
									do (sb-unix:fd-set (cxn-fd ,cxn) ,set)
									maximize (cxn-fd ,cxn))))

						 (when (> ,max max-fd)
						   (setf max-fd ,max)))))))

		(when read-cxns
		  (add-cxns-to-fd-set read-cxns read-set))
		(when write-cxns
		  (add-cxns-to-fd-set write-cxns write-set))
		(when except-cxns
		  (add-cxns-to-fd-set except-cxns except-set)))
			
	  (multiple-value-bind (val err)
		  (sb-unix:unix-fast-select (1+ max-fd)
							(when read-cxns (sb-alien:addr read-set))
							(when write-cxns (sb-alien:addr write-set))
							(when except-cxns (sb-alien:addr except-set))
							0 0)
		(declare (ignorable val))

		(unless (zerop err)
		  (error "select() returned error ~a!" err)))

	  (values
	   (remove-if-not
		(lambda (cxn) (sb-unix:fd-isset (cxn-fd cxn) read-set))
		read-cxns)
	   (remove-if-not
		(lambda (cxn) (sb-unix:fd-isset (cxn-fd cxn) write-set))
		write-cxns)
	   (remove-if-not
		(lambda (cxn) (sb-unix:fd-isset (cxn-fd cxn) except-set))
		except-cxns)))))

(defun cxn-update-input ()
  (multiple-value-bind (read-cxns write-cxns except-cxns)
	  (cxn-poll *cxns* nil *cxns*)
	(declare (ignore write-cxns))
		
	(dolist (cxn except-cxns)
	  (handle-error cxn))
	(dolist (cxn read-cxns)
	  (handle-input cxn))
	(setf *cxns* (delete-if (lambda (cxn)
							  (unless (cxn-connected cxn)
								(cxn-close cxn)
								t))
							*cxns*))))

(defun cxn-update-output ()
  (multiple-value-bind (read-cxns write-cxns except-cxns)
	  (cxn-poll nil *cxns* *cxns*)
	(declare (ignore read-cxns))
		
	(dolist (cxn except-cxns)
	  (handle-error cxn))
	(dolist (cxn write-cxns)
	  (handle-flush cxn))
	(setf *cxns* (delete-if (lambda (cxn)
							  (unless (cxn-connected cxn)
								(cxn-close cxn)
								t))
							*cxns*))))

(defun cxn-read (cxn)
  "Reads input from the cxn - returns t if cxn is still connected, nil
if cxn disconnected"
  (let* ((buf (make-array +cxn-buffer-size+ :element-type '(unsigned-byte 8))))
	(multiple-value-bind (count err)
		(sb-unix:unix-read
		 (cxn-fd cxn)
		 (sb-alien::vector-sap buf)
		 +cxn-buffer-size+)
			
	  (unless (zerop err)
		(error "read() returned error ~a!" err))

	  (cond
		((null count)
		 (setf (cxn-connected cxn) nil))
		((zerop count)
		 (setf (cxn-connected cxn) nil))
		((eql (aref buf 0) #xff)
		 ;; ignore telnet sequences
		 nil)
		(t
		 (setf (cxn-input-buf cxn)
			   (concatenate 'string (cxn-input-buf cxn)
							(map-into (make-string count) #'code-char (subseq buf 0 count))))
		 (incf (cxn-input-len cxn) count))))))

(defun queue-commands (cxn)
  "Finds all the line breaks and pushes lines onto the command queue"

  (loop
   for end-line = (position #\newline (cxn-input-buf cxn) :end (cxn-input-len cxn))
   while (and (cxn-connected cxn) end-line)
   do (push
	   (string-right-trim
		'(#\space #\return #\newline #\tab)
		(subseq (cxn-input-buf cxn) 0 end-line))
	   (cxn-commands cxn))
   (setf (cxn-input-buf cxn)
		 (subseq (cxn-input-buf cxn) (1+ end-line)))
   (decf (cxn-input-len cxn) (1+ end-line))))

(defun cxn-connect (hostname port cxn-type)
  #+sbcl (let* ((host (sb-bsd-sockets:get-host-by-name hostname))
				(addr (sb-bsd-sockets:host-ent-address host))
				(new-socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol (sb-bsd-sockets:get-protocol-by-name "tcp"))))

		   (when (sb-bsd-sockets:socket-connect addr port)
			 (let ((new-cxn (make-instance cxn-type :fd (sb-bsd-sockets:socket-file-descriptor new-socket) :socket new-socket)))
			   (portable-nonblock new-cxn)
			   (setf (cxn-input-buf new-cxn) "")
			   (setf (cxn-output-buf new-cxn) "")
			   (push new-cxn *cxns*)
			   new-cxn)))
  #+cmu (let* ((new-fd (extensions:connect-to-inet-socket hostname port))
			   (new-cxn (make-instance cxn-type :fd new-fd)))
		  (portable-nonblock new-cxn)
		  (setf (cxn-input-buf new-cxn) "")
		  (setf (cxn-output-buf new-cxn) "")
		  (push new-cxn *cxns*)
		  new-cxn))

(defun unix-shutdown (fd mode)
  #+sbcl (sb-unix::void-syscall ("shutdown" sb-alien:int sb-alien:int)
								fd mode)
  #+cmu (unix::void-syscall ("shutdown" c-call:int c-call:int)
							fd mode))

(defun cxn-close (cxn)
  (handle-close cxn)
  (unix-shutdown (cxn-fd cxn) 2)
  #+sbcl (sb-bsd-sockets:socket-close (cxn-socket cxn))
  #+cmu (unix:unix-close (cxn-fd cxn)))

(defun close-all-cxns ()
  (dolist (cxn *cxns*)
	(cxn-close cxn))
  (setf *cxns* nil))

(defun prepare-output (str)
  (loop
   for read-pt = 0 then (1+ read-pt)
   for write-pt = 0 then (1+ write-pt)
   while (< read-pt (length str))
   with result = (make-string (* 2 (length str)))
   do (case (char str read-pt)
		(#\newline
		 (setf (char result write-pt) #\return)
		 (incf write-pt)
		 (setf (char result write-pt) #\newline))
		(t
		 (setf (char result write-pt) (char str read-pt))))
   finally (return (subseq result 0 write-pt))))

(defun cxn-broadcast (not-cxn fmt &rest args)
  (declare (type simple-string fmt))
  (let ((str (format nil "~?" fmt args)))
	(dolist (cxn *cxns*)
	  (unless (eql cxn not-cxn)
		(cxn-write cxn "~a" str)))))

(defclass tempus-cxn (data-cxn)
  ((state :accessor state-of :initform 'login :type symbol)
   (need-prompt :accessor need-prompt-p :initform t)
   (account :accessor account-of :initform nil)
   (actor :accessor actor-of :initform nil)
   (page-buf :accessor page-buf-of :initform "")
   (wait :accessor wait-of :initform 0)
   (mode-data :accessor mode-data-of :initform nil)))

