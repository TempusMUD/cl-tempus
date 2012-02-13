(in-package #:tempus)

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
   (commands :accessor commands-of :type list :initform '())
   (wait :accessor wait-of :initform 0))
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

(defclass tempus-cxn (data-cxn)
  ((state :accessor state-of :initform 'login :type symbol)
   (connect-time :accessor connect-time-of :initform (local-time:now))
   (need-prompt :accessor need-prompt-p :initform t)
   (account :accessor account-of :initform nil)
   (actor :accessor actor-of :initform nil)
   (page-buf :accessor page-buf-of :initform "")
   (idle :accessor idle-of :initform 0)
   (mode-data :accessor mode-data-of :initform nil)
   (original-actor :accessor original-actor-of :initform nil)
   (snooping :accessor snooping-of :initform nil)
   (snooped-by :accessor snooped-by-of :initform nil)))