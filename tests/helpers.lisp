(in-package #:tempus.tests)

(in-root-suite)
(defsuite (test-full :documentation "Tempus tests, including slow tests"))
(defsuite (test :in test-full :documentation "Tempus tests"))

(defclass mock-cxn (tempus::tempus-cxn)
  ())

(defclass mock-player (tempus::player)
  ((savedp :accessor savedp :initarg :savedp :initform nil)
   (fullp :accessor fullp :initarg :fullp :initform nil)
   (override-security :accessor override-security-p :initarg :override-security :initform nil)))

(defclass mock-account (tempus::account)
  ())

(defun escape-mock-str (str)
  (with-output-to-string (result)
    (loop for idx from 0 to (1- (length str)) do
         (princ
          (case (char str idx)
            (#\&
             "&&")
            (#\return
             "")
            (#\newline
             "~%")
            (t
             (char str idx)))
          result))))

(defmethod tempus::cxn-queue-output ((cxn mock-cxn) str)
  "Escapes the color codes before sending them to the tempus-cxn writing routines"
  (call-next-method cxn
                    (cl-ppcre:regex-replace-all #/\r\n/ str "~%")))

(defmethod tempus::cxn-write ((cxn mock-cxn) fmt &rest args)
  "Escapes the color codes before sending them to the tempus-cxn writing routines"
  (let ((str (format nil "~?" fmt args)))
    (call-next-method cxn "~a" (escape-mock-str str))))

(defmethod tempus::save-player-to-xml ((player mock-player))
  (setf (savedp player) t))

(defmethod tempus::security-is-member ((player mock-player) group-name)
  (declare (ignore group-name))
  (if (override-security-p player)
      t
      (call-next-method)))

(defmethod tempus::handle-close ((cxn mock-cxn))
  nil)

(defmethod tempus::save-account ((cxn mock-account))
  nil)

(defixture mock-clan
  (:setup (name)
    (let* ((clan-id (+ 900 (random 100)))
           (clan (tempus::create-clan clan-id)))
      (setf (tempus::name-of clan) (string-capitalize name))
      (setf (tempus::badge-of clan) (format nil "-- ~a --" (string-capitalize name)))
      (setf (tempus::top-rank-of clan) 5)
      clan))
  (:teardown (clan)
    (tempus::delete-clan (tempus::idnum-of clan))))

(defvar *mock-fd* 0)

(defun make-mock-cxn ()
  (incf *mock-fd*)
  (let ((cxn (make-instance 'mock-cxn
                            :fd *mock-fd*
                            :peer-addr "127.0.0.1")))
    (setf (tempus::state-of cxn) 'tempus::playing)
    cxn))

(defun mock-cxn-input (cxn fmt &rest args)
  (let ((msg (format nil "~?" fmt args)))
    (setf (tempus::cxn-input-buf cxn)
          (concatenate 'string (tempus::cxn-input-buf cxn) msg))
    (incf (tempus::cxn-input-len cxn) (length msg))))

(defun clear-mock-buffers (&rest chars)
  (dolist (ch chars)
    (setf (tempus::cxn-input-buf (tempus::link-of ch)) nil)
    (setf (tempus::cxn-output-buf (tempus::link-of ch)) nil)
    (setf (tempus::cxn-input-len (tempus::link-of ch)) 0)))

(defun char-output (ch)
  (format nil "~{~a~}" (tempus::cxn-output-buf (tempus::link-of ch))))

(defmacro char-output-is (ch fmt &rest args)
  (if (null args)
    `(is (equal ,fmt (char-output ,ch)))
    `(let ((msg (cl-ppcre:regex-replace-all #/\n/ (format nil ,fmt ,@args) "~%")))
       (is (equal msg (char-output ,ch))))))

(defmacro char-output-has (ch fmt &rest args)
  (if (null args)
    `(is (search ,fmt (char-output ,ch)))
    `(let ((msg (cl-ppcre:regex-replace-all #/\n/ (format nil ,fmt ,@args) "~%")))
       (is (search msg (char-output ,ch))))))

(defvar *top-mock-player* 90000)
(defvar *top-mock-account* 90000)

(defixture mock-player
  (:setup (name &key (level 1) (fullp nil) (override-security nil) (room-num 100))
    (let* ((link (make-mock-cxn))
           (player (make-instance 'mock-player
                                  :name (string-capitalize name)
                                  :idnum (incf *top-mock-player*)
                                  :aliases (format nil "~(~a .~:*~a~)" name)
                                  :level (or level 1)
                                  :override-security override-security
                                  :link link
                                  :fullp fullp))
           (account (make-instance 'mock-account
                                   :idnum (incf *top-mock-account*)
                                   :name (format nil "test-~(~a~)" name))))
      (push (tempus::link-of player) tempus::*cxns*)
      (setf (tempus::actor-of link) player)
      (setf (tempus::account-of link) account)
      (setf (tempus::account-of player) account)
      (setf (tempus::load-room-of player) room-num)
      (cond
        (fullp
         (postmodern:execute (:insert-into 'accounts :set
                                           'idnum (tempus::idnum-of account)))
         (tempus::save-account account)
         (tempus::create-new-player player account)
         (let ((tempus::*log-output* (make-broadcast-stream)))
           (tempus::player-to-game player))
         (setf (tempus::cxn-output-buf link) nil))
        (t
         (push player tempus::*characters*)
         (setf (gethash (tempus::idnum-of player) tempus::*character-map*) player)
         (tempus::char-to-room player (tempus::real-room room-num))))
      player))
  (:teardown (ch)
    (when ch
      (when (tempus::in-room-of ch)
        (tempus::char-from-room ch t))
      (setf tempus::*characters* (delete ch tempus::*characters*))
      (remhash (tempus::idnum-of ch) tempus::*character-map*)
      (when (fullp ch)
        (tempus::delete-player ch)
        (postmodern:execute (:delete-from 'accounts :where (:= 'idnum (tempus::idnum-of (tempus::account-of ch))))))
      (remhash (tempus::name-of (tempus::account-of ch)) tempus::*account-name-cache*)
      (remhash (tempus::idnum-of (tempus::account-of ch)) tempus::*account-idnum-cache*)
      (setf tempus::*cxns* (delete (tempus::link-of ch) tempus::*cxns*)))))

(defixture mock-mobile
  (:setup (var &key name (room-num 100))
    (let ((mock-cxn (make-mock-cxn))
          (mobile (tempus::read-mobile 1)))
      (setf (tempus::name-of mobile) name)
      (setf (tempus::aliases-of mobile) name)
      (setf (tempus::link-of mobile) mock-cxn)
      (push mobile tempus::*characters*)
      (tempus::char-to-room mobile (tempus::real-room room-num))
      mobile))
  (:teardown (mob)
    (when mob
      (tempus::extract-creature mob 'tempus::disconnecting))))

(defixture mock-object
  (:setup (var &key (name "mock object"))
    (let* ((shared (make-instance 'tempus::obj-shared-data))
           (obj (make-instance 'tempus::obj-data
                               :shared shared)))
      (dotimes (i tempus::+max-obj-affect+)
        (setf (aref (tempus::affected-of obj) i)
              (make-instance 'tempus::obj-affected-type)))
      (setf (tempus::name-of obj) name)
      (setf (tempus::aliases-of obj) name)
      (setf (tempus::line-desc-of obj) (format nil "~:(~a~) is here." name))
      obj))
  (:teardown (obj)
    (tempus::extract-obj obj)))

(defixture mock-obj-prototype
  (:setup (var &key (name "a mock object"))
    (let* ((vnum (loop for num from 100 upto 199
                        when (null (tempus::real-object-proto num)) do (return num)
                        finally (return nil)))
           (new-object-shared (make-instance 'tempus::obj-shared-data :vnum vnum))
           (new-object (make-instance 'tempus::obj-data
                                      :name name
                                      :aliases name
                                      :line-desc (format nil "~a is here." name)
                                      :shared new-object-shared)))
    (dotimes (i tempus::+max-obj-affect+)
      (setf (aref (tempus::affected-of new-object) i)
            (make-instance 'tempus::obj-affected-type)))
    (setf (tempus::proto-of new-object-shared) new-object)
    (setf (gethash vnum tempus::*object-prototypes*) new-object)))
  (:teardown (obj)
    (dolist (obj (copy-list tempus::*object-list*))
      (when (= (tempus::vnum-of obj) (tempus::vnum-of obj))
        (tempus::extract-obj obj)))

    (remhash (tempus::vnum-of obj) tempus::*object-prototypes*)

    (dolist (tch tempus::*characters*)
      (when (and (typep tch 'tempus::player)
                 (tempus::olc-obj-of tch)
                 (eql (tempus::vnum-of (tempus::olc-obj-of tch)) (tempus::vnum-of obj)))
        (setf (tempus::olc-obj-of tch) nil)))))

(defixture mock-room
  (:setup (var &key name)
    (let* ((room-num (loop for num from 100 upto 199
                        when (null (tempus::real-room num)) do (return num)
                        finally (return nil)))
           (room (make-instance 'tempus::room-data
                                :number room-num
                                :name name
                                :description (format nil "This is test room '~a'.~%" name)
                                :zone (tempus::real-zone 1))))
      (setf (gethash room-num tempus::*rooms*) room)
      (push room (tempus::world-of (tempus::real-zone 1)))
      room))
  (:teardown (room)
    (remhash (tempus::number-of room) tempus::*rooms*)
    (setf (tempus::world-of (tempus::zone-of room))
          (delete room (tempus::world-of (tempus::zone-of room))))))

(defmacro with-captured-log (log expr &body body)
  `(let ((tempus::*log-output* (make-string-output-stream)))
     (unwind-protect
          ,expr
       (close tempus::*log-output*))
     (let ((,log (get-output-stream-string tempus::*log-output*)))
       (declare (ignorable ,log))
       ,@body)))

(defvar *function-traces* (make-hash-table))

(defmacro tracing-function (func-names &body body)
  `(unwind-protect
        (progn
          ,@(loop for func in func-names collect
                 `(sb-int::encapsulate ',func
                                       'tracer
                                       '(progn
                                         (push sb-int:arg-list
                                          (gethash ',func *function-traces*))
                                         (apply sb-int::basic-definition
                                          sb-int:arg-list))))
          ,@body)
     (progn
       ,@(loop for func in func-names collect
              `(sb-int::unencapsulate ',func 'tracer)))))

(defmacro function-trace-bind (bindings form &body body)
  `(unwind-protect
        (progn
          (clrhash *function-traces*)
          ,@(loop for binding in bindings
               as func = (second binding) collect
                 `(sb-int::encapsulate ',func
                                       'tracer
                                       '(progn
                                         (push (symbol-value 'sb-int:arg-list)
                                          (gethash ',func *function-traces*))
                                         (apply sb-int::basic-definition
                                          sb-int:arg-list))))
          ,form
          (let ,(loop for binding in bindings collect
                    `(,(first binding)
                       (gethash ',(second binding) *function-traces*)))
            ,@body))
     (progn
       ,@(loop for binding in bindings collect
              `(sb-int::unencapsulate ',(second binding) 'tracer)))))