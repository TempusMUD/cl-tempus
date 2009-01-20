(in-package #:tempus.tests)

(in-root-suite)
(defsuite (test :documentation "Tempus tests"))

(defclass mock-cxn (tempus::tempus-cxn)
  ())

(defclass mock-player (tempus::player)
  ((savedp :accessor savedp :initarg :savedp :initform nil)))

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

(defmethod tempus::handle-close ((cxn mock-cxn))
  nil)

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

(defun make-mock-mobile (name)
  (let ((mock-cxn (make-instance 'mock-cxn))
        (mobile (tempus::read-mobile 1)))
    (setf (tempus::name-of mobile) name)
    (setf (tempus::aliases-of mobile) name)
    (setf (tempus::link-of mobile) mock-cxn)
    (push mobile tempus::*characters*)
    mobile))

(defun setup-mock-mobile (ch room-num)
  (tempus::char-to-room ch (tempus::real-room room-num)))

(defun destroy-mock-mobile (mob)
  (when mob
    (tempus::extract-creature mob 'tempus::disconnecting)))

(defmacro with-mock-mobiles (vars &body body)
  `(let ,vars
     (unwind-protect
          (progn
            ,@(loop
                 for var in vars
                 collect `(setf ,var
                                (make-mock-mobile ,(string-capitalize var))))
            ,@(loop
                 for var in vars
                 collect `(setup-mock-mobile ,var 3002))
            ,@body)
       (handler-case
           (progn
             ,@(loop for var in vars
                    collect `(destroy-mock-mobile ,var)))
         (t (err)
           (stefil::record-failure 'stefil::error-in-teardown :condition err))))))

(defvar *top-mock-player* 90000)

(defun make-mock-player (name)
  (let* ((link (make-instance 'mock-cxn))
         (player (make-instance 'mock-player
                               :name name
                               :idnum (incf *top-mock-player*)
                               :aliases (format nil "~(~a .~:*~a~)" name)
                               :level 1
                               :link link))
         (account (make-instance 'tempus::account
                                 :name (format nil "test-~(~a~)" name))))
    (setf (tempus::actor-of link) player)
    (setf (tempus::account-of link) account)
    (setf (tempus::account-of player) account)
    (setf (tempus::state-of link) 'tempus::playing)
    (push player tempus::*characters*)
    player))

(defun setup-mock-player (ch room-num)
  (push (tempus::link-of ch) tempus::*cxns*)
  (tempus::char-to-room ch (tempus::real-room room-num)))

(defun destroy-mock-player (ch)
  (when ch
    (when (tempus::in-room-of ch)
      (tempus::char-from-room ch t))
    (setf tempus::*cxns* (delete (tempus::link-of ch) tempus::*cxns*))))

(defun make-mock-object (&optional (name "mock object"))
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

(defun destroy-mock-object (obj)
  (tempus::extract-obj obj))

(defmacro with-mock-objects (obj-specifiers &body body)
  (let ((vars (mapcar #'first obj-specifiers)))
    `(let ,vars
       (unwind-protect
            ,@(loop
                 for (var name) in obj-specifiers
                 collect `(setf ,var
                                (make-mock-object ,name)))
            ,@body)
       (handler-case
           (progn
             ,@(loop for var in vars
                    collect `(destroy-mock-object ,var)))
         (t (err)
           (stefil::record-failure 'stefil::error-in-teardown :condition err))))))

(defmacro with-mock-players (vars &body body)
  `(let ,vars
     (unwind-protect
          (progn
            ,@(loop
                 for var in vars
                 collect `(setf ,var
                                (make-mock-player ,(string-capitalize var))))
            ,@(loop
                 for var in vars
                 collect `(setup-mock-player ,var 3002))
            ,@body)
       (handler-case
           (progn
             ,@(loop for var in vars
                    collect `(destroy-mock-player ,var)))
         (t (err)
           (stefil::record-failure 'stefil::error-in-teardown :condition err))))))

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