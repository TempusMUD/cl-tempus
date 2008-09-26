(in-package #:tempus.tests)

(def-suite :tempus)

(defclass mock-cxn (tempus::tempus-cxn)
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

(defun make-mock-player (name)
  (let* ((link (make-instance 'mock-cxn))
         (player (make-instance 'tempus::player
                               :name name
                               :aliases (format nil "~(~a .~:*~a~)" name)
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
      (tempus::char-from-room ch))
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
           (fail "Signal caught in unwind-protect: ~a" err))))))

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
                 collect `(setup-mock-player ,var 3013))
            ,@body)
       (handler-case
           (progn
             ,@(loop for var in vars
                    collect `(destroy-mock-player ,var)))
         (t (err)
           (fail "Signal caught in unwind-protect: ~a" err))))))
