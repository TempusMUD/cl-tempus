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

(defun make-mock-player (name room-num)
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
    (push link tempus::*cxns*)
    (tempus::char-to-room player (tempus::real-room room-num))
    player))

(defun destroy-mock-player (ch)
  (tempus::char-from-room ch)
  (setf tempus::*cxns* (delete (tempus::link-of ch) tempus::*cxns*)))

(defmacro with-mock-players (vars &body body)
  `(let ,vars
     (unwind-protect
          (progn
            ,@(loop
                 for var in vars
                 collect `(setf ,var
                                (make-mock-player ,(string-capitalize var)
                                                  3013)))
            ,@body)
       (handler-case
           (progn
             ,@(loop for var in vars
                    collect `(destroy-mock-player ,var)))
         (t (err)
           (fail "Signal caught in unwind-protect: ~a" err))))))
