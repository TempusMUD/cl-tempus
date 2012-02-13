(in-package #:tempus)

(defvar *progs* nil)

(defclass prog-event ()
  ((phase :accessor phase-of :initarg :phase)
   (kind :accessor kind-of :initarg :kind)
   (cmd :accessor cmd-of :initarg :cmd)
   (args :accessor args-of :initarg :args)
   (subject :accessor subject-of :initarg :subject)
   (object :accessor object-of :initarg :object)))

(defclass prog-env ()
  ((exec-pt :accessor exec-pt-of :initarg :exec-pt)
   (executed :accessor executed-of :initarg :executed)
   (speed :accessor speed-of :initarg :speed)
   (wait :accessor wait-of :initarg :wait)
   (condition :accessor condition-of :initarg :condition)
   (owner :accessor owner-of :initarg :owner)
   (target :accessor target-of :initarg :target)
   (event :accessor event-of :initarg :event)
   (tracing :accessor tracing-of :initarg :tracing)
   (state :accessor state-of :initarg :state)))


(defun prog-unreference-object (obj)
  (dolist (prog *progs*)
    (when (eql (object-of (event-of prog)) obj)
      (setf (object-of (event-of prog)) nil))))

(defun trigger-prog-dying (ch killer)
  nil)

(defun trigger-prog-death (victim prog-kind killer)
  nil)