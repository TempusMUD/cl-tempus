(in-package :tempus)

(defparameter +quest-arena+ (ash 1 7))

(defclass quest ()
  ((kind :accessor kind-of :initarg :kind :initform nil)
   (name :accessor name-of :initarg :name :initform nil)
   (flags :accessor flags-of :initarg :flags :initform nil)
   (description :accessor description-of :initarg :description :initform nil)
   (updates :accessor updates-of :initarg :updates :initform nil)
   (owner-level :accessor owner-level-of :initarg :owner-level :initform nil)
   (min-level :accessor min-level-of :initarg :min-level :initform nil)
   (max-level :accessor max-level-of :initarg :max-level :initform nil)
   (min-gen :accessor min-gen-of :initarg :min-gen :initform nil)
   (max-gen :accessor max-gen-of :initarg :max-gen :initform nil)
   (loadroom :accessor loadroom-of :initarg :loadroom :initform nil)))

(defun quest-flagged (quest flag)
  (logtest (flags-of quest) flag))

(defun boot-quests ()
  nil)

(defun quest-name (idnum)
  "<not implemented>")

(defun quest-by-vnum (vnum)
  nil)

(defun remove-from-quest (idnum)
  nil)