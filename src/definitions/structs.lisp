(in-package :tempus)

(defclass extra-descr-data ()
  ((keyword :accessor keyword-of :initarg :keyword :initform nil)
   (description :accessor description-of :initarg :description :initform nil)))

(defparameter +special-cmd+ 0)          ; special command response
(defparameter +special-tick+ 1)         ; special periodic action
(defparameter +special-death+ 2)        ; special death notification
(defparameter +special-fight+ 3)        ; special fight starting
(defparameter +special-combat+ 4)       ; special in-combat ability
(defparameter +special-enter+ 5)        ; special upon entrance
(defparameter +special-leave+ 6)        ; special upon exit
(defparameter +special-reset+ 7)        ; zone reset

(defvar *special-funcs* (make-hash-table :test 'equal))
(defvar *special-flags* (make-hash-table :test 'equal))

