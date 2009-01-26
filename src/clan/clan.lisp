(in-package :tempus)

(defclass clan-member ()
  ((idnum :accessor idnum-of :initarg :idnum)
   (rank :accessor rank-of :initarg :rank)
   (no-mail :accessor no-mail-of :initarg :no-mail)))

(defclass clan ()
  ((idnum :accessor idnum-of :initarg :idnum)
   (name :accessor name-of :initarg :name)
   (badge :accessor badge-of :initarg :badge)
   (top-rank :accessor top-rank-of :initarg :top-rank)
   (rank-names :accessor rank-names-of :initarg :rank-names)
   (members :accessor members-of :initform nil)
   (rooms :accessor rooms-of :initform nil)
   (bank :accessor bank-of :initarg :bank)))

(defvar *clans* nil)

(defun boot-clans ()
  (slog "Reading clans")
  ;; FIXME: actually fill this in
  (slog "Clan boot successful"))

(defun real-clan (clan-id)
  nil)