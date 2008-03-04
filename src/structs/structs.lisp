(in-package :tempus)

(defclass extra-descr-data ()
  ((keyword :accessor keyword-of :initarg :keyword :initform nil)
   (description :accessor description-of :initarg :description :initform nil)))