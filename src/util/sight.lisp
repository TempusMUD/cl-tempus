(in-package #:tempus)

(defun room-is-dark (room)
  nil)

(defun has-dark-sight (ch)
  nil)

(defun check-sight-self (ch)
  t)

(defun can-see-room (ch room)
  t)

(defmethod is-visible-to ((obj obj-data) ch)
  t)

(defmethod is-visible-to ((target creature) ch)
  t)
