(in-package #:tempus)

(defun key-number (car)
  (aref (value-of car) 0))
(defun door-state (car)
  (aref (value-of car) 1))
(defun room-number (car)
  (aref (value-of car) 0))
(defun car-flags (car)
  (aref (value-of car) 2))
(defun car-special (car)
  (aref (value-of car) 3))

(defun max-energy (engine)
  (aref (value-of engine) 0))
(defun cur-energy (engine)
  (aref (value-of engine) 1))
(defun engine-state (engine)
  (aref (value-of engine) 2))
(defun use-rate (engine)
  (aref (value-of engine) 3))

(defun car-openable (car)
  (logtest (door-state car) +cont-closeable+))
  
(defun car-closed (car)
  (logtest (door-state car) +cont-closed+))

(defun is-device (obj)
  (= (kind-of obj) +item-device+))