(in-package #:tempus)

(defun door-state (car)
  (aref (value-of car) 1))

(defun car-openable (car)
  (logtest (door-state car) +cont-closeable+))
  
(defun car-closed (car)
  (logtest (door-state car) +cont-closed+))