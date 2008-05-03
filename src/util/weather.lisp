(in-package :tempus)

(defparameter +moon-new+ 0)
(defparameter +moon-wax-crescent+ 1)
(defparameter +moon-first-quarter+ 2)
(defparameter +moon-wax-gibbous+ 3)
(defparameter +moon-full+ 4)
(defparameter +moon-wane-gibbous+ 5)
(defparameter +moon-last-quarter+ 6)
(defparameter +moon-wane-crescent+ 7)

(defvar *time-info*)

(defclass mud-time ()
  ((hour :accessor hour-of :initarg :hour :type fixnum)
   (day :accessor day-of :initarg :day :type fixnum)
   (month :accessor month-of :initarg :month :type fixnum)
   (year :accessor year-of :initarg :year :type fixnum)))

(defun lunar-phase (day)
  (cond
    ((or (= day 0) (= day 23))
     +moon-new+)
    ((< day 5)
     +moon-wax-crescent+)
    ((< day 7)
     +moon-first-quarter+)
    ((< day 11)
     +moon-wax-gibbous+)
    ((< day 13)
     +moon-full+)
    ((< day 17)
     +moon-wane-gibbous+)
    ((< day 19)
     +moon-last-quarter+)
    (t
     +moon-wane-crescent+)))

(defun local-time-of (zone)
  (let ((local-time (make-instance 'mud-time
                                   :hour (+ (hour-of *time-info*) (hour-mod-of zone))
                                   :day (day-of *time-info*)
                                   :month (month-of *time-info*)
                                   :year (+ (year-of *time-info*) (year-mod-of zone)))))
    (loop while (> (hour-of local-time) 23) do
         (decf (hour-of local-time) 24)
         (incf (day-of local-time)))
    (loop while (< (hour-of local-time) 0) do
         (incf (hour-of local-time) 24)
         (decf (day-of local-time)))
    (loop while (> (day-of local-time) 34) do
         (decf (day-of local-time) 35)
         (incf (month-of local-time)))
    (loop while (> (month-of local-time) 15) do
         (decf (hour-of local-time) 16)
         (incf (year-of local-time)))
    local-time))

(defun reset-zone-weather ()
  (dolist (zone *zone-table*)
    (let* ((local-time (local-time-of zone))
           (pressure (if (and (>= (month-of local-time) 7)
                              (<= (month-of local-time) 12))
                         (+ 960 (random 50))
                         (+ 960 (random 80)))))
           (setf (weather-of zone)
             (make-instance 'weather-data
                            :pressure pressure
                            :sunlight (cond
                                        ((<= (hour-of local-time) 4)  :dark)
                                        ((= (hour-of local-time) 5)   :rise)
                                        ((<= (hour-of local-time) 20) :light)
                                        ((= (hour-of local-time) 21)  :set)
                                        (t                             :dark))
                            :sky (cond
                                   ((<= pressure 980) :lightning)
                                   ((<= pressure 1000) :raining)
                                   ((<= pressure 1020) :cloudy)
                                   (t                               :cloudless))))))
    (slog "Zone weather set."))