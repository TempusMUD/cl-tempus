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
(defvar *lunar-day* 0)

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
  (let ((hour (+ (hour-of *time-info*) (hour-mod-of zone)))
        (day (day-of *time-info*))
        (month (month-of *time-info*))
        (year (+ (year-of *time-info*) (year-mod-of zone))))
    (loop while (> hour 23) do
         (decf hour 24)
         (incf day))
    (loop while (< hour 0) do
         (incf hour 24)
         (decf day))
    (loop while (> day 34) do
         (decf day 35)
         (incf month))
    (loop while (> month 15) do
         (decf month 16)
         (incf year))
    (values hour day month year)))

(defun reset-zone-weather ()
  (dolist (zone *zone-table*)
    (multiple-value-bind (hour day month year) (local-time-of zone)
      (declare (ignore day year))
      (let* ((pressure (if (and (>= month 7)
                                (<= month 12))
                           (+ 960 (random 50))
                           (+ 960 (random 80)))))
        (setf (weather-of zone)
              (make-instance 'weather-data
                             :pressure pressure
                             :sunlight (cond
                                         ((<= hour 4)  :dark)
                                         ((= hour 5)   :rise)
                                         ((<= hour 20) :light)
                                         ((= hour 21)  :set)
                                         (t                             :dark))
                             :sky (cond
                                    ((<= pressure 980) :lightning)
                                    ((<= pressure 1000) :raining)
                                    ((<= pressure 1020) :cloudy)
                                    (t                               :cloudless)))))))
    (slog "Zone weather set."))