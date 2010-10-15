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
(defvar *jet-stream-state* t)

;; This is an associative array of weather change information.  The
;; key element is the return value from ZONE-WEATHER-CHANGE.  The
;; second element is the value to set the zone's sky to.  The rest of
;; the values are, in order, random-emit 1, random-emit 2, sunrise
;; emit, sunset emit, night emit
(defparameter *weather-change-emits*
  '((:clouds-start :cloudy
     "The sky starts to get cloudy."
     "Clouds begin to gather in the sky."
     "A veil of clouds appears with the sunrise."
     "As the suns dip below the horizon, clouds cover the sky."
     "A layer of clouds obscures the stars.")
    (:rain-start :raining
     "It starts to rain."
     "Rain begins to fall from the sky."
     "Rain begins to fall across the land, as the double suns rise."
     "AS the suns sink below the horizon, rain begins to fall."
     "Rain begins to fall from the night sky.")
    (:storm-start :storming
     "Lightning starts to show in the sky."
     "You can see lightning flickering across the sky."
     "Lightning and thunder begin to accompany the sunrise."
     "As the suns set, lightning begins to flicker across the sky."
     "The starry sky is lit by flashes of lightning.")
    (:storm-stop :raining
     "The lightning stops."
     "The lightning seems to have stopped."
     "The lightning ceases as the suns rise on a rainy day."
     "As the suns disappear, the lightning ceases."
     "The lightning and thunder cease.")
    (:rain-stop :cloudy
     "The rain stops."
     "The rain slacks up, then stops."
     "The rain ceases as the twin suns rise above the horizon."
     "The rain ceases as the twin suns sink below the horizon."
     "The rain stops falling from the night sky.")
    (:clouds-stop :clear
     "The clouds disappear."
     "The clouds have vanished, leaving a clear sky."
     "As the suns rise, the clouds in the sky vanish."
     "The last of the clouds in the sky vanish with the sunset."
     "The clouds part, leaving a clear view of the stars.")))

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

(defun update-zone-sunlight (zone hour day month)
  (cond
    ((= hour (- 4 (aref +daylight-modifiers+ month)))
     (cond
       ((zerop (random-range 0 1))
        (send-to-outside zone "A small blue sun rises in the east.~%"))
       ((eql (sky-of (weather-of zone)) :clear)
        (send-to-outside zone "The blue sun rises in the cloudless skies of the east.~%"))
       ((eql (sky-of (weather-of zone)) :cloudy)
        (send-to-outside zone "Through the veil of clouds, you see a glimmer of blue to the east.~%"))
       ((eql (sky-of (weather-of zone)) :raining)
        (send-to-outside zone "The faint light of the blue sun appears through the rain to the east.~%"))
       (t
        (send-to-outside zone "A small blue sun rises in the east.~%"))))
    ((= hour (- 5 (aref +daylight-modifiers+ month)))
     (setf (sunlight-of (weather-of zone)) :rise)
     (cond
       ((zerop (random-range 0 1))
        (send-to-outside zone "The huge red sun rises over the eastern horizon.~%"))
       ((eql (sky-of (weather-of zone)) :clear)
        (send-to-outside zone "The blazing red giant rises into the clear sky.~%"))
       ((eql (sky-of (weather-of zone)) :cloudy)
        (send-to-outside zone "The red light of the sun appears in the cloudy sky.~%"))
       (t
        (send-to-outside zone "The huge red sun rises over the eastern horizon.~%"))))
    ((= hour (- 6 (aref +daylight-modifiers+ month)))
     (setf (sunlight-of (weather-of zone)) :light)
     (cond
       ((zerop (random-range 0 1))
        (send-to-outside zone "The day has begun, both suns above the horizon.~%"))
       ((eql (sky-of (weather-of zone)) :clear)
        (send-to-outside zone "The day begins under a cloudless sky.~%"))
       ((eql (sky-of (weather-of zone)) :cloudy)
        (send-to-outside zone "The cloudy day has begun.~%"))
       ((eql (sky-of (weather-of zone)) :raining)
        (send-to-outside zone "The rainy day has begun.~%"))
       (t
        (send-to-outside zone "The day has begun, both suns above the horizon.~%")))

     (when (zerop day)
       (case month
         (2
          (send-to-outside zone "It is the first day of spring.~%"))
         (6
          (send-to-outside zone "It is the first day of summer.~%"))
         (10
          (send-to-outside zone "It is the first day of autumn.~%"))
         (14
          (send-to-outside zone "It is the first day of winter.~%")))))
    ((= hour 12)
     (send-to-outside zone "The red giant is high overhead now, at noon.~%"))
    ((= hour (+ 20 (aref +daylight-modifiers+ month)))
     (setf (sunlight-of (weather-of zone)) :sunset)
     (send-to-outside zone "The red giant sun slowly sets in the west.~%"))
    ((= hour (+ 21 (aref +daylight-modifiers+ month)))
     (setf (sunlight-of (weather-of zone)) :sunset)
     (send-to-outside zone "The red giant sun slowly sets in the west.~%"))
    ((= hour (+ 22 (aref +daylight-modifiers+ month)))
     (setf (sunlight-of (weather-of zone)) :sunset)
     (send-to-outside zone "The night has begun.~%"))))

(defun update-zone-moonlight (zone hour)
  (let ((phase (lunar-phase *lunar-day*)))
    (unless (eql phase :new)
      (let ((lunar-rise-time (mod (+ *lunar-day* 5) 24)))
        (cond
          ((= hour lunar-rise-time)
           (setf (moonlight-of (weather-of zone)) :rising)
           (unless (or (zerop *lunar-day*)
                       (not (eql (sky-of (weather-of zone)) :clear)))
             (send-to-outside zone "The ~a moon rises in the east.~%" (aref +lunar-phases+ phase))))
          ((or (= hour (+ lunar-rise-time 1))
               (= hour (- lunar-rise-time 23)))
           (setf (moonlight-of (weather-of zone)) :east))
          ((or (= hour (+ lunar-rise-time 7))
               (= hour (- lunar-rise-time 17)))
           (setf (moonlight-of (weather-of zone)) :high)
           (unless (or (zerop *lunar-day*)
                       (not (eql (sky-of (weather-of zone)) :clear)))
             (send-to-outside zone "The ~a moon is directly overhead.~%" (aref +lunar-phases+ phase))))
          ((or (= hour (+ lunar-rise-time 8))
               (= hour (- lunar-rise-time 16)))
           (setf (moonlight-of (weather-of zone)) :setting)
           (unless (or (zerop *lunar-day*)
                       (not (eql (sky-of (weather-of zone)) :clear)))
             (send-to-outside zone "The ~a moon begins to sink low in the west.~%" (aref +lunar-phases+ phase))))
          ((or (= hour (+ lunar-rise-time 14))
               (= hour (- lunar-rise-time 10)))
           (setf (moonlight-of (weather-of zone)) :none)
           (unless (or (zerop *lunar-day*)
                       (not (eql (sky-of (weather-of zone)) :clear)))
             (send-to-outside zone "The ~a moon sets in the west.~%" (aref +lunar-phases+ phase)))))))))

(defun another-hour ()
  (incf (hour-of *time-info*))
  (let ((old-lunar-phase (lunar-phase *lunar-day*)))

    (when (> (hour-of *time-info*) 23)
      (decf (hour-of *time-info*) 24)
      (incf (day-of *time-info*))
      (setf *lunar-day* (mod (1+ *lunar-day*) 24))

      (let ((new-lunar-phase (lunar-phase *lunar-day*)))
        (unless (eql old-lunar-phase new-lunar-phase)
          (case new-lunar-phase
            (:full
             (send-to-clerics :evil "The vile light of the full moon dampens your magic power.~%")
             (send-to-clerics :good "The blessed light of the full moon fills you with strength.~%"))
            (:wane-gibbous
             (send-to-clerics :evil "Your magic strengthens as the blasphemous light of the full moon passes.~%")
             (send-to-clerics :good "The blessing of the full moon leaves you as the moon wanes.~%"))
            (:new
             (send-to-clerics :evil "The unholy strength of the new moon fills you.~%")
             (send-to-clerics :good "The cold darkness of the new moon drains your strength.~%"))
            (:wax-crescent
             (send-to-clerics :evil "Your unholy strength wanes with the passing of the new moon.~%")
             (send-to-clerics :good "You feel your strength return as the new moon passes.~%")))))

      (when (> (day-of *time-info*) 34)
        (decf (day-of *time-info*) 35)
        (incf (month-of *time-info*)))

      (when (> (month-of *time-info*) 15)
        (decf (month-of *time-info*) 16)
        (incf (year-of *time-info*))))

    (dolist (zone *zone-table*)
      (unless (or (null (world-of zone))
                  (> (plane-of zone) +max-prime-plane+)
                  (= (time-frame-of zone) +time-timeless+)
                  (= (plane-of zone) +plane-underdark+))
        (multiple-value-bind (hour day month year)
            (local-time-of zone)
          (declare (ignore year))
          (update-zone-sunlight zone hour day month)
          (update-zone-moonlight zone hour))))))

(defun jet-stream ()
  (loop for hour from 2 downto -4 do
       (loop for zone in *zone-table*
            when (= (hour-mod-of zone) hour) do
            (incf (change-of (weather-of zone))
                  (loop
                     with count = 0
                     with change = 0
                     for fr-zone in *zone-table*
                     unless (or (/= (hour-mod-of fr-zone) (1- hour))
                                (> (latitude-of fr-zone) (1+ (latitude-of zone)))
                                (< (latitude-of fr-zone) (1- (latitude-of zone))))
                     do
                       (incf count)
                       (incf change (- (floor (+ (pressure-of (weather-of fr-zone))
                                               (pressure-of (weather-of zone)))
                                            2)
                                     (pressure-of (weather-of zone))))
                     finally (return (if count (floor change count) 0)))))))

(defun zone-weather-change (zone)
  (let ((pressure (pressure-of (weather-of zone))))
    (case (sky-of (weather-of zone))
      (:clear
       (when (or (< pressure 990)
                 (and (< pressure 1010) (zerop (random-range 0 3))))
         :clouds-start))
      (:cloudy
       (cond
         ((< pressure 970)
          :raining)
         ((< pressure 990)
          (if (zerop (random-range 0 3))
              :rain-start
              :clear))
         ((and (> pressure 1030) (zerop (random-range 0 3)))
          :storm-start)))
      (:raining
       (cond
         ((< pressure 970)
          (if (zerop (random-range 0 3))
              :storm-start
              :clear))
         ((> pressure 1030)
          :rain-stop)
         ((> pressure 1010)
          (when (zerop (random-range 0 3))
            :rain-stop))))
      (:storming
       (when (or (> pressure 1010)
                 (and (> pressure 990) (zerop (random-range 0 3))))
         :storm-stop))
      (t
       (setf (sky-of (weather-of zone)) :clear)))))

(defun zone-weather-update (zone)
  (unless (zone-flagged zone +zone-noweather+)
    (let ((multiplier (if (<= 9 (nth-value 2 (local-time-of zone)) 16)
                          (cond
                            ((> (pressure-of (weather-of zone)) 1000) -1)
                            ((> (pressure-of (weather-of zone)) 970) 1)
                            (t 0))
                          (cond
                            ((> (pressure-of (weather-of zone)) 1025) -1)
                            ((> (pressure-of (weather-of zone)) 990) 1)
                            (t 0)))))
      (setf (change-of (weather-of zone))
            (pin (+ (change-of (weather-of zone))
                    (* multiplier (dice 4 4) (random-range -15 15))) -15 15))

      (setf (pressure-of (weather-of zone)) (pin (+ (pressure-of (weather-of zone))
                                                    (change-of (weather-of zone)))
                                                 960 1040)))

    (let* ((info (assoc (zone-weather-change zone) *weather-change-emits*)))
      (when info
        (setf (sky-of (weather-of zone)) (second info))
        (send-to-outside zone "~a~%"
                      (cond
                        ((zerop (random-range 0 2))
                         (third info))
                        ((eql (sunlight-of (weather-of zone)) :rise)
                         (fourth info))
                        ((eql (sunlight-of (weather-of zone)) :set)
                         (fifth info))
                        ((eql (sunlight-of (weather-of zone)) :dark)
                         (sixth info))
                        (t
                         (seventh info))))))))

(defun weather-change ()
  (when *jet-stream-state*
    (jet-stream))
  (dolist (zone *zone-table*)
    (zone-weather-update zone)
    (zone-weather-change zone)))

(defun weather-and-time ()
  (another-hour)
  (weather-change))