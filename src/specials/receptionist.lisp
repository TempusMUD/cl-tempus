(in-package #:tempus)

(defvar *min-rent-cost* 1000)
(defconstant +rent-factor+ 1)
(defconstant +cryo-factor+ 4)

(defcommand (ch "rent") (:resting)
  (send-to-char ch "You can't do that here.~%"))
(defcommand (ch "cryo") (:resting)
  (send-to-char ch "You can't do that here.~%"))


(defun find-all-objects (obj-seq)
  "Returns a list of all the objects in OBJ-SEQ, including all the
objects contained by other objects."
  (let ((result nil))
    (map nil (lambda (obj)
               (push obj result)
               (setf result
                     (nconc (find-all-objects (contains-of obj))
                            result)))
         obj-seq)
    result))

(defun all-carried-objects (ch)
  "Returns a list of all objects that CH is carrying, wearing, or
implanted with, including objects in containers."
  (append
   (find-all-objects (remove nil (equipment-of ch)))
   (find-all-objects (remove nil (implants-of ch)))
   (find-all-objects (carrying-of ch))))

(defun unrentablep (obj)
  "Returns T if OBJ cannot be rented by a player."
  (or
   (is-obj-stat obj +item-norent+)
   (not (approvedp obj))
   (<= (vnum-of (shared-of obj)) -1)
   (and (is-obj-kind obj +item-key+)
        (zerop (obj-val-of obj 1)))
   (and (is-obj-kind obj +item-cigarette+)
        (not (zerop (obj-val-of obj 3))))))

(defun extract-unrentables (obj-list)
  "Given OBJ-LIST, removes all unrentable objects in OBJ-LIST that cannot be rented"
  (dolist (obj (copy-list obj-list))
    (extract-unrentables (contains-of obj))
    (when (and (unrentablep obj)
               (or (null (worn-by-of obj))
                   (not (is-obj-stat2 obj +item2-noremove+)))
               (not (is-obj-stat obj +item-nodrop+)))
      (extract-obj obj))))

(defun inform-rent-deadline (ch recep cost)
  (unless (zerop cost)
    (let ((msg (format nil "You can rent for ~d day~:p with the money you have on hand and in the bank."
                       (floor (if (eql (time-frame-of (zone-of (in-room-of ch))) +time-future+)
                                  (+ (cash-of ch)
                                     (future-bank-of (account-of ch)))
                                  (+ (gold-of ch)
                                     (past-bank-of (account-of ch))))
                              cost))))
      (if recep
          (perform-tell recep ch msg)
          (send-to-char "~a~%" msg)))))

(defun send-rent-line (ch obj count currency-str)
  (send-to-char ch "~10d ~a for ~a~:[ (x~d)~;~]~%"
                (cost-per-day-of (shared-of obj))
                currency-str
                (name-of obj)
                (= count 1)
                count))

(defun tally-obj-rent (ch obj-list currency-str displayp)
  (let ((last-obj (first obj-list))
        (total-cost 0)
        (count 1))
    (dolist (obj (sort (rest obj-list) #'< :key 'vnum-of))
      (unless (is-unrentable obj)
        (incf total-cost (cost-per-day-of (shared-of obj)))
        (when displayp
          (cond
            ((not (eql (shared-of last-obj) (shared-of obj)))
             (send-rent-line ch last-obj count currency-str)
             (setf count 1)
             (setf last-obj obj))
            (t
             (incf count))))))
    (when (and last-obj displayp)
      (send-rent-line ch last-obj count currency-str))
    total-cost))

(defun calc-daily-rent (ch factor currency-str displayp)
  (let* ((room (or (real-room (load-room-of ch))
                   (in-room-of ch)))
         (receptionist (and room
                            (find-if (lambda (tch)
                                       (and (is-npc tch)
                                            (or (eql (func-of (shared-of tch)) 'cryogenicist)
                                                (eql (func-of (shared-of tch)) 'receptionist))))
                                     (people-of room)))))
    (when receptionist
      (incf factor (* (get-cost-modifier ch receptionist) 100))))

  (let* ((total-cost (tally-obj-rent ch (all-carried-objects ch) currency-str displayp))
         (level-adj (floor (+ (/ (* 3 total-cost (+ 10 (level-of ch))) 100)
                              (* *min-rent-cost* (level-of ch))
                              (- total-cost)))))
    (setf total-cost (* (+ total-cost level-adj) factor))

    (when displayp
      (send-to-char ch "~10d ~a for level adjustment~%" level-adj currency-str)
      (unless (= factor 1)
        (send-to-char "        x%2f for services~%" factor))
      (send-to-char ch "-------------------------------------------~%")
      (send-to-char ch "~10d ~a TOTAL~%" total-cost currency-str))

    total-cost))

(defun display-unrentables (ch)
  (unless (immortal-level-p ch)
    (let ((forbid-renting nil))
      (dolist (obj (all-carried-objects ch))
        (when (unrentablep obj)
          (act ch :subject-emit (format nil "You cannot rent while ~a $p!"
                                        (cond
                                          ((null (worn-by-of obj)) "carrying")
                                          ((eql (get-eq ch (worn-on-of obj)) obj) "wearing")
                                          (t "implanted with"))))
          (setf forbid-renting t)))
      forbid-renting)))


(defun offer-rent (ch receptionist factor displayp)
  (with-pagination ((link-of ch))
    (when displayp
      (act receptionist :target ch
           :target-emit "$n writes up a bill and shows it to you:"))
    (let* ((currency (if (eql (time-frame-of (zone-of (in-room-of ch))) +time-future+)
                         "credits"
                         "coins"))
           (total-money (if (eql (time-frame-of (zone-of (in-room-of ch))) +time-future+)
                            (+ (cash-of ch)
                               (future-bank-of (account-of ch)))
                            (+ (gold-of ch)
                               (past-bank-of (account-of ch)))))
           (cost-per-day (calc-daily-rent ch factor currency displayp)))
      (unless (display-unrentables ch)
        (when displayp
          (when (eql factor +rent-factor+)
            (cond
              ((< total-money cost-per-day)
               (send-to-char ch "You don't have enough money to rent for a single day!~%"))
              ((plusp cost-per-day)
               (send-to-char ch "Your ~d ~a is enough to rent for &c~d&n day~:p.~%"
                             total-money currency (floor total-money cost-per-day))))))
        (floor cost-per-day)))))

(defun generic-receptionist (ch recep cmd factor)
  (let ((currency (if (eql (time-frame-of (zone-of (in-room-of ch))) +time-future+)
                      "credit"
                      "coin"))
        (money (if (eql (time-frame-of (zone-of (in-room-of ch))) +time-future+)
                   (cash-of ch)
                   (gold-of ch))))
    (cond
      ((not (member (first (command-info-pattern cmd))
                    '("rent" "offer")
                    :test #'string=))
       nil)
      ((not (awakep recep))
       (act ch :target recep :subject-emit "$E is unable to talk to you...")
       t)
      ((and (not (can-see-creature recep ch))
            (not (immortal-level-p ch)))
       (perform-say recep "say" "I don't deal with people I can't see!")
       t)
      ((string= (first (command-info-pattern cmd)) "rent")
       (let ((cost (offer-rent ch recep factor nil)))
         (perform-tell recep ch (if (eql factor +rent-factor+)
                                    (format nil "Rent will cost you ~d ~a~2:*~p per day." cost currency)
                                    (format nil "It will cost you ~d ~a~2:*p to be frozen" cost currency)))
         (cond
           ((> cost money)
            (perform-tell recep ch "...which I see you can't afford."))
           ((eql factor +rent-factor+)
            (rent-deadline ch recep cost)
            (act recep :target ch
                 :target-emit "$n stores your belongings and helps you into your private chamber."
                 :not-target-emit "$n helps $N into $S private chamber.")
            (setf (rentcode-of ch) 'rented)
            (setf (rent-per-day-of ch) cost)
            (setf (desc-mode-of ch) 'unknown)
            (setf (rent-currency-of ch) (if (eql (time-frame-of (zone-of (in-room-of ch)))
                                                 +time-future+)
                                            'cash 'gold))
            (setf (load-room-of ch) (number-of (in-room-of ch)))
            (save-player-to-xml ch)
            (extract-creature ch 'main-menu)
            (slog "~a has rented (~d/day, ~d ~a)" (name-of ch) cost money currency))
           (t
            (act recep :target ch
                 :target-emit "$n stores your belongings and helps you into your private chamber."
                 :not-target-emit "$n helps $N into $S private chamber.")
            (send-to-char ch "A white mist appears in the room, chilling you to the bone...~%")
            (send-to-char ch "You begin to lose consciousness...~%")
            (if (eql (time-frame-of (zone-of (in-room-of ch))) +time-future+)
                (decf (cash-of ch) cost)
                (decf (gold-of ch) cost))
            (setf (rentcode-of ch) 'cryo)
            (setf (rent-per-day-of ch) 0)
            (setf (rent-currency-of ch) (if (eql (time-frame-of (zone-of (in-room-of ch)))
                                                 +time-future+)
                                            'cash 'gold))
            (setf (load-room-of ch) (number-of (in-room-of ch)))
            (save-player-to-xml ch)
            (extract-creature ch 'main-menu)
            (slog "~a has cryo-rented" (name-of ch))))
         t))
      (t
       (offer-rent ch recep factor t)
       (act recep :target ch :not-target-emit "$n gives $N an offer.")
       t))))

(define-special receptionist (trigger self ch command vars) (+spec-mob+)
  (declare (ignore vars))
  (when (eql trigger 'command)
    (generic-receptionist ch self command +rent-factor+)))

(define-special cryogenicist (trigger self ch command vars) (+spec-mob+)
  (declare (ignore vars))
  (when (eql trigger 'command)
    (generic-receptionist ch self command +cryo-factor+)))