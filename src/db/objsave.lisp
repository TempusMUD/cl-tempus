(in-package #:tempus)

(defvar *min-rent-cost* 1000)


(defun get-cost-modifier (ch seller)
  "Returns a number from -44 to 44, indicating the percentage that should be added or removed from a transaction involving the two creatures."
  (* (- (cha-of seller) (cha-of ch)) 2))

(defun find-all-objects (obj-list)
  "Returns a list of all the objects in obj-list, including all the objects contained by other objects."
  (append obj-list
          (mapcar (lambda (obj)
                    (find-all-objects (contents-of obj)))
                  obj-list)))

(defun is-unrentable (obj)
  nil)

(defun send-rent-line (ch obj count currency-str)
  (send-to-char ch "~10d ~a for ~a~:[~; (x~d)~%"
                (cost-per-day-of obj)
                currency-str
                (name-of obj)
                (= count 1)
                count))

(defun tally-obj-rent (ch obj-list currency-str displayp)
  (let ((last-obj nil)
        (total-cost 0)
        (count 1))
    (dolist (obj (sort obj-list #'< :key 'vnum-of))
      (unless (is-unrentable obj)
        (incf total-cost (cost-per-day-of obj))
        (cond
          ((and (not (eql (shared-of last-obj) (shared-of obj))) displayp)
           (send-rent-line ch obj count currency-str)
           (setf count 1)
           (setf last-obj obj))
          (t
           (incf count)))))
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

  (let* ((total-cost (tally-obj-rent ch
                                     (find-all-objects (append (coerce (equipment-of ch) 'list)
                                                               (carrying-of ch)))
                                     currency-str displayp))
         (level-adj (+ (/ (* 3 total-cost (+ 10 (level-of ch))) 100)
                       (* *min-rent-cost* (level-of ch))
                       (- total-cost))))
    (setf total-cost (* (+ total-cost level-adj) factor))

    (when displayp
      (send-to-char ch "~10d ~a for level adjustment~%" level-adj currency-str)
      (unless (= factor 1)
        (send-to-char "        x%2f for services~%" factor))
      (send-to-char ch "-------------------------------------------~%")
      (send-to-char ch "~10d ~a TOTAL~%" total-cost currency-str))

    total-cost))