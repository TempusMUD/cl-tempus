(in-package #:tempus)

(defun apply-soil-to-char (ch obj kind pos)
  (when (eql pos +wear-random+)
    (setf pos (random-elt (loop
                             for idx upto +num-wears+
                             when (and (not (illegal-soilpos pos))
                                       (not (and (null (get-eq ch idx)) (char-soiled ch idx kind)))
                                       (not (and (get-eq ch idx) (obj-soiled (get-eq ch idx) kind))))
                             collect idx))))

  (unless (illegal-soilpos pos)
    (let ((eq (get-eq ch pos)))
      (cond
        ((and eq
              (or (eql eq obj) (null obj))
              (not (is-obj-stat2 eq +item2-nosoil+))
              (not (obj-soiled eq kind)))
         (setf (ldb (byte kind 1) (soilage-of eq)) 1))
        ((not (char-soiled ch pos kind))
         (setf (ldb (byte kind 1) (soilage-of ch)) 1)))))

  (when (and (eql kind +soil-blood+)
             obj
             (eql (vnum-of obj) +blood-vnum+))
    (incf (timer-of obj) (max 1 (- (timer-of obj) 5))))

  pos)