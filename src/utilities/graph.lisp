(in-package #:tempus)

(defstruct bfs-queue-element
  room dir)

(defparameter +find-path-index-reset+ 255)
(defvar *find-path-index* +find-path-index-reset+)
(defvar *find-path-head* nil)
(defvar *find-path-tail* nil)

(defun mark (room)
  (setf (find-path-index-of room) *find-path-index*))
(defun unmark (room)
  (setf (find-path-index-of room) 0))
(defun markedp (room)
  (= (find-path-index-of room) *find-path-index*))

(defun to-room (room dir)
  (when (aref (dir-option-of room) dir)
    (when (to-room-of (aref (dir-option-of room) dir))
      (real-room (to-room-of (aref (dir-option-of room) dir))))))

(defun valid-edge-p (room dir mode)
  (let ((dest (to-room room dir)))
    (and dest
         (not (markedp dest))
         (not (and (eql mode :psi-track)
                   (room-flagged dest +room-nopsionics+)))
         (or (not (eql mode :god-track))
             (not (or (room-flagged dest +room-notrack+)
                      (room-flagged dest +room-death+)))))))

(defun bfs-enqueue (room dir)
  (cond
    (*find-path-head*
     (setf (cdr *find-path-tail*)
           (cons (make-bfs-queue-element :room room :dir dir) nil))
     (setf *find-path-tail* (cdr *find-path-tail*)))
    (t
     (setf *find-path-head*
           (list (make-bfs-queue-element :room room :dir dir)))
     (setf *find-path-tail* *find-path-head*))))

(defun bfs-dequeue ()
  (pop *find-path-head*)
  (unless *find-path-head*
    (setf *find-path-tail* nil)))

(defun bfs-clear-queue ()
  (setf *find-path-head* nil)
  (setf *find-path-tail* nil))

(defun find-first-step (src target mode)
  (assert src nil "Illegal value SRC passed to find-first-step")
  (assert target nil "Illegal value TARGET passed to find-first-step")
  (when (eql src target)
    (return-from find-first-step nil))

  (bfs-clear-queue)
  (decf *find-path-index*)

  (when (zerop *find-path-index*)
    (setf *find-path-index* +find-path-index-reset+)
    (loop for zone in *zone-table* do
         (loop for room in (world-of zone) do
              (unmark room))))

  (mark src)

  (loop for dir from 0 upto (1- +num-dirs+)
       when (valid-edge-p src dir mode)
       do (bfs-enqueue (to-room src dir) dir))

  ;; Now, do the classic BFS
  (loop
     while *find-path-head*
     for elem = (car *find-path-head*)
     as elem-room = (bfs-queue-element-room elem)
     as elem-dir = (bfs-queue-element-dir elem)
     do (if (eql elem-room target)
            (return-from find-first-step elem-dir)
            (loop for dir from 0 upto (1- +num-dirs+)
               when (valid-edge-p elem-room dir mode)
               do (mark (to-room elem-room dir))
                 (bfs-enqueue (to-room elem-room dir) elem-dir)
               finally (bfs-dequeue))))
  ;; return nil if not found
  nil)

(defun find-distance-recurse (current dest distance)
  (if (eql current dest)
      distance
      (let ((dir (find-first-step current dest :god-mode)))
        (when dir
          (find-distance-recurse (to-room current dir)
                                 dest
                                 (1+ distance))))))

(defun find-distance (start dest)
  (find-distance-recurse start dest 0))

(defun hunt-victim (ch)
  "TODO: Implement hunt-victim"
  nil)

(defun smart-mobile-move (ch dir)
  (let* ((exit (exit ch dir))
         (room (and exit (real-room (to-room-of exit)))))
    (cond
      ((or (null exit) (null room))
       nil)
      ((logtest (exit-info-of exit) +ex-closed+)
       (cond
         ((logtest (exit-info-of exit) +ex-special+)
          ;; can't open here
          nil)
         ((logtest (exit-info-of exit) +ex-locked+)
          (cond
            ((find-key ch (key-of exit))
             (perform-door-unlock ch dir))
            ((and (> (check-skill ch +skill-pick-lock+) 30)
                  (find-lockpick ch))
             (perform-door-pick ch dir))
            ((and (> (check-skill ch +spell-knock+) 30)
                  (> (mana-of ch) (/ (max-mana-of ch) 2)))
             (cast-spell ch nil nil dir +spell-knock+))
            ((and (> (check-skill ch +skill-break-door+) 30)
                  (> (hitp-of ch) (/ (max-hitp-of ch) 2)))
             (perform-door-bash ch dir))))
         (t
          (perform-door-open ch dir))))
      ((and (room-is-open-air room)
            (/= (position-of ch) +pos-flying+))
       (cond
         ((can-travel-terrain ch (terrain-of room))
          (perform-fly ch))
         ((> (check-skill ch +spell-fly+) 30)
          (cast-spell ch ch nil nil +spell-fly+))
         ((> (check-skill ch +spell-air-walk+) 30)
          (cast-spell ch ch nil nil +spell-air-walk+))
         ((> (check-skill ch +spell-tidal-spacewarp+) 30)
          (cast-spell ch ch nil nil +spell-tidal-spacewarp+))
         ((randomly-true 10)
          (emit-voice ch nil +voice-hunt-openair+))))
      ((and (eql (terrain-of room)
                 +sect-water-noswim+)
            (/= (position-of ch)
                +pos-flying+)
            (can-travel-terrain ch (terrain-of room)))
       (cond
         ((aff-flagged ch +aff-inflight+)
          (perform-fly ch))
         ((> (check-skill ch +spell-waterwalk+)
             30)
          (cast-spell ch ch nil nil +spell-waterwalk+))
         ((randomly-true 10)
          (emit-voice ch nil +voice-hunt-water+))))
      (t
       (perform-move ch dir :walk t)))))