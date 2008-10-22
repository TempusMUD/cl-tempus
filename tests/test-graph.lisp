(in-package #:tempus.tests)

(in-suite (defsuite (tempus.graph :in test)))

(deftest mark/normal/marks-room ()
  (let ((room (tempus::real-room 3013)))
    (setf (tempus::find-path-index-of room) 0)
    (tempus::mark room)
    (is (= tempus::*find-path-index* (tempus::find-path-index-of room)))))

(deftest unmark/normal/marks-room ()
  (let ((room (tempus::real-room 3013)))
    (setf (tempus::find-path-index-of room) tempus::*find-path-index*)
    (tempus::unmark room)
    (is (= 0 (tempus::find-path-index-of room)))))

(deftest markedp/marked-room/returns-t ()
  (let ((room (tempus::real-room 3013)))
    (setf (tempus::find-path-index-of room) tempus::*find-path-index*)
    (tempus::mark room)
    (is (tempus::markedp room))))

(deftest markedp/unmarked-room/returns-nil ()
  (let ((room (tempus::real-room 3013)))
    (setf (tempus::find-path-index-of room) tempus::*find-path-index*)
    (tempus::unmark room)
    (is (not (tempus::markedp room)))))

(deftest to-room/normal/returns-room ()
  (let ((room (tempus::real-room 3013)))
    (is (eql (tempus::real-room 3002)
             (tempus::to-room room tempus::+south+)))))

(deftest to-room/no-exit/returns-nil ()
  (let ((room (tempus::real-room 3013)))
    (is (null (tempus::to-room room tempus::+down+)))))

(deftest valid-edge-p/valid-edge/returns-t ()
  (let ((room (tempus::real-room 3013)))
    (tempus::unmark (tempus::to-room room tempus::+north+))
    (is (tempus::valid-edge-p room tempus::+north+ :god-mode))))

(deftest valid-edge-p/no-exit/returns-nil ()
  (let ((room (tempus::real-room 3013)))
    (is (not (tempus::valid-edge-p room tempus::+down+ :god-mode)))))

(deftest valid-edge-p/to-marked-room/returns-nil ()
  (let ((room (tempus::real-room 3013)))
    (tempus::mark (tempus::real-room 3002))
    (is (not (tempus::valid-edge-p room tempus::+south+ :god-mode)))))

(deftest bfs-enqueue/normal/new-element-in-queue ()
  (let ((room (tempus::real-room 3013)))
    (let ((tempus::*find-path-queue* nil))
      (tempus::bfs-enqueue room tempus::+south+)
      (is (= 1 (length tempus::*find-path-queue*)))
      (is (eql room (tempus::bfs-queue-element-room
                     (first tempus::*find-path-queue*))))
      (is (eql tempus::+south+
               (tempus::bfs-queue-element-dir
                (first tempus::*find-path-queue*)))))))

(deftest bfs-dequeue/normal/pops-element-off-end ()
  (let ((room (tempus::real-room 3013)))
    (let ((tempus::*find-path-queue* nil))
      (tempus::bfs-enqueue room tempus::+south+)
      (let ((elem (tempus::bfs-dequeue)))
        (is (null tempus::*find-path-queue*))
        (is (eql room (tempus::bfs-queue-element-room elem)))
        (is (eql tempus::+south+ (tempus::bfs-queue-element-dir elem)))))))

(deftest find-first-step/same-room/returns-nil ()
  (let ((room (tempus::real-room 3013)))
    (is (null (tempus::find-first-step room room :god-mode)))))

(deftest find-first-step/one-room-away/returns-south ()
  (let ((start (tempus::real-room 3013))
        (end (tempus::real-room 3002)))
    (is (= tempus::+south+ (tempus::find-first-step start end :god-mode)))))

(deftest find-first-step/forty-rooms-away/returns-east ()
  (let ((start (tempus::real-room 3013))
        (end (tempus::real-room 24800)))
    (is (= tempus::+east+ (tempus::find-first-step start end :god-mode)))))

(deftest find-distance/same-room/returns-zero ()
  (let ((room (tempus::real-room 3013)))
    (is (zerop (tempus::find-distance room room)))))

(deftest find-distance/one-room-away/returns-one ()
  (let ((start (tempus::real-room 3013))
        (end (tempus::real-room 3002)))
    (is (= 1 (tempus::find-distance start end)))))

(deftest find-distance/forty-rooms-away/returns-forty ()
  (let ((start (tempus::real-room 3013))
        (end (tempus::real-room 24800)))
    (is (= 40 (tempus::find-distance start end)))))