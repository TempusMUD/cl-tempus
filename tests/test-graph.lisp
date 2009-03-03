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
  (let ((room (tempus::real-room 100)))
    (is (eql (tempus::real-room 101)
             (tempus::to-room room tempus::+east+)))))

(deftest to-room/no-exit/returns-nil ()
  (let ((room (tempus::real-room 100)))
    (is (null (tempus::to-room room tempus::+south+)))))

(deftest valid-edge-p/valid-edge/returns-t ()
  (let ((room (tempus::real-room 100)))
    (tempus::unmark (tempus::to-room room tempus::+east+))
    (is (tempus::valid-edge-p room tempus::+east+ :god-mode))))

(deftest valid-edge-p/no-exit/returns-nil ()
  (let ((room (tempus::real-room 100)))
    (is (not (tempus::valid-edge-p room tempus::+south+ :god-mode)))))

(deftest valid-edge-p/to-marked-room/returns-nil ()
  (let ((room (tempus::real-room 100)))
    (tempus::mark (tempus::real-room 101))
    (is (not (tempus::valid-edge-p room tempus::+east+ :god-mode)))))

(deftest bfs-enqueue/normal/new-element-in-queue ()
  (let ((room (tempus::real-room 100)))
    (let ((tempus::*find-path-head* nil))
      (tempus::bfs-enqueue room tempus::+south+)
      (is (= 1 (length tempus::*find-path-head*)))
      (is (eql room (tempus::bfs-queue-element-room
                     (first tempus::*find-path-head*))))
      (is (eql tempus::+south+
               (tempus::bfs-queue-element-dir
                (first tempus::*find-path-head*)))))))

(deftest bfs-dequeue/normal/pops-element-off-end ()
  (let ((room (tempus::real-room 100)))
    (let ((tempus::*find-path-head* nil))
      (tempus::bfs-enqueue room tempus::+south+)
      (tempus::bfs-dequeue)
      (is (null tempus::*find-path-head*)))))

(deftest find-first-step/same-room/returns-nil ()
  (let ((room (tempus::real-room 101)))
    (is (null (tempus::find-first-step room room :god-mode)))))

(deftest find-first-step/one-room-away/returns-south ()
  (let ((start (tempus::real-room 101))
        (end (tempus::real-room 100)))
    (is (= tempus::+west+ (tempus::find-first-step start end :god-mode)))))

(deftest find-first-step/forty-rooms-away/returns-east ()
  (let ((start (tempus::real-room 100))
        (end (tempus::real-room 24800)))
    (is (= tempus::+down+ (tempus::find-first-step start end :god-mode)))))

(deftest find-distance/same-room/returns-zero ()
  (let ((room (tempus::real-room 100)))
    (is (zerop (tempus::find-distance room room)))))

(deftest find-distance/one-room-away/returns-one ()
  (let ((start (tempus::real-room 101))
        (end (tempus::real-room 100)))
    (is (= 1 (tempus::find-distance start end)))))

(deftest find-distance/37-rooms-away/returns-37 ()
  (let ((start (tempus::real-room 100))
        (end (tempus::real-room 24800)))
    (is (= 37 (tempus::find-distance start end)))))