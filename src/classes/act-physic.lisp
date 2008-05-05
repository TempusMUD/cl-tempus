(in-package :tempus)

(defvar *timewarps* nil)

(defun boot-timewarp-data ()
  (handler-case
      (with-open-file (inf +timewarp-file+ :direction :input)
        (setf *timewarps*
              (loop for warp-rooms = (read inf nil)
                 while warp-rooms
                 collect warp-rooms))
        (slog "Timewarp data booted, ~d elements read." (length *timewarps*)))
    (error (err)
        (slog "Timewarp error: ~a" err))))