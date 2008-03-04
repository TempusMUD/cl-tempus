(in-package :tempus)

(defun interpret-scan-directive (stream format-idx format)
  (case (char format format-idx)
    (#\s
     (if (< (length format) (1+ format-idx))
         (with-output-to-string (result)
           (loop for c = (read-char stream nil nil)
                 until (or (null c) (char= c (char format (1+ format-idx)))) do
                 (princ c result)))))))

#+nil (defun scan (stream format)
  (let ((values nil))
    (loop with format-idx = 0
          until (> format-idx (1- (length format))) do
          (cond
            ((char= (char format format-idx) #\~)
             ;; Interpret format directive
             (incf format-idx)
             (let ((val (interpret-scan-directive stream format-idx format)))
               (if val
                   (push values val)
                   (return-from scan nil)))
             (incf format-idx))
            ((char= (char format format-idx) #\space)
             ;; space characters match any number of any space chars
             (when (graphic-char-p (read-char stream nil nil))
               (incf format-idx)))
            ((not (char= c (char format format-idx)))
             ;; Didn't match
             (return-from scan nil))
            (t
             ;; literal match
             (incf format-idx)))
    (values-list values))))
           
(defun get-line (inf)
  (loop for line = (read-line inf nil nil)
        for count from 0
        while (and line (string/= line "") (char= (char line 0) #\*))
        finally (return (values line count))))

(defun mlog (message &key group level type write-to-file)
  (declare (ignorable group level type))
  (when write-to-file
    (let ((time-string (sb-int:format-universal-time nil (get-universal-time)
                                                     :style :short)))
      (format *standard-output* "~19a :: ~a~%" time-string message)
      (force-output)))

  (unless (and group level)
    (return-from mlog))
  
  ;; Write to the people online
  nil)

(defun errlog (fmt &rest args)
  (let ((msg (format nil "SYSERR: ~?" fmt args))
        (backtrace (format nil "~{TRACE: ~s~%~}"
                           (let ((backt (sb-debug:backtrace-as-list)))
                             (subseq backt 2 (min 12 (length backt)))))))
    (mlog msg :group "coder" :level +level-ambassador+ :level :normal :write-to-file t)
    (mlog backtrace :group "coder" :level +level-ambassador+ :level :normal :write-to-file t)))

(defun slog (fmt &rest args)
  (let ((msg (format nil "~?" fmt args)))
    (mlog msg :level :complete :write-to-file t)))

(defclass regex-result ()
  ((target :accessor target-of :initarg :target)
   (pattern :accessor pattern-of :initarg :pattern)
   (match-start :accessor match-start-of :initarg :match-start)
   (match-end :accessor match-end-of :initarg :match-end)
   (reg-starts :accessor reg-starts-of :initarg :reg-starts)
   (reg-ends :accessor reg-ends-of :initarg :reg-ends)))

(defun regref (result idx)
  (assert result (result) "NIL regex-result passed to regref")
  (cond
    ((zerop idx)
      (subseq (target-of result)
              (match-start-of result)
              (match-end-of result)))
    ((null (aref (reg-starts-of result) (1- idx)))
     nil)
    (t
      (subseq (target-of result)
              (aref (reg-starts-of result) (1- idx))
              (aref (reg-ends-of result) (1- idx))))))

(defun scan (regex target &key start end)
  (assert target nil "Target for SCAN is NIL")
  (multiple-value-bind (match-start match-end reg-starts reg-ends)
      (cl-ppcre:scan regex target :start (or start 0) :end (or end (length target)))
    (when match-start
      (make-instance 'regex-result
                     :target target
                     :pattern regex
                     :match-start match-start
                     :match-end match-end
                     :reg-starts reg-starts
                     :reg-ends reg-ends))))

(defun random-range (min max)
  (+ min (random max)))

(defun snarf-file (path)
  "Returns a string with the contents of the file at PATH."
  (with-open-file (inf path :direction :input)
    (let ((buf (make-string (file-length inf))))
      (read-sequence buf inf)
      buf)))

(defun mud-time-passed (t2 t1)
  "Calculate the MUD time passed over the last t2-t1 centuries (secs)"
  (let ((secs (- t2 t1)))
    (multiple-value-bind (years secs)
        (floor secs +secs-per-mud-year+)
      (multiple-value-bind (months secs)
          (floor secs +secs-per-mud-month+)
        (multiple-value-bind (days secs)
            (floor secs +secs-per-mud-day+)
          (multiple-value-bind (hours secs)
              (floor secs +secs-per-mud-hour+)
            (declare (ignore secs))
            (values hours days months years)))))))
