(in-package #:tempus.tests)

(in-suite (defsuite (tempus.misc :in test)))

(tempus::enable-regex-reader-syntax)

(deftest feedback-command ()
  (let ((path (tempus::tempus-path "lib/misc/ideas"))
        (real-path (tempus::tempus-path "lib/misc/ideas.real")))
    (with-mock-players (alice)
      (unwind-protect
           (progn
             (when (probe-file path)
               (rename-file path real-path))
             (tempus::interpret-command alice "idea I have an idea!")
             (is (equal "Your idea has been recorded.  Thanks!~%" (char-output alice)))
             (with-open-file (inf path)
               (loop
                  for read-line = (read-line inf nil nil)
                  as last-line = (if read-line read-line last-line)
                  while read-line
                  finally (is (cl-ppcre:scan #/^Alice    \(... ..\) \[ 3013\] I have an idea!$/ last-line)))))
        (when (probe-file real-path)
          (rename-file real-path path))))))