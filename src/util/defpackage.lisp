(defpackage #:tempus
    (:use :common-lisp
          :local-time
          :iolib)
    (:export main))

(shadowing-import
 #+sbcl '(sb-int:format-universal-time
          sb-unix:fd-set
          sb-unix:fd-zero
          sb-unix:fd-isset
          sb-unix:unix-read
          sb-unix:unix-write
          sb-unix:unix-fast-select
          sb-sys:vector-sap
          sb-unix::void-syscall
          sb-alien:struct
          sb-alien:with-alien
          sb-alien:addr
          sb-sys:ignore-interrupt
          sb-unix:SIGPIPE)

 #+cmu '(extensions:format-universal-time
         unix:fd-set
         unix:fd-zero
         unix:fd-isset
         unix:unix-read
         unix:unix-write
         unix:unix-fast-select
         system:vector-sap
         unix::void-syscall
         alien:struct
         alien:with-alien
         alien:addr
         system:ignore-interrupt
         unix:sigpipe)
 (find-package "TEMPUS"))
