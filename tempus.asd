(defpackage #:tempus-system (:use #:asdf #:cl))
(in-package #:tempus-system)

(declaim (optimize (debug 3) (speed 0) (safety 3) (space 2)))

(defsystem tempus
  :name "Tempus"
  :version "1.0.d"
  :author "Daniel Lowe <dlowe@bitmuse.com>"
  :description "Tempus MUD Codebase"
  :depends-on (postmodern
               split-sequence
               local-time
               cl-ppcre
               uffi
               #+sbcl sb-bsd-sockets
               #+sbcl sb-posix)
	
  :components
  ((:module :src :components
            ((:file "db"
                    :pathname "db/db"
                    :depends-on ("defs"
                                 "creature"
                                 "obj-data"
                                 "zone-data"
                                 "room-data"
                                 "search"
                                 "artisan"
                                 "account"
                                 "spec-assign"
                                 "utils"))
             (:file "account" :pathname "db/account"
                    :depends-on ("defs"
                                 "utils"))
             (:file "spec-assign" :pathname "specials/spec-assign"
                    :depends-on ("defs"
                                 "utils"))
             (:file "comm" :pathname "net/comm"
                    :depends-on ("defs"
                                 "utils"))
             (:file "artisan" :pathname "mobiles/artisan"
                    :depends-on ("creature"))
             (:file "creature" :pathname "structs/creature" :depends-on ("defs"))
             (:file "obj-data" :pathname "structs/obj-data" :depends-on ("defs"))
             (:file "zone-data" :pathname "structs/zone-data" :depends-on ("defs"))
             (:file "room-data" :pathname "structs/room-data" :depends-on ("defs"))
             (:file "search" :pathname "structs/search" :depends-on ("defs"))
             (:file "structs" :pathname "structs/structs" :depends-on ("defs"))
             (:file "tmpstr" :pathname "util/tmpstr" :depends-on ("defs"))
             (:file "accstr" :pathname "util/accstr" :depends-on ("defs"))
             (:file "utils" :pathname "util/utils" :depends-on ("defs"))
             (:file "defs" :pathname "util/defs" :depends-on ("defpackage"))
             (:file "defpackage" :pathname "util/defpackage")))))
