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
               xmls
               #+sbcl sb-bsd-sockets
               #+sbcl sb-posix)
	
  :components
  ((:module :src :components
            ((:file "defpackage" :pathname "util/defpackage")
             (:file "defs" :pathname "util/defs"
                           :depends-on ("defpackage"))
             (:file "db" :pathname "db/db"
                         :depends-on ("defs" "creature" "obj-data" "zone-data" "room-data" "search" "artisan" "account" "spec-assign" "clan" "help" "act-social" "tongues" "act-physic" "dyntext" "spell-parser" "combat-messages" "quest" "artisan" "utils"))
             (:file "account" :pathname "db/account"
                              :depends-on ("defs" "utils"))
             (:file "accstr" :pathname "util/accstr"
                             :depends-on ("defs"))
             (:file "act-informative" :pathname "misc/act-informative"
                                 :depends-on ("defs" "utils"))
             (:file "act-physic" :pathname "classes/act-physic"
                                 :depends-on ("defs" "utils"))
             (:file "act-social" :pathname "social/act-social"
                                 :depends-on ("defs" "utils"))
             (:file "artisan" :pathname "mobiles/artisan"
                              :depends-on ("creature"))
             (:file "ban" :pathname "net/ban"
                          :depends-on ("defs" "utils"))
             (:file "constants" :pathname "db/constants")
             (:file "clan" :pathname "clan/clan"
                           :depends-on ("defs" "utils"))
             (:file "combat-messages" :pathname "combat/combat-messages"
                                      :depends-on ("defs" "utils"))
             (:file "comm" :pathname "net/comm"
                           :depends-on ("defs" "utils" "network" "nanny"))
             (:file "creature" :pathname "structs/creature"
                               :depends-on ("defs"))
             (:file "creature-io" :pathname "structs/creature-io"
                               :depends-on ("creature"))
             (:file "dyntext" :pathname "dyntext/dyntext"
                              :depends-on ("defs" "utils"))
             (:file "groups" :pathname "interpreter/groups"
                              :depends-on ("defs" "utils"))
             (:file "handler" :pathname "util/handler"
                            :depends-on ("defs"))
             (:file "help" :pathname "help/help"
                           :depends-on ("defs" "utils"))
             (:file "house" :pathname "house/house"
                            :depends-on ("defs" "utils"))
             (:file "interpreter" :pathname "interpreter/interpreter"
                              :depends-on ("defs" "utils"))
             (:file "nanny" :pathname "net/nanny"
                            :depends-on ("defs" "utils" "network"))
             (:file "network" :pathname "net/network"
                              :depends-on ("defs" "utils"))
             (:file "obj-data" :pathname "structs/obj-data"
                               :depends-on ("defs"))
             (:file "paths" :pathname "objects/paths"
                            :depends-on ("defs" "utils"))
             (:file "prog-compile" :pathname "search/prog-compile"
                                   :depends-on ("defs" "utils"))
             (:file "quest" :pathname "quest/quest"
                            :depends-on ("defs" "utils"))
             (:file "random" :pathname "util/random"
                                :depends-on ("defs"))
             (:file "room-data" :pathname "structs/room-data"
                                :depends-on ("defs"))
             (:file "search" :pathname "structs/search"
                             :depends-on ("defs"))
             (:file "sight" :pathname "util/sight"
                            :depends-on ("defs"))
             (:file "spec-assign" :pathname "specials/spec-assign"
                                  :depends-on ("defs" "utils"))
             (:file "spec-procs" :pathname "specials/spec-procs"
                                 :depends-on ("defs" "utils"))
             (:file "spell-parser" :pathname "magic/spell-parser"
                                   :depends-on ("defs" "utils"))
             (:file "structs" :pathname "structs/structs"
                              :depends-on ("defs"))
             (:file "tmpstr" :pathname "util/tmpstr"
                             :depends-on ("defs"))
             (:file "tongues" :pathname "social/tongues"
                              :depends-on ("defs" "utils"))
             (:file "utils" :pathname "util/utils"
                            :depends-on ("defs"))
             (:file "weather" :pathname "util/weather"
                              :depends-on ("zone-data"))
             (:file "zone-data" :pathname "structs/zone-data"
                                :depends-on ("defs"))))))
