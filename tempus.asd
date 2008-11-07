(defpackage #:tempus-system (:use #:asdf #:cl))
(in-package #:tempus-system)

#.(declaim (optimize (debug 3) (speed 0) (safety 3) (space 2)))

(defsystem :tempus
  :name "Tempus"
  :version "1.0.0d"
  :author "Daniel Lowe <dlowe@bitmuse.com>"
  :description "Tempus MUD Codebase"
  :depends-on (postmodern
               split-sequence
               asdf-system-connections
               local-time
               cl-ppcre
               cffi
               xmls
               #+sbcl sb-bsd-sockets
               #+sbcl sb-posix)

  :components
  ((:module :src :components
            ((:file "defpackage" :pathname "util/defpackage")
             (:file "defs" :pathname "util/defs"
                           :depends-on ("defpackage"))
             (:file "config" :pathname "misc/config"
                             :depends-on ("defpackage"))
             (:file "db" :pathname "db/db"
                         :depends-on ("defs" "constants" "creature" "obj-data" "zone-data" "room-data" "search" "artisan" "account" "spec-assign" "clan" "help" "act-social" "tongues" "act-physic" "dyntext" "spell-parser" "combat-messages" "quest" "artisan" "utils"))
             (:file "account" :pathname "db/account"
                              :depends-on ("defs" "utils"))
             (:file "act-comm" :pathname "misc/act-comm"
                               :depends-on ("defs"
                                            "utils"
                                            "constants"
                                            "interpreter"
                                            "act-social"))
             (:file "act-informative" :pathname "misc/act-informative"
                                      :depends-on ("defs"
                                                   "utils"
                                                   "interpreter"
                                                   "constants"
                                                   "creature"))
             (:file "act-movement" :pathname "misc/act-movement"
                                   :depends-on ("defs"
                                                "utils"
                                                "interpreter"
                                                "constants"))
             (:file "act-obj" :pathname "misc/act-obj"
                                :depends-on ("defs"
                                             "utils"
                                             "interpreter"
                                             "constants"
                                             "obj-data"
                                             "creature"))
             (:file "act-other" :pathname "misc/act-other"
                                :depends-on ("defs" "utils" "interpreter"))
             (:file "act-physic" :pathname "classes/act-physic"
                                 :depends-on ("defs" "utils" "interpreter"))
             (:file "act-social" :pathname "social/act-social"
                                 :depends-on ("defs" "utils" "interpreter"))
             (:file "act-wizard" :pathname "misc/act-wizard"
                                 :depends-on ("defs" "utils" "interpreter"))
             (:file "artisan" :pathname "mobiles/artisan"
                              :depends-on ("creature"))
             (:file "ban" :pathname "net/ban"
                          :depends-on ("defs" "utils"))
             (:file "bomb" :pathname "objects/bomb"
                           :depends-on ("defs" "utils"))
             (:file "char-class" :pathname "classes/char-class"
                    :depends-on ("defpackage"))
             (:file "constants" :pathname "db/constants"
                    :depends-on ("defpackage"))
             (:file "clan" :pathname "clan/clan"
                           :depends-on ("defs" "utils"))
             (:file "combat-messages" :pathname "combat/combat-messages"
                                      :depends-on ("defs" "utils"))
             (:file "comm" :pathname "net/comm"
                           :depends-on ("defs" "utils" "network" "nanny"))
             (:file "creature" :pathname "structs/creature"
                               :depends-on ("defs" "random"))
             (:file "creature-io" :pathname "structs/creature-io"
                                  :depends-on ("creature"))
             (:file "dyntext" :pathname "dyntext/dyntext"
                              :depends-on ("defs" "utils"))
             (:file "fight" :pathname "combat/fight"
                             :depends-on ("defs" "utils"))
             (:file "groups" :pathname "interpreter/groups"
                             :depends-on ("defs" "utils"))
             (:file "graph" :pathname "util/graph"
                             :depends-on ("creature" "room-data"))
             (:file "handler" :pathname "util/handler"
                              :depends-on ("defs"))
             (:file "help" :pathname "help/help"
                           :depends-on ("defs" "utils"))
             (:file "house" :pathname "house/house"
                            :depends-on ("defs" "utils"))
             (:file "interpreter" :pathname "interpreter/interpreter"
                                  :depends-on ("defs" "utils"))
             (:file "limits" :pathname "misc/limits"
                            :depends-on ("defs" "creature"))
             (:file "login" :pathname "misc/login"
                            :depends-on ("defs" "char-class"))
             (:file "magic" :pathname "magic/magic"
                           :depends-on ("defs" "creature"))
             (:file "mail" :pathname "mobiles/mail"
                           :depends-on ("defs" "creature"))
             (:file "nanny" :pathname "net/nanny"
                            :depends-on ("defs" "utils" "config" "network"
                                                "login" "mail"))
             (:file "network" :pathname "net/network"
                              :depends-on ("defs"))
             (:file "obj-data" :pathname "structs/obj-data"
                               :depends-on ("defs"))
             (:file "paths" :pathname "objects/paths"
                            :depends-on ("defs" "utils"))
             (:file "prog-compile" :pathname "search/prog-compile"
                                   :depends-on ("defs" "utils"))
             (:file "prog" :pathname "search/prog"
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
             (:file "smokes" :pathname "objects/smokes"
                             :depends-on ("defs"))
             (:file "spec-assign" :pathname "specials/spec-assign"
                                  :depends-on ("defs" "utils"))
             (:file "spec-procs" :pathname "specials/spec-procs"
                                 :depends-on ("defs" "utils"))
             (:file "spells" :pathname "magic/spells"
                                   :depends-on ("defs" "utils"))
             (:file "spell-parser" :pathname "magic/spell-parser"
                                   :depends-on ("defs" "utils"))
             (:file "structs" :pathname "structs/structs"
                              :depends-on ("defs"))
             (:file "tongues" :pathname "social/tongues"
                              :depends-on ("defs" "utils"))
             (:file "utils" :pathname "util/utils"
                            :depends-on ("defs" "network"))
             (:file "weather" :pathname "util/weather"
                              :depends-on ("zone-data"))
             (:file "zone-data" :pathname "structs/zone-data"
                                :depends-on ("defs"))))))


(defsystem #:tempus.test
  :name "tempus.test"
  :version "1.0.0d"
  :author "Daniel Lowe <dlowe@bitmuse.com>"
  :description "Testing code for the TempusMUD"
  :depends-on (:tempus :stefil)
  :components ((:file "defpackage" :pathname "tests/test-defpackage")
               (:file "helpers" :pathname "tests/helpers"
                      :depends-on ("defpackage"))
               (:file "test-comm" :pathname "tests/test-comm"
                      :depends-on ("helpers"))
               (:file "test-creature" :pathname "tests/test-creature"
                      :depends-on ("helpers"))
               (:file "test-graph" :pathname "tests/test-graph"
                      :depends-on ("helpers"))
               (:file "test-informative" :pathname "tests/test-informative"
                      :depends-on ("helpers"))
               (:file "test-io" :pathname "tests/test-io"
                      :depends-on ("helpers"))
               (:file "test-magic" :pathname "tests/test-magic"
                      :depends-on ("helpers"))
               (:file "test-misc" :pathname "tests/test-misc"
                      :depends-on ("helpers"))
               (:file "test-move" :pathname "tests/test-move"
                      :depends-on ("helpers"))
               (:file "test-network" :pathname "tests/test-network"
                      :depends-on ("helpers"))
               (:file "test-obj" :pathname "tests/test-obj"
                      :depends-on ("helpers"))
               (:file "test-parser" :pathname "tests/test-parser"
                      :depends-on ("helpers"))
               (:file "test-random" :pathname "tests/test-random"
                      :depends-on ("helpers"))
               (:file "test-utils" :pathname "tests/test-utils"
                      :depends-on ("helpers"))
               (:file "test-wizard" :pathname "tests/test-wizard"
                      :depends-on ("helpers"))))

(defmethod perform ((op test-op) (system (eql (find-system :tempus))))
  (operate 'load-op '#:tempus.test)
  (funcall (read-from-string "tempus::boot-db"))
  (write-line (princ-to-string (funcall (read-from-string "tempus.tests::test")))))


(defmethod operation-done-p ((op test-op) (system (eql (find-system :tempus))))
  nil)