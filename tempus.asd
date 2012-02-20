(defpackage #:tempus-system (:use #:asdf #:cl))
(in-package #:tempus-system)

#.(declaim (optimize (debug 3) (speed 0) (safety 3) (space 2)))

(defsystem :tempus
  :name "Tempus"
  :version "1.0.0"
  :author "Daniel Lowe <dlowe@dlowe.net>"
  :description "Tempus MUD Codebase"
  :depends-on (postmodern
               anaphora
               alexandria
               local-time
               cl-postgres+local-time
               cl-interpol
               cl-ppcre
               cffi
               cxml
               iolib)

  :components
  ((:module :src :serial t :components
            ((:module :compile :components
                      ((:file "defpackage")
                       (:file "defs" :depends-on ("defpackage"))
                       (:file "defcommand" :depends-on ("defpackage"))
                       (:file "config" :depends-on ("defpackage"))
                       (:file "constants" :depends-on ("defpackage"))))
             (:module :definitions
                      :components
                      ((:file "clan")
                       (:file "creature")
                       (:file "house")
                       (:file "room-data")
                       (:file "net-classes")
                       (:file "obj-data")
                       (:file "smokes" :depends-on ("obj-data"))
                       (:file "bomb" :depends-on ("obj-data"))
                       (:file "search")
                       (:file "structs")
                       (:file "zone-data")))
             (:module :utilities
                      :components
                      ((:file "ban")
                       (:file "comm")
                       (:file "graph")
                       (:file "logging")
                       (:file "login")
                       (:file "network")
                       (:file "random")
                       (:file "sight")
                       (:file "utils")))
             (:module :persistence
                      :components
                      ((:file "account")
                       (:file "creature-io")
                       (:file "db")))
             (:module :events
                      :components
                      ((:file "char-class")
                       (:file "handler")
                       (:file "limits")
                       (:file "paths")))
             (:module :actions
                      :components
                      ((:file "act-barb")
                       (:file "act-comm")
                       (:file "act-informative")
                       (:file "act-movement")
                       (:file "act-obj")
                       (:file "act-offensive")
                       (:file "act-other")
                       (:file "act-physic")
                       (:file "act-social")
                       (:file "act-thief")
                       (:file "act-wizard")
                       (:file "combat-messages")
                       (:file "combat-utils")
                       (:file "fight")
                       (:file "magic")
                       (:file "pvp")
                       (:file "spell-parser")
                       (:file "spells")
                       (:file "tongues")))
             (:module :support
                      :components
                      ((:file "dyntext")
                       (:file "editor")
                       (:file "help")
                       (:file "olc-mob")
                       (:file "olc-obj")
                       (:file "olc-srch")
                       (:file "olc-wld")
                       (:file "olc-zon")
                       (:file "olc")
                       (:file "quest")
                       (:file "voice")))
             (:module :textui
                      :components
                      ((:file "groups")
                       (:file "interpreter")
                       (:file "nanny")
                       (:file "wizard-cmds")
                       (:file "combat-cmds")))
             (:module :engine
                      :components
                      ((:file "prog")
                       (:file "prog-compile")
                       (:file "room-update")
                       (:file "mob-update")
                       (:file "weather")))
             (:module :specials
                      :components
                      ((:file "spec-procs")
                       (:file "spec-assign")
                       (:file "artisan")
                       (:file "fido")
                       (:file "mail")
                       (:file "guildmaster")
                       (:file "bounty-clerk")
                       (:file "mystical-enclave")
                       (:file "receptionist")
                       (:file "bank")
                       (:file "vendor")))))))


(defsystem #:tempus.test
  :name "tempus.test"
  :version "1.0.0"
  :author "Daniel Lowe <dlowe@dlowe.net>"
  :description "Testing code for the TempusMUD"
  :depends-on (:tempus :stefil)
  :components ((:file "defpackage" :pathname "tests/test-defpackage")
               (:file "helpers" :pathname "tests/helpers"
                      :depends-on ("defpackage"))
               (:file "test-clan" :pathname "tests/test-clan"
                      :depends-on ("helpers"))
               (:file "test-combat" :pathname "tests/test-combat"
                      :depends-on ("helpers"))
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
               (:file "test-olc" :pathname "tests/test-olc"
                      :depends-on ("helpers"))
               (:file "test-parser" :pathname "tests/test-parser"
                      :depends-on ("helpers"))
               (:file "test-random" :pathname "tests/test-random"
                      :depends-on ("helpers"))
               (:file "test-security" :pathname "tests/test-security"
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
