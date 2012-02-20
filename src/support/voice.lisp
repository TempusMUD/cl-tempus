(in-package #:tempus)

(defparameter +voice-taunting+ 0)         ; NPC remembers creature in room
(defparameter +voice-attacking+ 1)            ; NPC attacking remembered creature
(defparameter +voice-panicking+ 2)            ; NPC is running away from remembered
(defparameter +voice-hunt-found+ 3)           ; Hunter found his prey
(defparameter +voice-hunt-lost+ 4)            ; Hunter lost his prey
(defparameter +voice-hunt-gone+ 5)            ; Hunter's prey disappeared
(defparameter +voice-hunt-taunt+ 6)           ; Hunter is busy hunting
(defparameter +voice-hunt-unseen+ 7)          ; Hunter found invisible prey
(defparameter +voice-hunt-openair+ 8)         ; Hunter can't track over air
(defparameter +voice-hunt-water+ 9)           ; Hunter can't track over water
(defparameter +voice-fight-winning+ 10)        ; NPC is winning the fight
(defparameter +voice-fight-losing+ 11)         ; NPC is losing the fight
(defparameter +voice-fight-helping+ 12)        ; NPC is assisting another NPC
(defparameter +voice-obeying+ 13)              ; NPC is obeying a command


(defun emit-voice (ch target voice-id)
  "TODO: Implement emit-voice"
  nil)