(in-package #:tempus)

(define-special mystical-enclave (trigger self ch command vars) (+spec-rm+)
  (declare (ignore self vars))
  (when (and (eql trigger 'command)
             (string= "up" (first (command-info-pattern command)))
             (is-psionic ch)
             (>= (position-of ch) +pos-standing+)
             (null (mounted-of ch)))
    (let ((dest (real-room 30126)))
      (when dest
        (act ch
             :subject-emit "You leave upwards, into the future..."
             :place-emit "$n vanishes into the ceiling...")
        (char-from-room ch nil)
        (char-to-room ch dest nil)
        (look-at-room ch dest nil)
        (act ch :place-emit "$n arrives, climbing up from the past...")
        t))))