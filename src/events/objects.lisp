(in-package #:tempus)

(defconstant +fuse-burn+ 1)
(defconstant +fuse-electronic+ 2)
(defconstant +fuse-remote+ 3)
(defconstant +fuse-contact+ 4)
(defconstant +fuse-motion+ 5)


(defun fuse-is-remote? (fuse)
  (and (is-obj-kind fuse +item-fuse+)
       (= (fuse-type fuse) +fuse-remote+)))

(defun fuse-is-burnable? (fuse)
  (and (is-obj-kind fuse +item-fuse+)
       (= (fuse-type fuse) +fuse-burn+)))

(defun activate-device (ch obj)
  (decf (obj-val-of obj 1) (obj-val-of obj 3))
  (setf (obj-val-of obj 2) 1)
  (apply-object-affects ch obj t)
  (affect-total ch))

(defun activate-bomb (ch obj)
  (setf (obj-val-of (first (contains-of obj)) 1) 1)
  (setf (obj-val-of obj 3) (idnum-of ch)))

(defun detonate-bomb (obj)
  "TODO: implement detonate bomb"
  nil)