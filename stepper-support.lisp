;; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-
;; stepper support
(in-package :clco)

(defun restart-with-name-exists-p (name)
  "Might be useful for activating/deactivating menu items"
  (if 
   (find name swank::*sldb-restarts* :key #'restart-name)
   t
   nil))

(defun invoke-sldb-restart-by-name (name)
   (invoke-restart (find name swank::*sldb-restarts* :key #'restart-name)))
   
