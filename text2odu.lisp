; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-
; see also text2odu.tcl

(in-package :clco)

(defun make-oduvan-backend-buffer (tcl-id)
  "Called from tcl when text is created"
  (print `(make-oduvan-backend-buffer ,tcl-id))
  )

(defun notify-oduvan-on-tcl-text-change (&rest args)
  "Called from RoInsert, RoDelete, RoReplace"
  (print `(notify-oduvan-on-tcl-text-change ,@args)))
