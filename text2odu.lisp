; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-
; see also text2odu.tcl

(in-package :clco)

(defun make-oduvan-backend-buffer (clcon_text-pathname)
  "Called from tcl when text is created"
  (print `(make-oduvan-backend-buffer ,clcon_text-pathname) *terminal-io*)
  nil
  )

(defun notify-oduvan-on-tcl-text-insert (clcon_text-pathname index string)
  "Called from RoInsert"
  (print `(notify-oduvan-on-tcl-text-insert ,index ,string))
  nil)


(defun notify-oduvan-on-tcl-text-delete (clcon_text-pathname beg end)
  "Called from RoInsert"
  (print `(notify-oduvan-on-tcl-text-delete ,beg ,end))
  nil)
