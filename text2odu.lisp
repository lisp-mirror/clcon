; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-
; see also text2odu.tcl

(in-package :clco)

(defun make-oduvan-backend-buffer (clcon_text-pathname)
  "Called from tcl when text is created"
  #-clcon-oduvan (print `(make-oduvan-backend-buffer ,clcon_text-pathname) *terminal-io*)
  #+clcon-oduvan (do-make-oduvan-backend-buffer clcon_text-pathname)
  nil
  )

(defun notify-oduvan-on-tcl-text-insert (clcon_text-pathname index string)
  "Called from RoInsert"
  (declare (ignorable clcon_text-pathname))
  #-clcon-oduvan (print `(notify-oduvan-on-tcl-text-insert ,index ,string))
  #+clcon-oduvan (do-notify-oduvan-on-tcl-text-insert clcon_text-pathname index string)
  nil)


(defun notify-oduvan-on-tcl-text-delete (clcon_text-pathname beg end)
  "Called from RoInsert"
  (declare (ignorable clcon_text-pathname))
  #-clcon-oduvan (print `(notify-oduvan-on-tcl-text-delete ,beg ,end))
  #+clcon-oduvan (do-notify-oduvan-on-tcl-text-delete clcon_text-pathname beg end)
  nil)
