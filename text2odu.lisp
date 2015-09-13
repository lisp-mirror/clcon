; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-
; see also text2odu.tcl
; this code runs in arbitrary SWANK worker thread 

(in-package :clco)

(defun post-oduvan-event (clcon_text-pathname &rest args)
  #-clcon-oduvan (print `(post-oduvan-event ,clcon_text-pathname ,@args))
  #+clcon-oduvan (print `(post-oduvan-event ,clcon_text-pathname ,@args))
  nil
  )
  

(defun notify-oduvan-construct-backend-buffer (clcon_text-pathname)
  "Called from tcl when text is created"
  (post-oduvan-event 'constuct-backend-buffer clcon_text-pathname)
  )

(defun nti  (clcon_text-pathname index string)
  "notify-oduvan-tcl-text-insert . Called from RoInsert"
  (post-oduvan-event 'tcl-text-insert clcon_text-pathname index string)
  )


(defun notify-oduvan-tcl-text-delete (clcon_text-pathname beg end)
  "Called from RoInsert"
  (post-oduvan-event 'tcl-text-delete clcon_text-pathname beg end)
  )


(defun notify-oduvan-destroy-backend-buffer (clcon_text-pathname)
  "Called from RoInsert"
  (post-oduvan-event 'destroy-backend-buffer clcon_text-pathname)
  )

