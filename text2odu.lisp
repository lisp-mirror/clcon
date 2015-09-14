; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-
; see also text2odu.tcl
; this code runs in arbitrary SWANK worker thread 

(in-package :clco)


(defvar *text2odu-event-queue-lock*
  (bt:make-lock "*text2odu-event-queue-lock*")
  "Lock to operate with *text2odu-event-queue*")

(defvar *text2odu-event-queue*
  (list)
  "Event queue for messages from clcon to oduvan")


(deftype text2odu-event-kind ()
  '(member construct-backend-buffer tcl-text-insert tcl-text-delete destroy-backend-buffer))

(defstruct text2odu-event
  (kind (budden-tools:mandatory-slot 'kind) :type text2odu-event-kind)
  ...
  )
        
  

(defun post-oduvan-event (clcon_text-pathname &rest args)
  "Can be invoked in any thread"
  #-clcon-oduvan (print `(post-oduvan-event ,clcon_text-pathname ,@args))
  #+clcon-oduvan
  (bt:with-lock-held (*oduvan-input-event-queue-lock*)
    (print `(post-oduvan-event ,clcon_text-pathname ,@args))
    (setf *text2odu-event-queue*
          (append *text2odu-event-queue* (list clcon_text_pathname args)))
    )
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


(defun test1 ()
  (notify-oduvan-construct-backend-buffer "a")
  (sleep 0.1)
  (nti "a" 0 "b")
  (sleep 0.1)
  )

(test1)
