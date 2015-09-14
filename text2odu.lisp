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
  '(member construct-backend-buffer before-tcl-text-insert before-tcl-text-delete destroy-backend-buffer))

(defstruct row-col
  (row 0 :type integer)
  (col 0 :type integer)
  )

(defstruct text2odu-event
  (kind (budden-tools:mandatory-slot 'kind) :type text2odu-event-kind)
  (clcon_text-pathname (budden-tools:mandatory-slot 'clcon_text-pathname) :type string)
  (string nil :type (or null string)) ; string to insert
  (beg nil :type (or null row-col)) ; begin index
  (end nil :type (or null row-col))   ; end index
  )
        
(defmacro --> (object slot)
  "bubububu"
  `(slot-value ,object ',slot))


(defun parse-row-col (tcl-index)
  "Returns row-col structure by tcl-index of kind NN.NN"
  (let ((parsed (split-sequence:split-sequence #\. tcl-index)))
    (assert (eq (length parsed) 2))
    (make-row-col :row (parse-integer (first parsed))
                  :col (parse-integer (second parsed)))
   
    ))

;  :clcon-server
(defun post-oduvan-event (event)
  "Can be invoked in any thread"
  #-clcon-oduvan (print `(post-oduvan-event ,event))
  #+clcon-oduvan
  (bt:with-lock-held (*text2odu-event-queue-lock*)
    (print `(post-oduvan-event ,event))
    (setf *text2odu-event-queue*
          (append *text2odu-event-queue*
                  (list (budden-tools:the* text2odu-event event))))
    )
  nil
  )

(defun notify-oduvan-construct-backend-buffer (clcon_text-pathname)
  "Called from tcl when text is created, from arbitrary thread"
  (post-oduvan-event
   (make-text2odu-event
    :kind 'construct-backend-buffer
    :clcon_text-pathname clcon_text-pathname
    )))

(defun nti  (clcon_text-pathname index string)
  "notify-oduvan-tcl-text-insert . Called from RoInsert"
  (post-oduvan-event
   (make-text2odu-event
    :kind 'before-tcl-text-insert
    :clcon_text-pathname clcon_text-pathname
    :beg (parse-row-col index)
    :string string
    )))


(defun notify-oduvan-tcl-text-delete (clcon_text-pathname beg end)
  "Called from RoDelete before really deleting text"
  (post-oduvan-event
   (make-text2odu-event
    :kind 'before-tcl-text-delete
    :clcon_text-pathname clcon_text-pathname
    :beg (parse-row-col beg)
    :end (parse-row-col end)
    )))


(defun notify-oduvan-destroy-backend-buffer (clcon_text-pathname)
  "Called from RoInsert"
  (post-oduvan-event
   (make-text2odu-event
    :kind 'destroy-backend-buffer
    :clcon_text-pathname clcon_text-pathname
    )))


(defun test1 ()
  (notify-oduvan-construct-backend-buffer "a")
  (sleep 0.1)
  (nti "a" "0.0" "b")
  (sleep 0.1)
  (notify-oduvan-destroy-backend-buffer "a")
  )

(test1)
