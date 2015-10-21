;; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-
;; see also highlight-from-queue-to-tcl.lisp

;; 
;; threads: we do not assume in which thread notification functions are called. 
;; As oduvanchik knows something new about highlight changes, it calls functions from
;; the module. The functions work with *highlight-event-queue* only
;; we'll try to be always async. Synchronization is supported by
;; sending growing change number.

(in-package :clco)

(declaim (optimize debug))

(defvar *highlight-dispatcher-start-shutdown-lock*
  (bt:make-lock "clcon-server::*highlight-dispatcher-start-shutdown-lock*"))

(defvar *highlight-event-queue*
  (list)
  "Event queue for highlight messages from oduvan to all clcon_text's")

(defvar *highlight-condition-variable*
  (bt:make-condition-variable :name "clcon-server::*highlight-condition-variable*"
                              ))

(defvar *highlight-event-queue-lock*
  (bt:make-lock "*highlight-event-queue-lock*")
  "Lock to operate with *highlight-event-queue*")


(defvar *highlight-dispatcher-thread* nil)


(defun reset-highlight-event-queue ()
  (bt:with-lock-held (*highlight-event-queue-lock*)
    (setf *highlight-event-queue* (list))
    (bt:condition-notify *highlight-condition-variable*)
    )
  nil
  )

(deftype highlight-event-kind ()
  '(member
    highlight-single-line
    cancel-highlighting
    shutdown-highlight-dispatcher ; called at the exit
    ))

(defstruct highlight-event
  (kind (budden-tools:mandatory-slot 'kind) :type highlight-event-kind)
  (clcon_text-pathname nil :type (or null string)) 
  (string nil :type (or null string)) ; encoded-marks
  (change-id 0 :type integer) ; identifies state of the buffer as of odu::buffer-change-id
  (end-line-no 0 :type integer) ; last line no to which this event corresponds. If something is edited at this line or before, event is to be canceled
  (swank-connection nil :type (or null swank::multithreaded-connection) ) ; can be omitted for shutdown event
  )

(defun post-highlight-event (event)
  "Can be invoked in any thread"
  #-clcon-oduvan (print `(post-higlight-event ,event))
  #+clcon-oduvan
  (bt:with-lock-held (*highlight-event-queue-lock*)
    (setf *highlight-event-queue*
          (append *highlight-event-queue*
                  (list (budden-tools:the* highlight-event event))))
    (bt:condition-notify *highlight-condition-variable*)
    )
  nil
  )

(defun notify-highlight-single-line (clcon_text-pathname encoded-marks line-no change-id buffer)
  "See also clco::eval-highlight-single-line"
  (assert (equal clcon_text-pathname (oi::buffer-to-clcon_text buffer)))
  (post-highlight-event
   (make-highlight-event
    :kind 'highlight-single-line
    :clcon_text-pathname clcon_text-pathname
    :string encoded-marks
    :change-id change-id
    :end-line-no line-no
    :swank-connection (oi::variable-value 'odu::swank-connection :buffer buffer)
    )))


(defun shutdown-highlight-dispatcher ()
  "Shuts down dispatcher and waits for its termination. 
   See also start-highlight-dispatcher"
  (print "Shutting down highlight-dispatcher-thread...")
  (bt:with-lock-held (*highlight-dispatcher-start-shutdown-lock*)
    (unwind-protect
         (when (and (bt:threadp *highlight-dispatcher-thread*)
                    (bt:thread-alive-p *highlight-dispatcher-thread*))
           (post-oduvan-event
            (make-highlight-event
             :kind 'shutdown-highlight-dispatcher
             ))
           (bt:join-thread *highlight-dispatcher-thread*))
      (setf *highlight-dispatcher-thread* nil)
      nil
      )))



