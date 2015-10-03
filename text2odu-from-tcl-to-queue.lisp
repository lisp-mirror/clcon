;; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-
;; see also text2odu.tcl, clcon_text.tcl, doc/text2odu.md

;; Acception of text2odu events from tcl and putting them onto *text2odu-event-queue*
;; this code runs in arbitrary SWANK worker thread 
;; it must not depend on oduvanchik source


(in-package :clco)

(defvar *text2odu-disptatcher-start-shutdown-lock*
  (bt:make-lock "clcon-server::*text2odu-disptatcher-start-shutdown-lock*"))

(defvar *text2odu-event-queue*
  (list)
  "Event queue for messages from clcon to oduvan")

(defvar *text2odu-condition-variable*
  (bt:make-condition-variable :name "clcon-server::*text2odu-condition-variable*"
                              ))

(defvar *text2odu-event-queue-lock*
  (bt:make-lock "*text2odu-event-queue-lock*")
  "Lock to operate with *text2odu-event-queue*")


(defvar *text2odu-dispatcher-thread* nil)


(defun reset-text2odu-event-queue ()
  (bt:with-lock-held (*text2odu-event-queue-lock*)
    (setf *text2odu-event-queue* (list))
    (bt:condition-notify *text2odu-condition-variable*)
    )
  nil
  )

(deftype text2odu-event-kind ()
  '(member
    construct-backend-buffer
    before-tcl-text-insert
    before-tcl-text-delete
    destroy-backend-buffer
    shutdown-text2odu-dispatcher ; called at the exit
    ;; eval-oduvanchik-command ; hypotetic. We send command from tcl in freezed mode,
    ;; sync insertion points, bind *oduvanchik-backend* to t so that all
    ;; editions are sent synchronously to tcl instead of local processing in oduvanchik
    call-oduvanchik-function-with-clcon_text ; particular case of eval-oduvanchik-command just for one command
    ))

(defstruct row-col
  (row 0 :type integer)
  (col 0 :type integer)
  )

(defstruct text2odu-event
  (kind (budden-tools:mandatory-slot 'kind) :type text2odu-event-kind)
  (clcon_text-pathname nil :type (or null string)) 
  (string nil :type (or null string)) ; string to insert
  (beg nil :type (or null row-col)) ; begin index
  (end nil :type (or null row-col))   ; end index
  (far_tcl_cont_id nil :type (or null integer)) ; tcl continuation id to eval after event's action is processed
  (swank-connection nil :type (or null swank::multithreaded-connection)) ; can be omitted for several event types
  )

(defun parse-row-col (tcl-index)
  "Returns row-col structure by tcl-index of kind NN.NN."
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
    ;(print `(post-oduvan-event ,event))
    (setf *text2odu-event-queue*
          (append *text2odu-event-queue*
                  (list (budden-tools:the* text2odu-event event))))
    (bt:condition-notify *text2odu-condition-variable*)
    )
  nil
  )

(defun notify-oduvan-construct-backend-buffer (clcon_text-pathname)
  "Called from tcl when text is created, from arbitrary thread"
  (post-oduvan-event
   (make-text2odu-event
    :kind 'construct-backend-buffer
    :clcon_text-pathname clcon_text-pathname
    :swank-connection swank::*emacs-connection*
    )))

(defun nti  (clcon_text-pathname index string)
  "notify-oduvan-tcl-text-insert . Called from RoInsert"
  (post-oduvan-event
   (make-text2odu-event
    :kind 'before-tcl-text-insert
    :clcon_text-pathname clcon_text-pathname
    :beg (parse-row-col index)
    :string string
    :swank-connection swank::*emacs-connection*
    )))

(defun call-oduvanchik-function-with-clcon_text (clcon_text-pathname insert-index far_tcl_cont_id oduvanchik-function-name)
  "Send call-oduvanchik-function-with-clcon_text event to oduvanchik. See oduvanchik::call-oduvanchik-function-with-clcon_text"
  (post-oduvan-event
   (make-text2odu-event
    :kind 'call-oduvanchik-function-with-clcon_text
    :clcon_text-pathname clcon_text-pathname
    :string oduvanchik-function-name
    :beg (parse-row-col insert-index) ; likely to be unused
    :far_tcl_cont_id far_tcl_cont_id
    :swank-connection swank::*emacs-connection*
    )))

(defun notify-oduvan-tcl-text-delete (clcon_text-pathname beg end)
  "Called from RoDelete before really deleting text"
  (post-oduvan-event
   (make-text2odu-event
    :kind 'before-tcl-text-delete
    :clcon_text-pathname clcon_text-pathname
    :beg (parse-row-col beg)
    :end (if (equal end "") nil (parse-row-col end))
    :swank-connection swank::*emacs-connection*
    )))


(defun notify-oduvan-destroy-backend-buffer (clcon_text-pathname)
  "Called from RoInsert"
  (post-oduvan-event
   (make-text2odu-event
    :kind 'destroy-backend-buffer
    :clcon_text-pathname clcon_text-pathname
    :swank-connection swank::*emacs-connection*
    )))


(defun shutdown-text2odu-dispatcher ()
  "Shuts down dispatcher and waits for its termination. 
   See also start-text2odu-dispatcher"
  (print "Shutting down text2odu-dispatcher-thread...")
  (bt:with-lock-held (*text2odu-disptatcher-start-shutdown-lock*)
    (unwind-protect
         (when (and (bt:threadp *text2odu-dispatcher-thread*)
                    (bt:thread-alive-p *text2odu-dispatcher-thread*))
           (post-oduvan-event
            (make-text2odu-event
             :kind 'shutdown-text2odu-dispatcher
             ))
           (bt:join-thread *text2odu-dispatcher-thread*))
      (setf *text2odu-dispatcher-thread* nil)
      nil
      )))

