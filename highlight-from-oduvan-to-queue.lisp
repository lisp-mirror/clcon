;; -*- coding: utf-8; system: clcon-server; -*-
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
    highlight-single-line ; старая раскраска - испольузется в лиспе
    highlight-3 ; :РАСКРАСКА-3
    cancel-highlighting
    package-change ; we entered into a place where we have another package in the same buffer, or we obtained or lost information of it
    readtable-change
    mode-change ; editor mode changed
    shutdown-highlight-dispatcher ; called at the exit
    ))

(defstruct highlight-event
  (kind (budden-tools:mandatory-slot 'kind) :type highlight-event-kind)
  (clcon_text-pathname nil :type (nullable string)) 
  (string nil :type (nullable string)) ; encoded-marks
  (change-id 0 :type integer) ; identifies state of the buffer as of odu::buffer-change-id
  (end-line-no 0 :type integer) ; last line no to which this event corresponds. If something is edited at this line or before, event is to be canceled
  (swank-connection nil :type (nullable swank::multithreaded-connection) ) ; can be omitted for shutdown event
  (|Код-слоя-раскраски| 0 :type integer) ; см. :РАСКРАСКА-3
  )

(defun post-highlight-event (event)
  "Can be invoked in any thread. Events are then processed by clco::highlight-dispatcher-thread-function"
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
    :change-id change-id ; не используется  
    :end-line-no line-no ; не используется
    :swank-connection (oi::variable-value 'odu::swank-connection :buffer buffer)
    )))

(defun notify-highlight-3 (clcon_text-pathname encoded-marks buffer)
  "Для объекта РАСКРАСКА-3::|Отправитель-раскраски| .
   ПРАВЬМЯ реализовать eval-highlight-3 и организовать её вызов по диспетчеризации
   См. также notify-highlight-single-line, eval-highlight-3 . 
   РАСКРАСКА-3::|Закодировать-один-мазок| "
  (assert (equal clcon_text-pathname (oi::buffer-to-clcon_text buffer)))
  (post-highlight-event
   (make-highlight-event
    :kind 'highlight-3
    :clcon_text-pathname clcon_text-pathname
    :string encoded-marks
    :swank-connection (oi::variable-value 'odu::swank-connection :buffer buffer)
    )))

(defun encode-last-package-name-sent-to-tcl-type (x)
  "Encodes value of oi::last-package-name-sent-to-tcl-type to be sent to lisp"
  (etypecase x
    ((eql :unknown) "{0}")
    (string (format nil "{1 ~A}" (cl-tk:tcl-escape x)))))

(defun encode-last-readtable-name-sent-to-tcl-type (x)
  "Encodes value of oi::last-package-or-readtable-name-sent-to-tcl-type to be sent to lisp"
  (etypecase x
    (string (assert (string= x "UNKNOWN")) "{0}") ; unknown
    (symbol (format nil "{1 ~A}" (cl-tk:tcl-escape (string x))))))


(defun notify-package-change (clcon_text-pathname package-name-or-info buffer)
  "See also odu::send-package-to-clcon, notify-readtable-change"
  (let ((package-info
         (encode-last-package-name-sent-to-tcl-type
          package-name-or-info)))
    (post-highlight-event
     (make-highlight-event
      :kind 'package-change
      :clcon_text-pathname clcon_text-pathname
      :string package-info
      :swank-connection (oi::variable-value 'odu::swank-connection :buffer buffer)
      ))))


(defun notify-readtable-change (clcon_text-pathname readtable-name-or-info buffer)
  "Clone of notify-package-change"
  (let ((readtable-info
         (encode-last-readtable-name-sent-to-tcl-type
          readtable-name-or-info)))
    (post-highlight-event
     (make-highlight-event
      :kind 'readtable-change
      :clcon_text-pathname clcon_text-pathname
      :string readtable-info
      :swank-connection (oi::variable-value 'odu::swank-connection :buffer buffer)
      ))))

(defun notify-mode-change (clcon_text-pathname mode-name-or-info buffer)
  "Clone of notify-package-change"
  (let ((mode-info
         (encode-last-package-name-sent-to-tcl-type
          mode-name-or-info)))
    (post-highlight-event
     (make-highlight-event
      :kind 'mode-change
      :clcon_text-pathname clcon_text-pathname
      :string mode-info
      :swank-connection (oi::variable-value 'odu::swank-connection :buffer buffer)
      ))))



(defun shutdown-highlight-dispatcher ()
  "Shuts down dispatcher and waits for its termination. 
   See also start-highlight-dispatcher"
  (print "Shutting down highlight-dispatcher-thread...")
  (bt:with-lock-held (*highlight-dispatcher-start-shutdown-lock*)
    (unwind-protect
         (when (and (bt:threadp *highlight-dispatcher-thread*)
                    (bt:thread-alive-p *highlight-dispatcher-thread*))
           (post-highlight-event
            (make-highlight-event
             :kind 'shutdown-highlight-dispatcher
             ))
           (bt:join-thread *highlight-dispatcher-thread*))
      (setf *highlight-dispatcher-thread* nil)
      nil
      )))



