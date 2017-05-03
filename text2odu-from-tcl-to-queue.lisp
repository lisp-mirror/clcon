;; -*- coding: utf-8 ; Encoding: utf-8 ; system :clcon-server ; -*-
;; see also text2odu.tcl, clcon_text.tcl, doc/text2odu.md

;; Acception of text2odu events from tcl and putting them onto *text2odu-event-queue*
;; this code runs in arbitrary SWANK worker thread 
;; it must not depend on oduvanchik source


(in-package :clco)

;; Disable stepping here
(proclaim '(optimize (debug 3) (compilation-speed 3)))

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

    ;; this event originates from oduvanchik itself, but we want it was processed as it come from the outside, so we have no internal oduvanchik's functions on the stack. Data is
    ;; contained in fn-and-args field.
    call-oduvanchik-from-itself
    ;; special message when cursor moved
    cursor-moved 
    |Лисп-зпт-пришли-раскраску|  ; h <Код-слоя> 
    ))

(defstruct row-col
  (row 0 :type integer)
  (col 0 :type integer)
  )

(defstruct text2odu-event
  (kind (budden-tools:mandatory-slot 'kind) :type text2odu-event-kind)
  (clcon_text-pathname nil :type (or null string)) 
  (string nil :type (or null string)) ; string argument, e.g. string to insert
  (fn-and-args nil :type (or null (cons symbol t))) ; fn-and-args for call-oduvanchik-function-with-clcon_text
  (options nil :type (or null (cons string t))) ; options for call-oduvanchik-function-with-clcon_text
  (beg nil :type (or null row-col)) ; begin index
  (end nil :type (or null row-col))   ; end index
  (far_tcl_cont_id nil :type (or null integer)) ; tcl continuation id to eval after event's action is processed
  (swank-connection nil :type (or null swank::multithreaded-connection)) ; can be omitted for several event types
  (tick_count 0 :type integer)
  (|Код-слоя| nil :type (or null integer))
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
  ;(assert (TEXT2ODU-EVENT-SWANK-CONNECTION event))
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

(defun notify-oduvan-construct-backend-buffer (clcon_text-pathname tick_count filename)
  "Called from tcl when text is created, from arbitrary thread. See also oduvanchik::eval-construct-backend-buffer; ::clcon_text::MaybeSendToLisp"
  (post-oduvan-event
   (make-text2odu-event
    :kind 'construct-backend-buffer
    :clcon_text-pathname clcon_text-pathname
    :string filename
    :swank-connection swank::*emacs-connection*
    :tick_count tick_count
    )))

(defun ncm (clcon_text-pathname tick_count pos)
  "notify-oduvan-cursor-moved. See oduvanchik::eval-notify-oduvan-cursor-moved, ::clcon_text::MaybeSendToLisp"
  (post-oduvan-event
   (make-text2odu-event
    :kind 'cursor-moved
    :clcon_text-pathname clcon_text-pathname
    :beg (parse-row-col pos)
    :swank-connection swank::*emacs-connection*
    :tick_count tick_count
    )))

(defun nhi (clcon_text-pathname tick_count |Код-слоя|)
  " .apr EVAL-NOTIFY-ODUVAN-TCL-Лисп-зпт-пришли-раскраску, ::clcon_text::MaybeSendToLisp"
  (post-oduvan-event
   (make-text2odu-event
    :kind '|Лисп-зпт-пришли-раскраску|
    :clcon_text-pathname clcon_text-pathname
    :swank-connection swank::*emacs-connection*
    :tick_count tick_count
    :|Код-слоя| |Код-слоя|
    )))

(defun nti  (clcon_text-pathname tick_count index string)
  "notify-oduvan-tcl-text-insert . Called from RoInsert. See oduvanchik::eval-before-tcl-text-insert, ::clcon_text::MaybeSendToLisp"
  (post-oduvan-event
   (make-text2odu-event
    :kind 'before-tcl-text-insert
    :clcon_text-pathname clcon_text-pathname
    :beg (parse-row-col index)
    :string string
    :swank-connection swank::*emacs-connection*
    :tick_count tick_count
    )))

(defun call-oduvanchik-function-with-clcon_text (clcon_text-pathname tick_count insert-index far_tcl_cont_id oduvanchik-function-name-and-args options)
  "Send call-oduvanchik-function-with-clcon_text event to oduvanchik. See oduvanchik::eval-call-oduvanchik-function-with-clcon_text, ::clcon_text::MaybeSendToLisp. 
Передача tick_count здесь недостаточна, поскольку в ходе выполнения функции состояние буфера tk изменится и нужно будет заново получить её. Мы получим её после разморозки
вместе с позицией курсора, но некий временной лаг всё же возникнет, а можно было бы его избежать. Впрочем, в общем случае невозможно без заморозки tk гарантировать синхронность, поэтому временная рассинхронизация не должна иметь значения"
  (post-oduvan-event
   (make-text2odu-event
    :kind 'call-oduvanchik-function-with-clcon_text
    :clcon_text-pathname clcon_text-pathname
    :fn-and-args oduvanchik-function-name-and-args
    :options options
    :beg (parse-row-col insert-index) ; likely to be unused
    :far_tcl_cont_id far_tcl_cont_id
    :swank-connection swank::*emacs-connection*
    :tick_count tick_count
    )))

(defun notify-oduvan-tcl-text-delete (clcon_text-pathname tick_count beg end)
  "Called from RoDelete before really deleting text, see ::clcon_text::MaybeSendToLisp"
  (post-oduvan-event
   (make-text2odu-event
    :kind 'before-tcl-text-delete
    :clcon_text-pathname clcon_text-pathname
    :beg (parse-row-col beg)
    :end (if (equal end "") nil (parse-row-col end))
    :swank-connection swank::*emacs-connection*
    :tick_count tick_count
    )))


(defun notify-oduvan-destroy-backend-buffer (clcon_text-pathname)
  "Called from RoInsert. See also ::clcon_text::MaybeSendToLisp"
  (post-oduvan-event
   (make-text2odu-event
    :kind 'destroy-backend-buffer
    :clcon_text-pathname clcon_text-pathname
    :swank-connection swank::*emacs-connection*
    )))


(defun order-call-oduvanchik-from-itself (fn-and-args)
  "See oduvanchik::eval-order-call-oduvanchik-from-itself . Called from lisp only. 
  См. также команду ODUVANCHIK::VYZVATQ-FUNKCIYU-ZPT-OPREDELYONNUYU-NE-V-PAKETE-ODUVANCHIKA-COMMAND"
  ;(sb-debug:print-backtrace :count 5)
  (post-oduvan-event
   (make-text2odu-event
    :kind 'call-oduvanchik-from-itself
    ; copy it just in case it is stack-allocated
    :fn-and-args (copy-list fn-and-args) 
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

