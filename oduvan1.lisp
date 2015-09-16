; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-
; see also text2odu.tcl, text2odu.lisp

; this file is only built if :clcon-oduvan feature present
; threads: we do not assume in which thread notification functions are called. 
; we have to design threading model for editor. E.g. one thread per buffer, one 
; dispatcher thread. Or no dedicated dispatcher thread.


(in-package :clco)

(declaim (optimize debug))

#| Useful functions:

 oduvanchik-internals::after-editor-initializations - run code after editor initialized


|#

(defun pop-text2odu-event-queue ()
  "Made after example in sb-thread:condition-wait, but without timeout as it is not interfaced to from bordeaux-threads"
  (bt:with-lock-held (*text2odu-event-queue-lock*)
    (loop until *text2odu-event-queue*
       do (or (bt:condition-wait
               *text2odu-condition-variable*
               *text2odu-event-queue-lock*
               )
              ;; Lock not held, must unwind without touching *data*.
              (return-from pop-text2odu-event-queue nil)))
    (pop *text2odu-event-queue*)))


(defun podsunutq-event (key-event text2odu-event)
  "Puts fake event onto oduvan event queue as if it came from keyboard. Clone of oduvanchik-internals::q-event . Note we do not set hunk"
  (bt:with-lock-held (oduvanchik-internals::*q-event-lock*) ; budden
    (oduvanchik-ext:without-interrupts
      (let* (
             ; stolen from oduvanchik-internals::window-input-handler
             (stream oduvanchik-internals::*editor-input*)
             (new (make-fake-keyboard-event key-event text2odu-event))
             (tail (oduvanchik-internals::editor-input-tail stream)))
        (setf (oduvanchik-internals::input-event-next tail) new)
        (setf (oduvanchik-internals::editor-input-tail stream) new)))))

; (defun new-event (key-event x y hunk next &optional unread-p)
(defun make-fake-keyboard-event (key-event text2odu-event)
  "Just trying to put some event as if it was from the keyboard"
  (oduvanchik-internals::new-event
   ; (oduvanchik-ext:char-key-event #\!)  ; stolen from hi::translate-tty-event
   key-event 
   11
   3
   nil ; hunk was smth like #<oduvanchik.x11::x11-hunk nil+374, "Main" {DDC03F1}>
   nil
   nil
   text2odu-event
   ))

(defun text2odu-dispatcher-thread-function ()
  (loop
     (let ((e (pop-text2odu-event-queue)))
       (when e
         (cond
           ((eq (--> e kind) 'shutdown-text2odu-dispatcher)
            (setf *text2odu-dispatcher-thread* nil)
            (return-from text2odu-dispatcher-thread-function nil))
           (t
            ;(format t "~%Sending real event ~S to oduvanchik keyboard buffer!" e)
            (clco-oduvanchik-key-bindings::text2odu-dispatcher-to-editor-queue-put e)
            (unless oduvanchik-internals::*direct-tcl*
              (podsunutq-event
               clco-oduvanchik-key-bindings:*text2odu-key-event-f8*
               e))))))))


(defun start-text2odu-dispatcher ()
  "See also shutdown-text2odu-dispatcher"
  (bt:with-lock-held (*text2odu-disptatcher-start-shutdown-lock*)
    (assert (null *text2odu-dispatcher-thread*))
    (setf *text2odu-dispatcher-thread*
          (bt:make-thread #'text2odu-dispatcher-thread-function
                          :name
                          "text2odu-dispatcher-thread-function"
                          )))
  )

(defun shutdown-text2odu-dispatcher-on-oduvanchik-exit-hook ()
  (shutdown-text2odu-dispatcher))

(defun start-oduvanchik (&key (direct-tcl nil))
  (declare (special oduvanchik::exit-hook oduvanchik::entry-hook))
  (let ((entered nil)
        (entered-lock (bt:make-lock "Oduvanchik entry signal lock"))
        )
    (reset-text2odu-event-queue)
    (oduvanchik-internals::remove-all-hooks oduvanchik::exit-hook)
                                        ; if previous run crashed, this can be useful
    (ignore-errors (shutdown-text2odu-dispatcher))
    (oduvanchik::add-hook oduvanchik::exit-hook
                          'shutdown-text2odu-dispatcher-on-oduvanchik-exit-hook)
    (oduvanchik::add-hook
     oduvanchik::entry-hook
     (lambda () (bt:with-lock-held (entered-lock) (setf entered t))))
    
    (setf oduvanchik-internals::*direct-tcl* direct-tcl)
    (setf oduvanchik-internals::*eval-command-from-tcl-hook* 'oduvanchik::eval-pending-text2odu-events)
    
    (bt:make-thread #'oduvanchik:oduvanchik :name "Oduvanchik")
    (format t "~%Waiting for oduvanchik to start")
                                        ; wait for oduvanchik startup 
    (loop
       (when (bt:with-lock-held (entered-lock) entered)
         (return)
         )
       (sleep 0.3)
       (format t "."))
    (clco-oduvanchik-key-bindings::set-clco-oduvanchik-key-bindings)
    (start-text2odu-dispatcher)
    ))

(defun shutdown-oduvanchik-via-keyboard-buffer ()
  (cond
    (oduvanchik-internals::*direct-tcl*
     (warn "shutdown-oduvanchik-via-keyboard-buffer: in direct tcl mode, you must not use keyboard buffer. Command ignored"))
    (t
     (podsunutq-event clco-oduvanchik-key-bindings::*f17-key-event* nil))))

; need separate file for this
(defun test1 ()
  (with-output-to-string (ddd #|*standard-output*|#)
    (start-oduvanchik)
    (sleep 0.5)
    (notify-oduvan-construct-backend-buffer "a")
    (sleep 0.1)
    (nti "a" "0.0" "(defun ugu () 'ugu)")
    (sleep 0.1)
    (notify-oduvan-destroy-backend-buffer "a")
    (shutdown-oduvanchik-via-keyboard-buffer)
    )
  (print "You need to move mouse over oduvanchik's window to close editor. Waiting...")
  (loop
     (unless *text2odu-dispatcher-thread* (return))
     (sleep 0.5)
     (format t "."))
  nil
  )

;(test1)

(defun test2 ()
  (with-output-to-string (*standard-output*)
    (dotimes (i 2)
      (reset-text2odu-event-queue)
      (start-text2odu-dispatcher)
      (sleep 0.1)
      (shutdown-text2odu-dispatcher)
      (sleep 0.1)
      ))
  nil)

(test2)

