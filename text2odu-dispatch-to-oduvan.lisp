; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-
; see also text2odu.tcl, text2odu.lisp

; this file is only built if :clcon-oduvan feature present
; threads: we do not assume in which thread notification functions are called. 

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


(defun podsunutq-event (key-event)
  "Puts fake event onto oduvan event queue as if it came from keyboard. Clone of oduvanchik-internals::q-event . Note we do not set hunk"
  (bt:with-lock-held (oduvanchik-internals::*q-event-lock*) ; budden
    (oduvanchik-ext:without-interrupts
      (let* (
             ; stolen from oduvanchik-internals::window-input-handler
             (stream oduvanchik-internals::*editor-input*)
             (new (make-fake-keyboard-event key-event))
             (tail (oduvanchik-internals::editor-input-tail stream)))
        (setf (oduvanchik-internals::input-event-next tail) new)
        (setf (oduvanchik-internals::editor-input-tail stream) new)))))

; (defun new-event (key-event x y hunk next &optional unread-p)
(defun make-fake-keyboard-event (key-event)
  "Just trying to put some event as if it was from the keyboard"
  (oduvanchik-internals::new-event
   ; (oduvanchik-ext:char-key-event #\!)  ; stolen from hi::translate-tty-event
   key-event 
   11
   3
   nil ; hunk was smth like #<oduvanchik.x11::x11-hunk nil+374, "Main" {DDC03F1}>
   nil
   nil
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
            (podsunutq-event
             clco-oduvanchik-key-bindings:*text2odu-key-event-f8*
             )))))))


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



