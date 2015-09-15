; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-

(in-package :clco-oduvanchik-key-bindings)
(named-readtables::in-readtable :oduvanchik-ext-readtable)

(defvar *text2odu-key-event-f8* #k"F8" "Key event we will use to put text2odu events to keyboard buffer")
(defvar *f17-key-event* #k"F17" "We use it for exit command")
; (defvar *f18-key-event* #k"F18" "We use it to evaluate calls from tcl")


(defvar *text2odu-dispatcher-to-editor-queue-lock* (bt:make-lock "clcon-server::*text2odu-dispatcher-to-editor-queue-lock*"))
(defvar *text2odu-dispatcher-to-editor-queue* (list) "Queue to pass events from *text2odu-event-queue* to editor")

; FIXME merge it with lisp's atomic-queue.
(defun text2odu-dispatcher-to-editor-queue-reset ()
  (bt:with-lock-held (*text2odu-dispatcher-to-editor-queue-lock*)
    (setf *text2odu-dispatcher-to-editor-queue* nil)))

(defun text2odu-dispatcher-to-editor-queue-put (e)
  (bt:with-lock-held (*text2odu-dispatcher-to-editor-queue-lock*)
    (budden-tools:nenqueue *text2odu-dispatcher-to-editor-queue* e)))

(defun text2odu-dispatcher-to-editor-queue-pop ()
  (bt:with-lock-held (*text2odu-dispatcher-to-editor-queue-lock*)
    (pop *text2odu-dispatcher-to-editor-queue*)))

(defun set-clco-oduvanchik-key-bindings ()
  ; (oduvanchik::bind-key "Beginning of line" #k"F8")
  (oduvanchik::bind-key "Exit Oduvanchik" *f17-key-event*)
  (oduvanchik::bind-key "evaltext2oduevent" *f8-key-event*)
  )

(set-clco-oduvanchik-key-bindings)


