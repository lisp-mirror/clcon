; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-

(in-package :clco-oduvanchik-key-bindings)
(named-readtables::in-readtable :oduvanchik-ext-readtable)


(defvar *text2odu-key-event-f8* #k"F8" "Key event we will use to put text2odu events to keyboard buffer")
(defvar *f17-key-event* #k"F17" "We use it for exit command")
(defvar *f18-key-event* #k"F18")

; for single chars, use (oduvanchik-ext:char-key-event #\x)


;(defcommand "getclconevent" (p)
;    "Get single clcon event"
;    "Get single clcon event"
;  убираем диспетчер?
                                        ;  )

(defun set-clco-oduvanchik-key-bindings ()
  (oduvanchik::bind-key "Beginning of line" #k"F8")
  (oduvanchik::bind-key "Exit Oduvanchik" *f17-key-event*)
  )

(set-clco-oduvanchik-key-bindings)


