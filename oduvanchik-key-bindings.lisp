; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-

(in-package :clco-oduvanchik-key-bindings)
(named-readtables::in-readtable :oduvanchik-ext-readtable)


(defvar *f8-key-event* #k"F8")
(defvar *f17-key-event* #k"F17")
(defvar *f18-key-event* #k"F18")

; for single chars, use (oduvanchik-ext:char-key-event #\x)


(defcommand "getclconevent" (p)
    "Get single clcon event"
    "Get single clcon event"
  убираем диспетчер?
  )

(defun set-clco-oduvanchik-key-bindings ()
  (oduvanchik::bind-key "Beginning of line" #k"F8")
  )

(set-clco-oduvanchik-key-bindings)
