;; -*- Encoding: utf-8; Coding: utf-8 ; -*-
;; Скрипт сборки образа - загружается после zagruzitq-server--clcon.lisp
;; (C) Denis Budyak 2015
;; MIT License

(in-package :cl-user)

(eval-when (:compile-toplevel)
  (error "Не компилируй ~S, загружай его с помощью LOAD" *compile-file-truename*))

;;;;;;;;;;;;;;;;;; Starting server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Возможно, в этом образе есть ещё один сервер SWANK, нужный для связи с EMACS
(swank:create-server :port *clcon-swank-port* :dont-close t)

;;;;;;;;;;;;;;;;;; Starting editor backend ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(clco:start-oduvanchik)

;;;;;;;;;;;;;;;;;; Starting editor frontend ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(zapustitq-klienta--clcon)


