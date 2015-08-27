; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-

; We don't want write all code in tcl. 
; here we introduce some security risk where
; we allow server to send tcl code for client to execute
; This is important when we connect from IDE to remote server
; there are ways to reduce risk:
; 1. Use safe interpreters on tcl side
; 2. Check code on tcl side

(in-package :clco)

(defun eval-in-tcl (code &optional (nowait t))
  "Eval FORM in tcl. Code is a string"
  (cond (nowait 
         (swank::send-to-emacs `(:eval-no-wait ,code)))
        (t
         (error "eval-in-tcl :wait not implemented. See swank::eval-in-emacs for code"
                ))))
