; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-

; We don't want write all code in tcl. 
; here we introduce some security risk where
; we allow server to send tcl code for client to execute
; This is important when we connect from IDE to remote server
; there are ways to reduce risk:
; 1. Use safe interpreters on tcl side
; 2. Check code on tcl side
; In general, avoid using this functionality. It is for quick-and-dirty hacks, 


(in-package :clco)

(defun eval-in-tcl (code &key (nowait t))
  "Eval FORM in tcl. Code is a string. Must be called from worker thread or in the scope of with-connection. Note that ping-pong protection from piling up of events on one side is disabled (TODO report this issue to slime-devel)"
  (cond (nowait 
         (swank::send-to-emacs `(:eval-no-wait ,code)))
        (t
         (force-output)
         (let ((tag (swank::make-tag)))
	   (swank::send-to-emacs `(:eval ,(swank::current-thread-id) ,tag 
                                        ,code))
	   (let ((value (caddr (swank::wait-for-event `(:emacs-return ,tag result)))))
	     (swank::dcase value
	       ((:ok value) value)
               ((:error kind . data) (error "~a: ~{~a~}" kind data))
	       ((:abort) (abort))))))))
