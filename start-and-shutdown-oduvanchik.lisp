; -*- coding: utf-8 ; Encoding: utf-8 ; system :clcon-server ; -*-
; clco:start-oduvanchik, clco::shutdown-oduvanchik-via-keyboard-buffer

(in-package :clco)

(declaim (optimize debug))

(defun shutdown-text2odu-dispatcher-on-oduvanchik-exit-hook ()
  (shutdown-text2odu-dispatcher))

(defun start-oduvanchik ()
  (declare (special oduvanchik::exit-hook oduvanchik::entry-hook))
  (let ((entered nil)
        (entered-lock (bt:make-lock "Oduvanchik entry signal lock"))
        )
    (reset-text2odu-event-queue)
    (oduvanchik-internals::remove-all-hooks oduvanchik::exit-hook)


    ;; if previous run crashed, this can be useful
    (ignore-errors (shutdown-text2odu-dispatcher))
    (ignore-errors (shutdown-highlight-dispatcher))

    (oduvanchik::add-hook oduvanchik::exit-hook
                          'shutdown-text2odu-dispatcher-on-oduvanchik-exit-hook)
    (oduvanchik::add-hook oduvanchik::exit-hook
                          'shutdown-highlight-dispatcher-on-oduvanchik-exit-hook)

    (oduvanchik::add-hook
     oduvanchik::entry-hook
     (lambda () (bt:with-lock-held (entered-lock) (setf entered t))))
    
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
    (start-highlight-dispatcher)
    #+clcon (setf oi::*beep-function* 'odu::bell-with-tcl)
    #+clcon (odu::decorate-oduvanchik-message)
    ))

(defun shutdown-oduvanchik-via-keyboard-buffer ()
  (podsunutq-event clco-oduvanchik-key-bindings::*f17-key-event*))

; need separate file for this
(defun test1 ()
  (with-output-to-string (ddd #|*standard-output*|#)
    (start-oduvanchik)
    (sleep 0.5)
    (notify-oduvan-construct-backend-buffer "a")
    (sleep 0.1)
    (nti "a" "1.0" "(defun ugu () 'ugu)")
    (sleep 0.1)
    (notify-oduvan-destroy-backend-buffer "a")
    (shutdown-oduvanchik-via-keyboard-buffer)
    )
  ; (print "You need to move mouse over oduvanchik's window to close editor. Waiting...")
  (loop
     (unless (and *text2odu-dispatcher-thread* *highlight-dispatcher-thread*) (return))
     (sleep 0.5)
     (format t "."))
  nil
  )

; (test1)

(defun test2 ()
  (with-output-to-string (*standard-output*)
    (dotimes (i 2)
      (ignore-errors (shutdown-text2odu-dispatcher))
      (reset-text2odu-event-queue)
      (start-text2odu-dispatcher)
      (sleep 0.1)
      (shutdown-text2odu-dispatcher)
      (sleep 0.1)
      ))
  nil)

; (test2)

