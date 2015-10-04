; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-


(in-package :clco)

(declaim (optimize debug))

(defun pop-highlight-event-queue ()
  "Made after example pop-text2odu-event-queue"
  (bt:with-lock-held (*highlight-event-queue-lock*)
    (loop until *highlight-event-queue*
       do (or (bt:condition-wait
               *highlight-condition-variable*
               *highlight-event-queue-lock*
               )
              ;; Lock not held, must unwind without touching *data*.
              (return-from pop-highlight-event-queue nil)))
    (pop *highlight-event-queue*)))


(defun highlight-dispatcher-thread-function ()
  (loop
     (let ((e (pop-highlight-event-queue)))
       (when e
         (cond
           ((eq (--> e kind) 'shutdown-highlight-dispatcher)
            (setf *highlight-dispatcher-thread* nil)
            (return-from highlight-dispatcher-thread-function nil))
           (t
            ;(format t "~%Sending real event ~S to oduvanchik keyboard buffer!" e)
            ;(clco-oduvanchik-key-bindings::highlight-dispatcher-to-editor-queue-put e)
            (warn "Send highlight ~S to tcl manually!" e)
            #| (let* ((cmd (with-output-to-string (ou)
                        (format ou "::edt::ApplyHighlightToLine ~A " clcon_text)
                        (encode-marks-for-line line ou)
                        )))
                                        ;(format *trace-output* "~S" cmd)
            (swank::with-connection (connection)
              ;; we should carefully synchronize them indeed
              (clco:eval-in-tcl cmd :nowait nil))) |#
            ))))))


(defun start-highlight-dispatcher ()
  "See also shutdown-highlight-dispatcher"
  (bt:with-lock-held (*highlight-dispatcher-start-shutdown-lock*)
    (assert (null *highlight-dispatcher-thread*))
    (setf *highlight-dispatcher-thread*
          (bt:make-thread #'highlight-dispatcher-thread-function
                          :name
                          "highlight-dispatcher-thread-function"
                          )))
  )

(defun shutdown-highlight-dispatcher-on-oduvanchik-exit-hook ()
  (shutdown-highlight-dispatcher))



