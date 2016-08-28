; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server -*- 
(in-package :swank)

;; there are functions we patch. Their copies with swank-original- prefix are given here
;; we could also use decorate-function

(defun swank-original-wait-for-event/event-loop (connection pattern timeout)
  (assert (or (not timeout) (eq timeout t)))
  (loop 
   (check-slime-interrupts)
   (let ((event (poll-for-event connection pattern)))
     (when event (return (car event))))
   (let ((events-enqueued (sconn.events-enqueued connection))
         (ready (wait-for-input (list (current-socket-io)) timeout)))
     (cond ((and timeout (not ready))
            (return (values nil t)))
           ((or (/= events-enqueued (sconn.events-enqueued connection))
                (eq ready :interrupt))
            ;; rescan event queue, interrupts may enqueue new events 
            )
           (t
            (assert (equal ready (list (current-socket-io))))
            (dispatch-event connection
                            (decode-message (current-socket-io))))))))

(defun swank-original-read-loop (connection)
  (let ((input-stream (connection.socket-io connection))
        (control-thread (mconn.control-thread connection)))
    (with-swank-error-handler (connection)
      (loop (send control-thread (decode-message input-stream))))))

(defslimefun swank-original-backtrace (start end)
  "Return a list ((I FRAME PLIST) ...) of frames from START to END.

I is an integer, and can be used to reference the corresponding frame
from Emacs; FRAME is a string representation of an implementation's
frame."
  (loop for frame in (compute-backtrace start end)
        for i from start collect 
        (list* i (frame-to-string frame)
               (ecase (frame-restartable-p frame)
                 ((nil) nil)
                 ((t) `((:restartable t)))))))