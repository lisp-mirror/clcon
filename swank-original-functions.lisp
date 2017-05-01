; -*- coding: utf-8 ; Encoding: utf-8 ; system :clcon-server -*- 
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


(defslimefun swank-original-ed-in-emacs (&optional what)
  "Edit WHAT in Emacs.

WHAT can be:
  A pathname or a string,
  A list (PATHNAME-OR-STRING &key LINE COLUMN POSITION),
  A function name (symbol or cons),
  NIL. "
  (flet ((canonicalize-filename (filename)
           (pathname-to-filename (or (probe-file filename) filename))))
    (let ((target 
           (etypecase what
             (null nil)
             ((or string pathname) 
              `(:filename ,(canonicalize-filename what)))
             ((cons (or string pathname) *)
              `(:filename ,(canonicalize-filename (car what)) ,@(cdr what)))
             ((or symbol cons)
              `(:function-name ,(prin1-to-string what))))))
      (cond (*emacs-connection* (send-oob-to-emacs `(:ed ,target)))
            ((default-connection)
             (with-connection ((default-connection))
               (send-oob-to-emacs `(:ed ,target))))
            (t (error "No connection"))))))
