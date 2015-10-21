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


(defun eval-highlight-single-line (e)
  "See also clco::notify-highlight-single-line"
  (let* ((cmd (format nil "::edt::ApplyHighlightToLine ~A ~A"
                      (highlight-event-clcon_text-pathname e)
                      (--> e string))
           ))
    (swank::with-connection ((--> e swank-connection))
      ;; we should carefully synchronize them indeed
      (clco:eval-in-tcl cmd :nowait nil))))

(defun highlight-dispatcher-thread-function ()
  (loop
     (let ((e (pop-highlight-event-queue)))
       (when e
         (ecase (highlight-event-kind e)
           (shutdown-highlight-dispatcher
            (setf *highlight-dispatcher-thread* nil)
            (return-from highlight-dispatcher-thread-function nil))
           (highlight-single-line
            (eval-highlight-single-line e)
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



