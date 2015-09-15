; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-

(in-package :oduvanchik)

(defmacro with-mark-in-row-col ((name row-col) &body body)
  "Evaluates body in the scope where name is bound to right-inserting temporary mark placed at clco::row-col struct"
  ;(beginning-of-buffer-command)
                                        ;(line-next
  (alexandria:once-only (row-col)
    `(with-mark ((,name (buffer-start-mark (current-buffer)) :right-inserting))
       (line-offset ,name (clco::row-col-row ,row-col) (clco::row-col-col ,row-col))
       ,@body)))

(defmethod delete-characters-between-marks (beg end)
  "Code stolen from delete-characters"
  (cond
    ((mark>= beg end)
     (error "mark>= beg end")
     t)
    (t
     (setf (region-start oduvanchik-internals::*internal-temp-region*) beg
           (region-end oduvanchik-internals::*internal-temp-region*) end)
     (delete-region oduvanchik-internals::*internal-temp-region*)
     t)))

(defun eval-before-tcl-text-insert (e)
  (etypecase e
    (clcon-server::text2odu-event
     (with-mark-in-row-col (beg (clcon-server::text2odu-event-beg e))
       (insert-string beg (clcon-server::text2odu-event-string e))))))

(defun eval-construct-backend-buffer (e)
  (declare (ignore e))
  (delete-characters-between-marks
   (buffer-start-mark (current-buffer))
   (buffer-end-mark (current-buffer)))
  )
  

; for single chars, use (oduvanchik-ext:char-key-event #\x)
(defun eval-text2odu-event (e)
  "This code is executed inside a command. Transrom text2odu events to oduvanchik function calls"
  (format t "~%eval-text2odu-event: received ~S" e)
  (etypecase e
    (clcon-server::text2odu-event
     (ecase (clco::text2odu-event-kind e)
       (clco::construct-backend-buffer
        (eval-construct-backend-buffer e))
       (clco::before-tcl-text-insert
        (eval-before-tcl-text-insert e))
       (clco::before-tcl-text-delete
        (warn "ignoring before-tcl-text-delete event")
        )
       (clco::destroy-backend-buffer
        (warn "ignoring destroy-backend-buffer event")
        )
       (clco::shutdown-text2odu-dispatcher
        (error "clco::shutdown-text2odu-dispatcher event must not arrive here")
        )))))
     


(oduvanchik::defcommand "evaltext2oduevent" (p)
    "Get and eval single clcon event"
    "Get and eval single clcon event"
  (loop 
     (let ((e (clco-oduvanchik-key-bindings::text2odu-dispatcher-to-editor-queue-pop)))
       (cond
         (e (eval-text2odu-event e))
         (t (return))
         ))))



