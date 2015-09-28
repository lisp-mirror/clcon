;; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-
;; evaluation of text2odu events. Takes place in editor thread.
(in-package :oduvanchik)

(defun set-mark-to-row-and-col (mark row col)
  "Row and col a given in clcon_text 's coordinate system"
  (let* ((line (mark-line mark))
         (buffer (line-buffer line)))
    (move-mark mark (buffer-start-mark buffer))
    (line-offset mark (- row 1) col)))

(defmacro with-mark-in-row-col ((name row-col) &body body)
  "Evaluates body in the scope where name is bound to right-inserting temporary mark placed at clco::row-col struct"
  ;(beginning-of-buffer-command)
                                        ;(line-next
  (alexandria:once-only (row-col)
    `(with-mark ((,name (buffer-start-mark (current-buffer)) :right-inserting))
       (set-mark-to-row-and-col ,name (clco::row-col-row ,row-col) (clco::row-col-col ,row-col))
       ,@body)))

(defun delete-characters-between-marks (beg end)
  "Code stolen from delete-characters"
  (cond
    ((mark> beg end)
     (error "mark>= beg end")
     )
    ((mark= beg end)
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

(defun eval-before-tcl-text-delete (e)
  (etypecase e
    (clcon-server::text2odu-event
     (let ((ebeg (clcon-server::text2odu-event-beg e))
           (eend (clcon-server::text2odu-event-end e)))
       (with-mark-in-row-col (beg ebeg)
         (cond
           (eend
            (with-mark-in-row-col (end eend)
              (delete-characters-between-marks beg end)))
           (t
            (delete-characters beg 1)
            ))))))
  t)

(defun eval-construct-backend-buffer (e)
  (let ((b (current-buffer)))
    (delete-characters-between-marks
     (buffer-start-mark b)
     (buffer-end-mark b))
    ; b must be current here
    (lisp-mode-command nil)
    (setf (buffer-name b) (clco::text2odu-event-clcon_text-pathname e))
    ))


(defun send-buffer-point-to-clcon_text (buffer)
  "Sets insert mark at clcon_text to buffer-point of buffer"
  (let ((clcon_text (oi::buffer-to-clcon_text buffer))
        (p (buffer-point buffer)))
    (oi::send-mark-to-clcon_text clcon_text p :remote-name "insert")
    ))

(defun nop (&rest args) (declare (ignore args)))

(defun eval-for-emacs-clone-for-oduvanchik (fn id)
  "Copy-paste from swank::eval-for-emacs with some additions"
  (let (ok result condition)
    ))


(defun call-oduvanchik-fn-internal (fn clcon_text buffer)
  (let ((oduvanchik-internals::*do-editing-on-tcl-side* t))
    (funcall fn nil)
    (clco::compare-clcon_text-and-oduvanchik-buffer-contents clcon_text)
    )
  (odu::send-buffer-point-to-clcon_text buffer)
  )

(defun call-oduvanchik-function-with-clcon_text (e)
  "see swank:eval-for-emacs as an example of error handling"
  (let* ((clcon_text (clco::text2odu-event-clcon_text-pathname e))
         (cur-row-col (clco::text2odu-event-beg e))
         (connection (clco::text2odu-event-swank-connection e))
         (buffer (oi::clcon_text-to-buffer clcon_text))
         (fn-name (clco::text2odu-event-string e))
         (fn (find-symbol (string-upcase fn-name) :oduvanchik))
         (cont (clco::text2odu-event-far_tcl_cont_id e))
         )
    (assert (and
             (eq (symbol-package fn) (find-package :oduvanchik)) ; security limitation
             (fboundp fn)) () "Symbol ~S (~S) not found, funbound or have home-package different from :oduvanchik" fn-name fn)
    (swank::with-connection (connection) 
      (use-buffer buffer
        (odu::set-mark-to-row-and-col (current-point)
                                      (clco::row-col-row cur-row-col)
                                      (clco::row-col-col cur-row-col))
        ;; (oi::sync-mark-from-clcon_text clcon_text (current-point) "insert")
        ;; known functions are indent-new-line-command and new-line-command
        (swank::eval-for-emacs `(call-oduvanchik-fn-internal ',fn ',clcon_text ',buffer) :common-lisp-user cont)
        
        nil
        ))))

; for single chars, use (oduvanchik-ext:char-key-event #\x)
(defun eval-text2odu-event (e)
  "This code is executed inside a command. Transrom text2odu events to oduvanchik function calls"
  (etypecase e
    (clcon-server::text2odu-event
     (ecase (clco::text2odu-event-kind e)
       (clco::construct-backend-buffer
        (eval-construct-backend-buffer e))
       (clco::before-tcl-text-insert
        (eval-before-tcl-text-insert e))
       (clco::before-tcl-text-delete
        (eval-before-tcl-text-delete e)
        ; (warn "ignoring before-tcl-text-delete event")
        )
       (clco::destroy-backend-buffer
        (warn "ignoring destroy-backend-buffer event")
        )
       (clco::shutdown-text2odu-dispatcher
        (error "clco::shutdown-text2odu-dispatcher event must not arrive here")
        )
       (clco::call-oduvanchik-function-with-clcon_text
        (call-oduvanchik-function-with-clcon_text e))
       ))))
     

(defun eval-pending-text2odu-events (&key (hang t))
  (loop
     (let ((e (clco-oduvanchik-key-bindings::text2odu-dispatcher-to-editor-queue-pop :hang hang)))
       (cond
         (e (eval-text2odu-event e))
         (t (return))
         ))))


(oduvanchik::defcommand "evaltext2oduevent" (p)
    "Get and eval single clcon event"
    "Get and eval single clcon event"
  (eval-pending-text2odu-events :hang p)
  ;(oduvanchik.x11::kick-oduvanchik)
  )



