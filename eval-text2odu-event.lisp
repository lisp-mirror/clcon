;; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-
;; evaluation of text2odu events. Takes place in editor thread.
;; see doc/text2odu.md
(in-package :oduvanchik)


(defun check-mark-is-at-row-and-col (mark row col)
  (multiple-value-bind (r c)
      (oi::mark-row-and-col mark)
    (unless (= row r) 
            (break "Mark ~S was expected to be at row ~A, but it is at ~A" mark row r))
    (unless (= col c) 
            (break "Mark ~S was expected to be at column ~A, but it is at ~A" mark col c))
    ))

(defun set-mark-to-row-and-col (mark row col)
  "Row and col a given in clcon_text 's coordinate system"
  (check-something-ok mark)
  (let* ((line (mark-line mark))
         (buffer (line-buffer line)))
    (move-mark mark (buffer-start-mark buffer))
    (line-offset mark (- row 1) col)
    (check-mark-is-at-row-and-col mark row col)
    (check-something-ok mark)
    nil
    ))

(defmacro with-mark-in-row-col ((name row-col) &body body)
  "Evaluates body in the scope where name is bound to right-inserting temporary mark placed at clco::row-col struct. See also oi::mark-row-and-col"
  ;(beginning-of-buffer-command)
                                        ;(line-next
  (alexandria:once-only (row-col)
    `(with-mark ((,name (buffer-start-mark (current-buffer)) :right-inserting))
       (set-mark-to-row-and-col ,name (clco::row-col-row ,row-col) (clco::row-col-col ,row-col))
       (check-something-ok ,name)
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

(defun eval-notify-oduvan-cursor-moved (e)
  "Cursor moved. See clco::ncm"
  (etypecase e
    (clcon-server::text2odu-event
     (let* ((clcon_text (clcon-server::text2odu-event-clcon_text-pathname e))
            (buffer (oi::clcon_text-to-buffer clcon_text)))
       (assert buffer)
       (use-buffer buffer 
         (with-mark-in-row-col (cursor-point (clcon-server::text2odu-event-beg e))
           (move-mark (current-point) cursor-point)
           ;; this is what it all what done for
           (oduvan-invisible-maybe-highlight-open-parens)
           (maybe-send-package-to-tcl clcon_text)
           (maybe-send-readtable-to-tcl clcon_text)
           (maybe-send-mode-to-tcl clcon_text)
           (check-something-ok cursor-point)
           )))))
  nil)

(defun eval-before-tcl-text-insert (e)
  "See clco::nti"  
  (etypecase e
    (clcon-server::text2odu-event
     (let* ((clcon_text (clcon-server::text2odu-event-clcon_text-pathname e))
            (buffer (oi::clcon_text-to-buffer clcon_text)))
       (assert buffer)
       (use-buffer buffer 
         (odu::check-something-ok)
         (with-mark-in-row-col (beg (clcon-server::text2odu-event-beg e))
           (insert-string beg (clcon-server::text2odu-event-string e))
           (odu::check-something-ok beg))))))
  nil
  )

(defun eval-before-tcl-text-delete (e)
  "See clco::notify-oduvan-tcl-text-delete"
  (etypecase e
    (clcon-server::text2odu-event
     (let* ((ebeg (clcon-server::text2odu-event-beg e))
            (eend (clcon-server::text2odu-event-end e))
            (clcon_text (clcon-server::text2odu-event-clcon_text-pathname e))
            (buffer (oi::clcon_text-to-buffer clcon_text)))
       (assert buffer)
       (use-buffer buffer
         (with-mark-in-row-col (beg ebeg)
           (cond
             (eend
              (with-mark-in-row-col (end eend)
                (delete-characters-between-marks beg end)))
             (t
              (delete-characters beg 1)
              ))
           (check-something-ok beg)
           )))))
  t)

(defun find-buffer-by-name (buffer-name)
  (getstring buffer-name *buffer-names*))

(defun delete-old-buffer (buffer-name)
  "Find buffer with that name and delete it. We rely on the fact we have one window only so we can switch to safe haven in the only window"
  (let ((b (find-buffer-by-name buffer-name)))
    (when b
      (switch-to-safe-haven-buffer)
      (delete-buffer b))
    ))

(defun switch-to-safe-haven-buffer ()
  "Switches to 'safe haven' buffer creating it if needed"
  (let* ((name "*safe haven*")
         (existing (find-buffer-by-name name))
         (result (or existing (make-buffer name))))
    (change-to-buffer result)
    (unless existing
      (auto-save-mode-command 0))
    ))

(defhvar "Swank connection" "Always nil (hopefully)")

(defun eval-construct-backend-buffer (e)
  (let* ((buffer-name (clco::text2odu-event-clcon_text-pathname e)))         
    (delete-old-buffer buffer-name)
    (let* ((b (make-buffer buffer-name)))
      (change-to-buffer b)
      (oduvanchik-interface:defhvar "Swank Connection" "Ugugu" :buffer b)
      (auto-save-mode-command 0)
      (lisp-mode-command nil)
      (budden-tools::show-expr (clco::text2odu-event-swank-connection e))
      (setf (oi::variable-value 'odu::swank-connection :buffer b)
            (clco::text2odu-event-swank-connection e))
      )))

(defun eval-destroy-backend-buffer (e)
  (let* ((buffer-name (clco::text2odu-event-clcon_text-pathname e))
         (old-buffer (find-buffer-by-name buffer-name)))
    (when old-buffer
      (switch-to-safe-haven-buffer)
      (delete-buffer old-buffer))))


(defun send-buffer-point-to-clcon_text (buffer)
  "Sets insert mark at clcon_text to buffer-point of buffer"
  (let ((clcon_text (oi::buffer-to-clcon_text buffer))
        (p (buffer-point buffer)))
    (assert clcon_text () "~S This is not a clcon_text backend buffer" buffer)
    (oi::send-mark-to-clcon_text clcon_text p :remote-name "insert")
    (odu::check-something-ok p)
    ))

(defun nop (&rest args) (declare (ignore args)))

(defun call-oduvanchik-fn-internal (fn-funcall-list options clcon_text buffer)
  "Called from eval-call-oduvanchik-function-with-clcon_text via eval-for-emacs"
  (declare (ignorable clcon_text))
  (odu::check-something-ok)
  (let (result)      
    (let* ((oduvanchik-internals::*do-editing-on-tcl-side* t))
      (cond
        ((equal (getf options :send_selection) "1")
         (oi::transfer-selection-from-clcon_text buffer)))
      (setf result (apply 'funcall fn-funcall-list))
      )
    (odu::send-buffer-point-to-clcon_text buffer)
    (odu::check-something-ok)
    result
    ))

;; ;; Reads "(\"fn-name\" arg1 ... argN)" and converts to
;; ;; (fn arg1 argN), where fn must be a function from :odu package
;; (defun parse-ecofwct-string-to-fn-and-args (e)
;;   "See comment near fn definition"
;;   (let* (
;;          (fn-name-and-args (clco::text2odu-event-fn-and-args e))
;;          (fn-name (first fn-name-and-args))
;;          (fn (find-symbol (string-upcase fn-name) :oduvanchik))
;;          )
;;     (assert (and
;;              (eq (symbol-package fn) (find-package :oduvanchik)) ; security limitation
;;              (fboundp fn)) () "Symbol ~S (~S) not found, funbound or have home-package different from :oduvanchik" fn-name fn)
;;     (list fn (cdr fn-name-and-args))))


(defun parse-ecofwct-options (options)
  (let* (
         (ov-pairs (budden-tools:splice-list options))
         result
         )
    (dolist (ov-pair ov-pairs)
      (destructuring-bind (option value) ov-pair
        (let ((sym-option 
               (alexandria:switch (option :test 'string=)
                 ("send_selection" :send_selection)
                 (t (error "Wrong option ~S in parse-ecofwct-options" option)))))
          (push value result)
          (push sym-option result))))
    result))

; test
(assert (equalp (parse-ecofwct-options '("send_selection" "t"))
                '(:send_selection "t")))
        
(defun eval-call-oduvanchik-function-with-clcon_text (e)
  "see swank:eval-for-emacs as an example of error handling. See clco:call-oduvanchik-function-with-clcon_text"
  (let* ((clcon_text (clco::text2odu-event-clcon_text-pathname e))
         (cur-row-col (clco::text2odu-event-beg e))
         (connection (clco::text2odu-event-swank-connection e))
         (buffer (oi::clcon_text-to-buffer clcon_text))
         (fn-funcall-list (clco::text2odu-event-fn-and-args e))
         (fn (car fn-funcall-list))
         (options (parse-ecofwct-options (clco::text2odu-event-options e)))
         (cont (clco::text2odu-event-far_tcl_cont_id e))
         )
    (assert (and
             (eq (symbol-package fn) (find-package :oduvanchik)) ; security limitation
             (fboundp fn)) () "Symbol ~S funbound or have home-package different from :oduvanchik" fn)

    (swank::with-connection (connection) 
      (use-buffer buffer
        (odu::set-mark-to-row-and-col (current-point)
                                      (clco::row-col-row cur-row-col)
                                      (clco::row-col-col cur-row-col))
        ;; (oi::sync-mark-from-clcon_text clcon_text (current-point) "insert")
        ;; known functions are indent-new-line-command and new-line-command
        (swank::eval-for-emacs `(call-oduvanchik-fn-internal ',fn-funcall-list ',options ',clcon_text ',buffer) :common-lisp-user nil cont)
        (check-something-ok)
        nil ; real return is done via continuations machinery
        ))))

(defun eval-order-call-oduvanchik-from-itself (e)
  "See clco::order-call-oduvanchik-from-itself"
  (let* ((fn-funcall-list (clco::text2odu-event-fn-and-args e))
         (fn (car fn-funcall-list)))
    (check-something-ok)
    (assert (and
             (eq (symbol-package fn) (find-package :oduvanchik)) ; security limitation
             (fboundp fn)) () "Symbol ~S funbound or have home-package different from :oduvanchik" fn)
    (apply #'funcall fn-funcall-list))
    (check-something-ok)
  nil ; nowhere to return
  )


; for single chars, use (oduvanchik-ext:char-key-event #\x)
(defun eval-text2odu-event (e)
  "This code is executed inside a command. Transform text2odu events to oduvanchik function calls"
  (check-something-ok)
  (etypecase e
    (clcon-server::text2odu-event
     (ecase (clco::text2odu-event-kind e)
       (clco::cursor-moved
        (eval-notify-oduvan-cursor-moved e))
       (clco::before-tcl-text-insert
        (eval-before-tcl-text-insert e))
       (clco::before-tcl-text-delete
        (eval-before-tcl-text-delete e)
        ; (warn "ignoring before-tcl-text-delete event")
        )
       (clco::construct-backend-buffer
        (eval-construct-backend-buffer e))
       (clco::destroy-backend-buffer
        (eval-destroy-backend-buffer e)
        )
       (clco::shutdown-text2odu-dispatcher
        (error "clco::shutdown-text2odu-dispatcher event must not arrive here")
        )
       (clco::call-oduvanchik-function-with-clcon_text
        (eval-call-oduvanchik-function-with-clcon_text e))
       (clco::call-oduvanchik-from-itself
        (eval-order-call-oduvanchik-from-itself e))
       )))
  (check-something-ok)
  )

(defun assert-we-are-in-oduvanchik-thread ()
  (assert (string= 
           (bt:thread-name (bt:current-thread))
           "Oduvanchik")))

(defun eval-pending-text2odu-events (&key (hang t))
  (assert-we-are-in-oduvanchik-thread)
  (loop
     (let ((e (clco-oduvanchik-key-bindings::text2odu-dispatcher-to-editor-queue-pop :hang hang)))
       (cond
         (e (eval-text2odu-event e))
         (t (return))
         ))))


(oduvanchik::defcommand "evaltext2oduevent" (p)
    "Get and eval single clcon event"
    "Get and eval single clcon event"
  (check-something-ok)
  (eval-pending-text2odu-events :hang p)
  (check-something-ok)
  ;(oduvanchik.x11::kick-oduvanchik)
  )



