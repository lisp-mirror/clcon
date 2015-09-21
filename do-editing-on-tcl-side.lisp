;; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-
;; File related to sending editing primitives to tcl. 
(in-package :oduvanchik-internals)


(defun offset-of-line (line)
  "Line is an editor's line object, e.g. received as mark-line function. Return line number, at which it is located, starting from 1"
  (let* ((buffer (line-buffer line))
         (start-mark (buffer-start-mark buffer))
         (start-line (mark-line start-mark))
         (current-line start-line)
         (result 1))
    (loop
       (when (eq current-line line)
         (return result))
       (let ((next (line-next current-line)))
         (assert next)
         (incf result)
         (setf current-line next)
         ))))

(defmethod call-tcl-editing (code)
  "See gf documentation"
  (assert *do-editing-on-tcl-side*)
  (let ((*do-editing-on-tcl-side* nil))
    (clco:eval-in-tcl code :nowait nil)))

(defmethod call-tcl-simple (code)
  "See generic-function documentation"
  (clco:eval-in-tcl code :nowait nil)
  )

(defmethod tcl-code-for-insert-character (clcon_text remote-mark-name character)
  "See generic-function docstring"
  (let ((tcl-character (cl-tk:tcl-escape (make-string 1 :initial-element character))))
    (format nil "~A insert ~A ~A" clcon_text remote-mark-name tcl-character)))

(defmethod tcl-code-for-insert-string (clcon_text remote-mark-name string)
  "See generic-function docstring"
  (format nil "~A insert ~A ~A" clcon_text remote-mark-name (cl-tk:tcl-escape string)))

(defmethod tcl-code-for-delete-region (clcon_text remote-beg-mark-name remote-end-mark-name)
  "See generic-function docstring"
  (format nil "~A delete ~A ~A" clcon_text remote-beg-mark-name remote-end-mark-name)
  )

#| tests: 
 (oduvanchik::send-mark-to-clcon_text ### (oduvanchik::buffer-start-mark (oduvanchik::current-buffer)) :remote-name "bb")
 (oduvanchik::send-mark-to-clcon_text ### (oduvanchik::buffer-end-mark (oduvanchik::current-buffer)) :remote-name "bb")
|#

#|(defmacro with-mark-in-row-col ((name row-col) &body body)
  "Evaluates body in the scope where name is bound to right-inserting temporary mark placed at clco::row-col struct"
  ;(beginning-of-buffer-command)
                                        ;(line-next
  (alexandria:once-only (row-col)
    `(with-mark ((,name (buffer-start-mark (current-buffer)) :right-inserting))
       (line-offset ,name (- (clco::row-col-row ,row-col) 1) (clco::row-col-col ,row-col))
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

|#
