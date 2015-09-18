;; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-
;; File related to sending editing primitives to tcl. 
(in-package :oduvanchik)


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

(defun send-mark-to-clcon_text (mark remote-name)
  "For mark, creates corresponding mark in text with name = remote-name, or moves an existing mark"
  (check-type remote-name (or symbol string))
  (check-type mark mark)
  (let ((line (mark-line mark)))
    (assert line)
    (let ((offset-of-line (offset-of-line line)))
      (format *terminal-io* "send-mark-to-clcon_text: ~A.~A ~A" offset-of-line (mark-charpos mark) remote-name)
      )
    ))

#| tests: 
 (oduvanchik::send-mark-to-clcon_text (oduvanchik::buffer-start-mark (oduvanchik::current-buffer)) "bb")
 (oduvanchik::send-mark-to-clcon_text (oduvanchik::buffer-end-mark (oduvanchik::current-buffer)) "bb")
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
