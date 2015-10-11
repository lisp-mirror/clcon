;; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-
;; File related to sending editing primitives to tcl. 
(in-package :oduvanchik-internals)

(defun remote-mark-name (mark default)
  "If mark is buffer-point, return just 'insert'. Otherwise, return default"
  (let* ((line (mark-line mark))
         (buffer (line-buffer line))
         (point (buffer-point buffer)))
    (if (eq mark point) "insert" default)))    

(defmethod sync-mark-from-clcon_text (clcon_text mark remote-name)
  "See also send-mark-to-clcon_text"
  (cerror "This gf should not be used" "")
  (check-type remote-name (or symbol string))
  (check-type mark mark)
  (let* ((code (format nil "expr [~A index ~A]" clcon_text remote-name))
         (index (call-tcl-simple code)))
    (multiple-value-bind (row col) (parse-tcl-text-index index)
      (odu::set-mark-to-row-and-col mark row col)
      (multiple-value-bind (new-row new-col)
          (mark-row-and-col mark)
        (unless (and (= row new-row) (= col new-col))
          (cerror "continue" "sync-mark-from-clcon_text failed: row:~A?~A,col:~A?~A"
                  row new-row col new-col))
        ))))

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
  (with-standard-io-syntax
    (let ((tcl-character (cl-tk:tcl-escape (make-string 1 :initial-element character))))
      (format nil "~A insert ~A ~A"
              clcon_text remote-mark-name tcl-character
              ))))

(defmethod tcl-code-for-insert-string (clcon_text remote-mark-name string)
  "See generic-function docstring"
  (with-standard-io-syntax
    (format nil "~A insert ~A ~A"
            clcon_text remote-mark-name (cl-tk:tcl-escape string)
            )))

(defmethod tcl-code-for-delete-region (clcon_text remote-beg-mark-name remote-end-mark-name)
  "See generic-function docstring"
  (format nil "~A delete ~A ~A" clcon_text remote-beg-mark-name remote-end-mark-name)
  )

(defmethod tcl-code-for-delete-characters (clcon_text remote-beg-mark-name n)
  "See generic-function docstring"
  (format nil "~A delete ~A {~A + ~D c}"
          clcon_text remote-beg-mark-name remote-beg-mark-name n))


(defun buffer-of-mark (mark)
  "Returns buffer of the mark"
  (line-buffer (mark-line mark)))

(defmethod insert-string-with-clcon_text (mark string start end)
  "When oi::*do-editing-on-tcl-side*, does all processing by itself. If processes request, returns t. Otherwise returns nil"
  (assert *do-editing-on-tcl-side*)
  (assert (= start 0))
  (assert (= end (length string)))
  (let* ((b (buffer-of-mark mark)))
    (when (clcon_text-backend-buffer-p b)
      (let* ((clcon_text (oi::buffer-to-clcon_text b))
             (rmn (remote-mark-name mark "is")))
        (call-combined-tcl-editing
         (tcl-code-for-send-mark-to-clcon_text
          clcon_text mark :remote-name rmn)
         (tcl-code-for-insert-string
          clcon_text rmn string))
         ;(budden-tools:show-expr `(insert-string-before-sync-mark ,(multiple-value-list (mark-row-and-col mark))))
         ;(sync-mark-from-clcon_text clcon_text mark rmn)
         ;(budden-tools:show-expr `(insert-string-after-sync-mark ,(multiple-value-list (mark-row-and-col mark))))
        (call-tcl-editing 
         (tcl-code-for-unset-clcon_text-mark clcon_text rmn))
        )
      t)))

(defmethod insert-character-with-clcon_text (mark character)
  (assert *do-editing-on-tcl-side*)
  (let* ((b (buffer-of-mark mark))
         (clcon_text (oi::buffer-to-clcon_text b))
         (rmn (remote-mark-name mark "ic")))
    (when (clcon_text-backend-buffer-p b)
      (call-combined-tcl-editing
       (tcl-code-for-send-mark-to-clcon_text
        clcon_text mark :remote-name rmn)
       (tcl-code-for-insert-character
        clcon_text rmn character))
                                        ;(sync-mark-from-clcon_text clcon_text mark rmn)
      (call-tcl-editing
       (tcl-code-for-unset-clcon_text-mark clcon_text rmn))
      t
      )))

(defmethod insert-region-with-clcon_text (mark region)
  (when *do-editing-on-tcl-side*
    (let ((rs (region-to-string region)))
      (insert-string-with-clcon_text mark rs 0 (length rs))))
  )

(defmethod ninsert-region-with-clcon_text (mark region)
  (assert *do-editing-on-tcl-side*)
  (let* ((b (buffer-of-mark mark)))
    (when (bufferp b) ; see %buffer slot description in oi::line definition
      ;; this means text is in some buffer and thus we must reflect it on clcon side
      (insert-region-with-clcon_text mark region)
      t)))

(defmethod delete-characters-with-clcon_text (mark n)
  "We have to reproduce some logic from primary (defmethod delete-characters t) method"
  ;; code from primary method
  (assert *do-editing-on-tcl-side*)
  (let* ((line (mark-line mark))
         (charpos (mark-charpos mark))
         (length (line-length* line))
         (b (buffer-of-mark mark)))
    (cond
      ((not (clcon_text-backend-buffer-p b))
       nil)
      ((zerop n) t)
      ;; Deleting chars on one line, just bump the pointers.
      ((<= 0 (+ charpos n) length)
       ;; end of code from primary method
       (let* ((rmn-dcb (remote-mark-name mark "dcb"))
              (clcon_text (buffer-to-clcon_text b)))
         (call-combined-tcl-editing
          (tcl-code-for-send-mark-to-clcon_text 
           clcon_text mark :remote-name rmn-dcb)
          (tcl-code-for-delete-characters clcon_text rmn-dcb n)
          (tcl-code-for-unset-clcon_text-mark clcon_text rmn-dcb)
          ))
       t
       )
      (t
       nil) ; primary method would call delete region and there we will send command to tcl
      )))

(defmethod delete-region-with-clcon_text (region)
  (assert *do-editing-on-tcl-side*)
  (let* ((start (region-start region))
         (end (region-end region))
         (first-line (mark-line start))
         (buffer (line-%buffer first-line)))
    (when (bufferp buffer)
      (let* (
             (clcon_text (buffer-to-clcon_text buffer))
             (rmn-bb (remote-mark-name start "bb"))
             (rmn-ee (remote-mark-name end "ee")))
        (call-combined-tcl-editing
         (tcl-code-for-send-mark-to-clcon_text
          clcon_text start :remote-name rmn-bb)
         (tcl-code-for-send-mark-to-clcon_text
          clcon_text end :remote-name rmn-ee)
         (tcl-code-for-delete-region clcon_text rmn-bb rmn-ee)
         (tcl-code-for-unset-clcon_text-mark clcon_text rmn-bb)
         (tcl-code-for-unset-clcon_text-mark clcon_text rmn-ee)))
      t
      )
    ))

(defmethod delete-and-save-region-with-clcon_text (region)
  (assert *do-editing-on-tcl-side*)
  (delete-region-with-clcon_text region)
  )


;; Region selection
(defun tcl-code-to-select-region (clcon_text rmn-rb rmn-re)
  (format nil "::edt::TextSetSelectionTo ~A ~A ~A"
          clcon_text (tcl-index-of-mark rmn-rb) (tcl-index-of-mark rmn-re)))

(defmethod transfer-selection-to-clcon_text (region)
  (let* ((rb (region-start region))
         (re (region-end region))
         (buffer (buffer-of-mark rb))
         (clcon_text (buffer-to-clcon_text buffer)))
    (when clcon_text
      (call-tcl-simple
       (tcl-code-to-select-region clcon_text rb re)
       ))))


(defmethod transfer-selection-from-clcon_text (buffer)
  (let* ((coords (get-clcon_text-selection-coordinates buffer)))
    (cond
      ((null coords)
       nil)
      (t
       (destructuring-bind (beg-row beg-col end-row end-col) coords
         (multiple-value-bind (row col)
             (oi::mark-row-and-col (current-point))
           (cond
             ((and (= row beg-row)
                   (= col beg-col))
              (odu::with-mark-in-row-col (m (clco::make-row-col :row end-row :col end-col))
                (push-buffer-mark (copy-mark m) t)))
             ((and (= row end-row)
                   (= col end-col))
              (odu::with-mark-in-row-col (m (clco::make-row-col :row beg-row :col beg-col))
                (push-buffer-mark (copy-mark m) t)))
             (t
              (error "transfer-selection-from-clcon-text: current-point must coincide with either beginning of selection or with end of selection")))))))))


(defun get-clcon_text-selection-coordinates (buffer)  
  "Returns list: rowbeg, colbeg, rowend, colend of selection, or nil if there is no selection"
  (let* ((clcon_text (buffer-to-clcon_text buffer))
         (tcl-code (format nil "::edt::TextSelectionCoordinates ~A" clcon_text))
         (index-string (call-tcl-simple tcl-code))
         )
    (assert clcon_text () "~S is not a clcon_text backend buffer" buffer)
    (cond
      ((string= index-string "")
       nil
       )
      (t
       (let* ((indices (split-sequence:split-sequence #\  index-string))
              (ibeg (multiple-value-list (parse-tcl-text-index (first indices))))
              (iend (multiple-value-list (parse-tcl-text-index (second indices)))))
         (append ibeg iend))))))
         

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
