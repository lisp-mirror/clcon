;; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-
;; Our lisp mode and other application-level oduvanchik commands for clcon. 
(in-package :oduvanchik)
(named-readtables::in-readtable :oduvanchik-ext-readtable)

(defcommand "Find Source" (p)
    "Find source with swank machinery. Note if there are several sources they're printed at the console as hyperlinks, no jumping"
    ""
  (let* ((s (symbol-string-at-point))
         (code (clco::server-lookup-definition s)))
    code))


(defcommand "Sync Cursor" (p)
    "Debug-time command to sync cursor. There were no need to make it a command"
    "Does nothing but printing current cursor position. The rely upon the fact that clco::call-oduvanchik-function-with-clcon_text syncs cursor of backend buffer with that of clcon_text"
  (multiple-value-bind (row col)
      (oi::mark-row-and-col (current-point))
    (clco:eval-in-tcl
     (format nil "puts {Oduvan: ~D.~D}" row col)
     :nowait nil
     )
    ))

(defun encode-marks-for-line (line stream &key line-number)
  "{linenumber {charpos0 font0} {charpos1 font1} ...} 
  If we know line-number, we can pass it"
  (let* ((marks (oi::line-marks line))
         (any-mark (first marks))
         (exact-line-number line-number)
         (have-something nil)
         )
    (unless any-mark
      (return-from encode-marks-for-line nil))
    (unless exact-line-number
      (setf exact-line-number
            (+ (nth 1 (oi::mark-position any-mark)) 1))
      (assert (= exact-line-number
                 (oi::tag-line-number (oi::%line-tag line)))))
    (dolist (m (sort marks '< :key 'oi::mark-charpos))
      (typecase m
        (oi:font-mark
         (cond
           (have-something)
           (t
            (setf have-something t)
            (format stream "{~D " exact-line-number)
            ))            
         (format stream "{~D ~D} " (oi:mark-charpos m) (oi::font-mark-font m)))))
    (when have-something 
      (format stream "} "))
    ))

(defun numbered-line-of-buffer-by-clcon (clcon_text number)
  "Number starts from 1. See also oi::mark-row-and-col"
  (let* ((first-line 
          (slot-value
           (slot-value
            (slot-value
             (oi::clcon_text-to-buffer clcon_text)
             'oi::%region)
            'oi::start)
           'oi::line))
         (line first-line)
         (i 1))
    (assert (> number 0))
    (loop
       (when (= number i)
         (return line))
       (setf line (oi::line-next line))
       (incf i))))
       
    

;;(defun dump-all-marks (clcon_text stream-designator)
;;  (do ((s (odu::numbered-line-of-buffer-by-clcon clcon_text 1)
;;          (oi::line-next s)
;;          ))
;;      ((null s) t)
;;    (encode-marks-for-line s stream-designator)
;;    ))


(defun buffer-change-id (buffer)
    "We believe that: 
    oi::now-tick always grows
    oi::buffer-modified-tick contains some value of (oi::tick) 
    "
    (oi::buffer-modified-tick buffer))

(defmethod oi::recompute-syntax-marks :around (line tag)
  (multiple-value-bind (result recomputed)
      (call-next-method)
    (when recomputed
      #+oduvan-enable-highlight
      (let* ((buffer (line-buffer line)))
        (when (bufferp buffer)
          (let* ((clcon_text (oi::buffer-to-clcon_text buffer))
                 (connection (and clcon_text
                                        ; if not, this is not clcon_text backend buffer
                                  (oduvanchik-interface:variable-value
                                   'odu::swank-connection ;"Swank Connection"
                                   :buffer buffer))
                   ))
            (when (and clcon_text connection)
              (let* ((line-number (oi::tag-line-number (oi::%line-tag line)))
                     (encoded-marks
                      (with-output-to-string (ou)
                        (encode-marks-for-line line line-number ou)))
                     (change-id (buffer-change-id buffer)))
                (clco::notify-highlight-single-line 
                 clcon_text encoded-marks line-number change-id buffer)
                )
              )))))
    result))


(defun recompute-line-tags-starting-from-line-background (start-line)
  (iter (for line initially start-line then (line-next line))
        (while line)
        (oi::recompute-line-tag line)
        ;;(until (eq line end-line)) ; это наше отличие
        ))
  
(defmethod oi::recompute-tags-up-to :around (end-line background)
  "We always recompute everything to the end of file. end-line is required to know buffer only"
  (cond
    ((null background)
     (call-next-method))
    (t
     (let* ((level (oi::buffer-tag-line-number (line-buffer end-line)))
            (start-line
             (iter (for line initially end-line then prev)
                   (for prev = (line-previous line))
                   (let ((validp (< (oi::line-number line) level)))
                     (finding line such-that (or validp (null prev)))))))
       (unless (line-previous start-line)
         (let ((tag (oi::make-tag :syntax-info (oi::empty-syntax-info))))
           (setf (oi::%line-tag start-line) tag)
           (setf (oi::tag-syntax-info tag) (oi::recompute-syntax-marks start-line tag)))
         (setf start-line (line-next start-line)))
       ;; Это только для начальной отладки! Потом выделять случай, когда что-то реально поменялось, и переделать в сообщение
       (recompute-line-tags-starting-from-line-background start-line)
       ))))


(defun cl-boolean-to-tcl (x)
  "Returns 0 or 1 in numeric form"
  (if x "1" "0")
  )

(defun clcon-prompt-for-y-or-n (&rest args
                                &key ((:must-exist must-exist) t)
                                  (default nil defaultp)
                                  default-string
                                  ((:prompt prompt) "Y or N? ")
                                  ((:help *parse-help*) "Type Y or N."))
  "For oi::*clcon-prompt-for-y-or-n-hook*. See also oi::prompt-for-y-or-n"
  (declare (ignore args default defaultp default-string))
  (let* ((q-must-exist (cl-boolean-to-tcl must-exist))
         ;; default is ignored now. It is a boolean, if true,
         ;; default value is true, otherwise it is false
         (q-prompt (cl-tk:tcl-escape prompt))
         (q-parse-help (cl-tk:tcl-escape *parse-help*))
         (cmd (format nil "::odu::prompt_for_y_or_n ~A ~A ~A" q-must-exist q-prompt q-parse-help))
         (tcl-result (clco:eval-in-tcl cmd :nowait nil)))
    (cond
      ((string= tcl-result "yes")
       #k"y"
       )
      ((string= tcl-result "no")
       #k"n"
       )
      ((string= tcl-result "cancel")
       (editor-error)
       ))))

(setf oi::*clcon-prompt-for-y-or-n-hook* 'clcon-prompt-for-y-or-n)


; (slot-value (slot-value (slot-value (slot-value (oi::clcon_text-to-buffer ".__edit1.text") 'oi::%region) 'oi::start) 'oi::line) 'oi::marks)

; (clco:eval-in-tcl (with-output-to-string (ou) (format ou "::edt::ApplyHighlightToLine .__edit1.text ") (odu::encode-marks-for-line (odu::numbered-line-of-buffer-by-clcon ".__edit1.text" 3) ou)))


(defun check-display-start-mark-ok (window)
  (let* ((mark (window-display-start window)))
    (oi::check-mark-is-in-its-line mark)))


(defun check-display-start-ok ()
  (dolist (b oi::*buffer-list*)
    (dolist (w (buffer-windows b))
      (check-display-start-mark-ok w))))


(defun count-marks-in-current-buffer ()
  "{linenumber {charpos0 font0} {charpos1 font1} ...} 
  If we know line-number, we can pass it"
  (let* ((b (current-buffer))
         (m (buffer-start-mark b))
         (l (mark-line m))
         (count 0))
    (do ((ll l (line-next ll)))
        ((null ll) count)
      (incf count (length (oi::line-marks ll))))))
