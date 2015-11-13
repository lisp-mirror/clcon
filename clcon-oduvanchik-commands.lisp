;; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-
;; Our lisp mode and other application-level oduvanchik commands for clcon. 
(in-package :oduvanchik)
(named-readtables::in-readtable :oduvanchik-ext-readtable)

(defcommand "Find Source" (p)
    "Find source with swank machinery. Note if there are several sources they're printed at the console as hyperlinks, no jumping"
    ""
  (let* ((s ;(symbol-string-at-point)
          (get-symbol-from-current-point))
         (code (clco::server-lookup-definition s (odu::package-at-point) (odu::readtable-at-point))))
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


(defun oduvan-invisible-kill-open-paren-font-marks ()
  "Rework of kill-open-paren-font-marks"
  (when *open-paren-font-marks*
    (let* ((start (region-start *open-paren-font-marks*))
           (end (region-end *open-paren-font-marks*))
           (lines (mapcar #'mark-line (list start end)))
           (economy-lines (remove nil (remove-duplicates lines))))
      (delete-font-mark start)
      (delete-font-mark end)
      (setf *open-paren-font-marks* nil)
      (when (every 'oi::line-tag-valid-p economy-lines)
        (dolist (line economy-lines)
          (oi::maybe-send-line-highlight-to-clcon line))))))


(defun maybe-send-package-to-tcl (clcon_text)
  "Send package at current-point if it is known. We assume and do not check that clcon_text corresponds to current buffer. Returns t if package was sent. See also maybe-send-readtable-to-tcl"
  (let* ((p (current-point))
         (line (mark-line p))
         (buffer (line-buffer line))
         (package (odu::package-at-point-if-known))
         (last-package (oi::buffer-last-package-name-sent-to-tcl buffer))
         )
    (cond
     ((equalp package last-package)
      ; do nothing
      nil
      )
     (t
      (send-package-to-clcon clcon_text package buffer)
      (setf (oi::buffer-last-package-name-sent-to-tcl buffer) package)
      t
      ))))

(defun maybe-send-readtable-to-tcl (clcon_text)
  "Similar to maybe-send-package-to-tcl"
  (let* ((p (current-point))
         (line (mark-line p))
         (buffer (line-buffer line))
         (rt (odu::readtable-at-point-if-known))
         (last-rt (oi::buffer-last-readtable-name-sent-to-tcl buffer))
         )
    (cond
     ((equalp rt last-rt)
      ; do nothing
      nil
      )
     (t
      (send-readtable-to-clcon clcon_text rt buffer)
      (setf (oi::buffer-last-readtable-name-sent-to-tcl buffer) rt)
      t
      ))))

(defun oduvan-invisible-maybe-highlight-open-parens ()
  "Rework of odu::maybe-highlight-open-parens. Works in the current buffer. 
 It looks like working with parens is independent of buffer highlighting. So we don't
 bother ourselves with redrawing"
  (check-something-ok)
  (when (value highlight-open-parens)
    (multiple-value-bind (start end)
        (funcall (value open-paren-finder-function)
                 (current-point))
      (cond
        ((and start end)
         (oduvan-invisible-kill-open-paren-font-marks)
         (set-open-paren-font-marks start end)
         (let* ((lines (mapcar #'mark-line (list start end)))
                (economy-lines (remove nil (remove-duplicates lines))))
           (when (every 'oi::line-tag-valid-p economy-lines)
             (dolist (line economy-lines)
               (oi::maybe-send-line-highlight-to-clcon line)))))
        (t
         (oduvan-invisible-kill-open-paren-font-marks)
         )))))


(defun line-effective-marks (line)
  "Marks of the line plus marks of odu::*open-paren-font-marks*"
  (oi::check-something-ok)
  (let* ((tag (line-tag-no-recalc line))
         (syntax-info (oi::tag-syntax-info tag))
         (marks ; this worked (oi::sy-font-marks syntax-info)
          (oi::line-marks line) ; this ceased to work before. 
           )
         (sorted-marks (sort (copy-list marks) '< :key 'oi::mark-charpos)))
    (declare (ignorable syntax-info))
    (oi::check-something-ok)
    sorted-marks
    ))

(defun encode-marks-for-line (line stream)
  "{linenumber {charpos0 font0} {charpos1 font1} ...} 
  If we know line-number, we can pass it. line-tag must be ok."
  (oi::check-something-ok)
  (let* ((tag (line-tag-no-recalc line))
       ;(syntax-info (oi::tag-syntax-info tag))
       (exact-line-number (oi::tag-line-number tag))
       (sorted-marks (line-effective-marks line)))
    (format stream "{~D " exact-line-number)
    (dolist (m sorted-marks)
      (typecase m
        (oi:font-mark
         (format stream "{~D ~D} " (oi:mark-charpos m) (oi::font-mark-font m)))))
    (format stream "} ")
    (oi::check-something-ok)
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


(defmethod oi::maybe-send-line-highlight-to-clcon :around (line)
  "If we have highlight enabled in features, send it to clcon"
  #-oduvan-enable-highlight
  (declare (ignore line))
  #+oduvan-enable-highlight
  (cond
    ((null line))
    (t 
     (let* ((buffer (line-buffer line))
            (tag (oi:line-tag-no-recalc line))
            (line-number (oi::tag-line-number tag))
            (sy (oi::tag-syntax-info tag)))
       (assert sy)
       (when (bufferp buffer)
         (let* ((clcon_text (oi::buffer-to-clcon_text buffer))
                (connection (and clcon_text
                                 ;; if not, this is not clcon_text backend buffer
                                 (oduvanchik-interface:variable-value
                                  'odu::swank-connection ;"Swank Connection"
                                  :buffer buffer))
                  ))
           (when (and clcon_text connection)
             (let* ((encoded-marks
                     (with-output-to-string (ou)
                       (encode-marks-for-line line ou)))
                    (change-id (buffer-change-id buffer)))
               (clco::notify-highlight-single-line 
                clcon_text encoded-marks line-number change-id buffer)
               )
             )))))))

(defun send-package-to-clcon (clcon_text package buffer)
  "Posts event of package change to a highlight events queue. See also send-readtable-to-clcon"
  #-oduvan-enable-highlight
  (declare (ignore clcon_text package buffer))
  #+oduvan-enable-highlight
  (clco::notify-package-change clcon_text package buffer)
  )


(defun send-readtable-to-clcon (clcon_text rt buffer)
  "Similar to send-package-to-clcon"
  #-oduvan-enable-highlight
  (declare (ignore clcon_text rt buffer))
  #+oduvan-enable-highlight
  (clco::notify-readtable-change clcon_text rt buffer)
  )


(defmethod oi::recompute-tags-up-to (end-line (background (eql t)))
  "We always recompute everything to the end of file. end-line is required to know buffer only"
  (oi::check-something-ok)
  (let* ((buffer (line-buffer end-line))
         (dummy1 (assert buffer))
         (buffer-end (oi::buffer-end-mark buffer))
         (dummy2 (assert buffer-end))
         (real-end-line (mark-line buffer-end))
         (new-highlight-wave-id (reset-background-highlight-process buffer))
         (level (oi::buffer-tag-line-number buffer))
         (start-line
          (iter (for line initially real-end-line then prev)
                (for prev = (line-previous line))
                (let ((validp (< (oi::line-number line) level)))
                  (finding line such-that (or validp (null prev)))))))
    (declare (ignore dummy1 dummy2))
    (oi::check-something-ok)
    (clco::order-call-oduvanchik-from-itself
     (list 'recompute-line-tags-starting-from-line-background
           buffer start-line new-highlight-wave-id))
    ))

(defun recompute-line-tags-starting-from-line-background (buffer start-line highlight-wave-id)
  (assert-we-are-in-oduvanchik-thread)
  (let ((check-the-buffer (line-buffer start-line)))
    (cond
      ((not (= highlight-wave-id (oi::buffer-highlight-wave-id buffer)))
       ;; We're obsolete. Die.
       (return-from recompute-line-tags-starting-from-line-background nil))
      ((not (equal buffer check-the-buffer))
       (error "Again we are trying to paint deleted line..."))
      (t 
       (oi::check-something-ok)
       (when start-line
         (oi::recompute-line-tag start-line)
         (let ((next (oi::line-next start-line)))
           (when next
             (clco::order-call-oduvanchik-from-itself
              (list 'recompute-line-tags-starting-from-line-background
                    buffer next highlight-wave-id)))))))))

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

(defun check-all-lines-are-disjoint ()
  (let* ((b (current-buffer))
         (all-line-chars nil)
         (m (buffer-start-mark b))
         (l (mark-line m))
         )
    (do ((ll l (line-next ll)))
        ((null ll)
         nil
         )
      (push (oi::line-chars ll) all-line-chars))
    (unless
        (= (length all-line-chars)
           (length (remove-duplicates all-line-chars)))
      (print "Some line-chars coincide"))))






(defun mark-buffer (mark) (odu::line-buffer (odu::mark-line mark)))


(defcommand "Test Get Symbol From Current Point" (p)
    "Find source with swank machinery. Note if there are several sources they're printed at the console as hyperlinks, no jumping"
    ""
  (let* ((s (get-symbol-from-current-point)))
    (print s)
    nil))


(defun get-symbol-from-current-point (&REST keyargs &KEY (PREVIOUS T) (MAX-LENGTH 100) (MAX-NON-ALPHANUMERIC 15) (CREATE-NEW nil))
  "editor::get-symbol-from-point используется в редакторе, когда хотим забрать символ из текущей точки буфера. Что мы делаем в адвайсе?
  Полностью подменяем команду. Читаем символ с помощью ридера, переключённого в спец. режим. 
  Если с пакетом неясность, ищем все символы с таким именем и предлагаем пользователю выбор.   
  Имеем возможность не создавать символ при попытке забрать его из буфера - это аккуратная политика. 
  Выражение a^b рассматриваем как два символа a и b "
  (declare (ignore previous max-non-alphanumeric))
  (perga-implementation:perga function
    (let point (odu::current-point))
    (unless (BUDDEN-TOOLS::packages-seen-p *readtable*)
      (unless (member :create-new (budden-tools:splice-list keyargs) :key 'car)
        (setf keyargs (append keyargs (list :create-new t))))
      (return-from function (odu::symbol-string-at-point)))
    (let p1 (odu::copy-mark point :temporary))
    (let buf-beg (oi::buffer-start-mark (mark-buffer point)))
    (let buf-end (oi::buffer-end-mark (mark-buffer point)))
    (let rest-length (or max-length -1))
    (let symbol-beginning nil)
    (let v-in-symbol nil) ; истина, когда внутри символа
    (let cur-in-symbol nil) ; истина, когда текущий char относится к символу
    ; looking for a symbol at or before point
    (do () ((not (and (> rest-length 0)
                  (odu::mark> p1 buf-beg))) nil) 
      (unless (odu::mark= p1 buf-end)
        (let next-char (odu::next-character p1))
        (unless next-char (error "No character there"))
        (setf cur-in-symbol (editor-budden-tools::char-can-be-in-symbol next-char)))
      (when cur-in-symbol
        (unless v-in-symbol
           ;(setf symbol-end (copy-point p1 :temporary))
          (setf v-in-symbol t)))
      (when v-in-symbol
        (unless cur-in-symbol
          (odu::character-offset p1 1)
          (setf symbol-beginning (odu::copy-mark p1 :temporary))
          (return nil)))
      (odu::character-offset p1 -1)
      (incf rest-length -1)
      )
    ; find the end of the symbol
    (when symbol-beginning
      (let lookup-end (odu::copy-mark symbol-beginning :temporary))
      (let lookup-end-count max-length)
      (do () ((not (and (odu::mark< lookup-end buf-end)
                        (or 
                         (null max-length) 
                         (> lookup-end-count 0)))) nil)
        (odu::character-offset lookup-end 1)
        (incf lookup-end-count -1))
      (let ss (clco::string-between-marks symbol-beginning lookup-end))
      (let package (or
                    (find-package (odu::package-at-point))
                    (progn
                      (warn "unable to learn package at ~S. Assuming cl-user" point)
                      (find-package :cl-user))))
      (cond
       (create-new
        (ignore-errors
          (let ((budden-tools::*inhibit-readmacro* t)
                (*package* package)
                )
            (read-from-string ss))))
       (t 
        (:@ multiple-value-bind (maybe-potential-symbol maybe-error)
                   (ignore-errors
                    (let ((sbcl-reader-budden-tools-lispworks:*return-package-and-symbol-name-from-read* t)
                          (*package* package)
                          (budden-tools::*inhibit-readmacro* t))
                      (read-from-string ss))
                    ))
        (cond
         ((typep maybe-error 'error) ; maybe-symbol can be nil, and read returns position. 
                                     ; so we check if there is a error
          (values nil nil) ; no symbol
          )
         ((sbcl-reader-budden-tools-lispworks:potential-symbol-p maybe-potential-symbol)
          (editor-budden-tools::process-potential-symbol maybe-potential-symbol package)
          )
           
         ;((not (symbolp maybe-symbol))
         ; (editor-error "~S is not a symbol name" maybe-symbol))
         ((and (consp maybe-potential-symbol)
               (eq (car maybe-potential-symbol) 'budden-tools:|^|)
               (SBCL-READER-BUDDEN-TOOLS-LISPWORKS:potential-symbol-p (second maybe-potential-symbol)))
          (editor-budden-tools::process-potential-symbol (second maybe-potential-symbol) package))

         (t ; in some modes we should not err
          (values nil nil)
          )
         )
        )
       ))))
