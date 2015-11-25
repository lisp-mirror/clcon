;; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-
;; Our lisp mode and other application-level oduvanchik commands for clcon. 
(in-package :oduvanchik)
(named-readtables::in-readtable :oduvanchik-ext-readtable)

(defcommand "Find Source" (p)
    "Find source with swank machinery. Note if there are several sources they're printed at the console as hyperlinks, no jumping"
    ""
  (multiple-value-bind (string symbol) (get-symbol-from-current-point)
    (let ((code
           (clco::server-lookup-definition (or symbol string)
                                           (odu::package-at-point)
                                           (odu::readtable-at-point))))
      code)))


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
  "Send package at current-point if it is known. We assume and check that clcon_text corresponds to current buffer. Returns t if package was sent. See also maybe-send-readtable-to-tcl"
  (let* ((p (current-point))
         (line (mark-line p))
         (buffer (line-buffer line))
         (package (odu::package-at-point-if-known))
         (last-package (oi::buffer-last-package-name-sent-to-tcl buffer))
         )
    (assert (eq buffer (oi::clcon_text-to-buffer clcon_text)))
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
    (assert (eq buffer (oi::clcon_text-to-buffer clcon_text)))
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

(defun maybe-send-mode-to-tcl (clcon_text)
  "Send mode at current-point if it is known. We assume and do not check that clcon_text corresponds to current buffer. Returns t if package was sent. See also maybe-send-readtable-to-tcl"
  (let* ((p (current-point))
         (line (mark-line p))
         (buffer (line-buffer line))
         (mode (first (oi::buffer-modes buffer)))
         (last-mode (oi::buffer-last-mode-name-sent-to-tcl buffer))
         )
    (cond
     ((equalp mode last-mode)
      ; do nothing
      nil
      )
     (t
      (send-mode-to-clcon clcon_text mode buffer)
      (setf (oi::buffer-last-mode-name-sent-to-tcl buffer) mode)
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


(defun send-mode-to-clcon (clcon_text mode buffer)
  "Clone of send-package-to-clcon"
  #-oduvan-enable-highlight
  (declare (ignore clcon_text mode buffer))
  #+oduvan-enable-highlight
  (clco::notify-mode-change clcon_text mode buffer)
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


(defun where-is-mark-relative-to-symbol (p1)
  "0 - mark is just after the end of symbol string
   1 - mark is inside or just after the end of symbol string
   2 - mark is at the beginning of symbol string
   3 - mark is not in symbol string. Syntax info is ignored, we look
   just at the char itself
   "
  (perga-implementation:perga
   (let prev-char (odu::previous-character p1))
   (let prev-in-symbol
         (and prev-char
              (editor-budden-tools::char-can-be-in-symbol prev-char)))
   (let next-char (odu::next-character p1))
   (let next-in-symbol
         (and next-char
              (editor-budden-tools::char-can-be-in-symbol next-char)))
   (cond
    ((and (not prev-in-symbol) (not next-in-symbol))
     3)
    ((and (not prev-in-symbol) next-in-symbol)
     2)
    ((and prev-in-symbol (not next-in-symbol))
     0)
    (t
     1))))

(defun get-symbol-from-current-point (&REST keyargs &KEY (PREVIOUS 1) (MAX-LENGTH 100) (MAX-NON-ALPHANUMERIC 15) (return-symbol-too t) (CREATE-NEW nil))
  "See also odu::symbol-string-at-point . Если мы в нашей таблице чтения, читаем символ с помощью ридера, переключённого в спец. режим. Если с пакетом неясность, ищем все символы с таким именем и предлагаем пользователю выбор. Имеем возможность не создавать символ при попытке забрать его из буфера - это аккуратная политика. 
  Выражение a^b рассматриваем как два символа a и b. 

  create-new - разрешить создавать символ. Если мы не в нашей таблице чтения (null (packages-seen-p...)) , то create-new игнорируется и символ может быть создан

  Возвращает два значения:
  1. Строка символа, к-рая под курсором, даже если мы не знаем такого символа
  2. Если нас просили вернуть символ и под курсором - известный символ, либо нам разрешили его создать, то возвращает найденный или созданный символ. Если мы не знаем такого символа или не в нашей таблице чтения, возвращаем nil
  Если строки символа нет, вернёт (values \"\" nil)

  previous -
    0 мы должны стоять уже внутри символа (возможно, в конце)
    1 мы берём подходящий символ выше по тексту или тот, в котором стоим. 
    2 мы должны стоять в конце символа, иначе пищим.
  "
  (declare (ignore max-non-alphanumeric keyargs))
  (perga-implementation:perga function
    (check-type previous (integer 0 2))
    (let point (odu::current-point))
    (let readtable (or (named-readtables:find-readtable (odu::readtable-at-point))
                       (named-readtables:find-readtable nil)))
    (let package-designator (odu::package-at-point))
    (let our-readtable (budden-tools::packages-seen-p readtable))
    (let p1 (odu::copy-mark point :temporary))
    (let buf-beg (oi::buffer-start-mark (mark-buffer point)))
    (let buf-end (oi::buffer-end-mark (mark-buffer point)))
    (let rest-length (or max-length -1))
    (let symbol-beginning nil)
    (let v-in-symbol nil) ; истина, когда внутри символа
    (let cur-in-symbol nil) ; истина, когда текущий char относится к символу
    ; Движемся назад от текущей точки и ищем начало символа, записывая в p1
    (let checked-where-we-must-be-initially nil)
    (when (odu::mark= p1 buf-beg)
      (oduvanchik-interface:loud-message "Can not complete or indent at the beginning of buffer")
      (return-from function (values "" nil)))
    (do ()
        ((not (and (> rest-length 0)
                   (odu::mark> p1 buf-beg))) nil)
      (let prev-char (odu::previous-character p1))
      (setf cur-in-symbol (editor-budden-tools::char-can-be-in-symbol prev-char))
      (unless checked-where-we-must-be-initially
        (let where-we-are (where-is-mark-relative-to-symbol p1))
        (cond
         ((and (= previous 0) ; Должны стоять в символе изначально
               (not (member where-we-are '(0 1 2))) ; и не стоим в символе
               )
          (return-from function (values "" nil)))
         ((and (= previous 2)
               (/= where-we-are 0))
          (bell-with-tcl) 
          (return-from function (values "" nil))))  
        (setf checked-where-we-must-be-initially t))
      (when cur-in-symbol
        (unless v-in-symbol
          (setf v-in-symbol t)))
      (when v-in-symbol
        (unless cur-in-symbol
          (setf symbol-beginning (odu::copy-mark p1 :temporary))
          (return nil)))
      (odu::character-offset p1 -1)
      (incf rest-length -1)
      )
    (cond
     ((not symbol-beginning)
      ; ничего нет
      (return-from function (values "" nil)))
     (t
      (get-symbol-from-current-point-part-2
       symbol-beginning max-length buf-end our-readtable return-symbol-too create-new package-designator readtable point)
      ))))

(defun get-symbol-from-current-point-part-2 (symbol-beginning max-length buf-end our-readtable return-symbol-too create-new package-designator readtable point)
  (perga-implementation:perga
   (let lookup-end (odu::copy-mark symbol-beginning :temporary))
   (let lookup-end-count max-length)
   ; Теперь нужно найти конец символа. Для нашей - просто вырезать кусок максимально возможной длины, т.к. мы будем читать символ ридером
   (do () ((not (and (odu::mark< lookup-end buf-end)
                     (or 
                      (null max-length) 
                      (> lookup-end-count 0)))) nil)
     (unless (editor-budden-tools::char-can-be-in-symbol (odu::next-character lookup-end))
       (setf lookup-end (oi::copy-mark lookup-end :temporary))
       (return))
     (odu::character-offset lookup-end 1)
     (incf lookup-end-count -1))
   (let ss (clco::string-between-marks symbol-beginning lookup-end))
   (let package (or
                 (find-package package-designator)
                 (progn
                   (warn "unable to learn package at ~S. Assuming cl-user" point)
                   (find-package :cl-user))))
   (get-symbol-from-current-point-part-3 ss package readtable our-readtable return-symbol-too create-new)))

(defun get-symbol-from-current-point-part-3 (ss package readtable our-readtable return-symbol-too create-new)
  (perga-implementation:perga
   (cond
    ((not (and our-readtable return-symbol-too))
     (values ss nil)
     )
    (create-new
     (ignore-errors
      (let ((budden-tools::*inhibit-readmacro* t)
            (*readtable* readtable)
            (*read-eval* nil)
            (*package* package)
            )
        (values ss (read-from-string ss)))))
    (t 
     (:@ multiple-value-bind (maybe-potential-symbol maybe-error)
         (ignore-errors
          (perga-implementation:perga 
           (let sbcl-reader-budden-tools-lispworks:*return-package-and-symbol-name-from-read* t)
           (let *package* package)
           (let budden-tools::*inhibit-readmacro* t)
           (read-from-string ss))
          ))
     (cond
      ((typep maybe-error 'error) ; maybe-symbol can be nil, and read returns position. 
       ; so we check if there is a error
       (values "" nil) ; no symbol
       )
      ((symbolp maybe-potential-symbol)
       (values ss maybe-potential-symbol))
      ((sbcl-reader-budden-tools-lispworks:potential-symbol-p maybe-potential-symbol)
       (assert (null editor-budden-tools::*in-find-source*))
       ; if this assertion is not true, we could return not the symbol
       ; we had under cursor, but something irrelevant

       (:@ multiple-value-bind (symbol found)
           (editor-budden-tools::process-potential-symbol
            maybe-potential-symbol package)
           )
       (cond
        (found
         (values ss symbol))
        (t
         (values "" nil))))
      
      ;((not (symbolp maybe-symbol))
      ; (editor-error "~S is not a symbol name" maybe-symbol))
      ((and (consp maybe-potential-symbol)
            (eq (car maybe-potential-symbol) 'budden-tools:|^|)
            (SBCL-READER-BUDDEN-TOOLS-LISPWORKS:potential-symbol-p (second maybe-potential-symbol)))
       (:@ multiple-value-bind (symbol found)
           (editor-budden-tools::process-potential-symbol (second maybe-potential-symbol) package))
       (assert found)
       (values ss symbol))
      (t ; in some modes we should not err
       (values "" nil)
       )
      )
     )
    ))) 


(defun completions-menu-run (list &key (owner "") (title "odu::call-scrollable-menu"))
  "Stub - always select first completion"
  (let*
      ((qlist (mapcar 'cl-tk:tcl-escape list))
       (qtitle (cl-tk:tcl-escape title))
       (cmd (format nil "::completions_menu::run [list~{ ~A~}] -owner [list ~A] -title ~A" qlist owner qtitle)))
  (clco:eval-in-tcl cmd :nowait nil)
  ))

(defcommand "Indent or Complete Symbol With Budden Tools"
     (p) "Complete Symbol With Local Package Nicknames and advanced readtable-case"
         "Complete Symbol With Local Package Nicknames and advanced readtable-case"
  (declare (ignorable p))
  ;; получаем исходный текст, который нужно завершить
  (let* ((str (get-symbol-from-current-point :previous 2))
         (str-len (length str)))
    (cond
     ((= str-len 0)
      (indent-command nil)
      (beginning-of-line-command nil))
     (t
      (complete-symbol-with-budden-tools-inner str str-len)
      ))))

(defun complete-symbol-with-budden-tools-inner (str str-len)
  (let* ((package-name (or (package-at-point) :cl-user))
         ;(package (or (find-package package-name) :cl-user))
         (rt-name (readtable-at-point))
         (rt (named-readtables:find-readtable rt-name))
         #|(res (budden-tools::do-complete-symbol-with-budden-tools
                 str
                 package
                 #'error
                 (lambda (show-list) (call-scrollable-menu show-list nil))))|#
         (completions
          (let ((*readtable* rt))
            (swank:completions str package-name)))
         (completion-list (first completions))
         ; (longest-completion (second completions))
         )
    (flet ((replace-str-with (res)
      (delete-previous-character-command str-len)
      (insert-string (current-point) res)))
    (cond
     ((null completion-list)
      (bell-with-tcl))
     ((null (second completion-list))
      (replace-str-with (first completion-list)))
     (t
      (let ((choice
             (completions-menu-run completion-list :title "Comletions:")))
        (unless (string= choice "")
          (replace-str-with choice)))
      )))))

