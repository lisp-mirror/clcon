;; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-
;; Our lisp mode and other application-level oduvanchik commands for clcon. 
(in-package :oduvanchik)
(named-readtables::in-readtable :oduvanchik-ext-readtable)

(defun forward-or-backward-form-or-word (forward-p)
  "Если мы внутри строки или комментария, идти по словам. Иначе, идти по формам"
  (perga-implementation:perga
   (let point (current-point))
   (let word-p (not (valid-spot point forward-p)))
   (cond
    (forward-p
     (cond
      (word-p
       (forward-word-command nil)
       )
      (t
       (forward-form-command nil))))
    (t
     (cond
      (word-p
       (backward-word-command nil))
      (t
       (backward-form-command nil)))))   
   ))

(defun forward-or-backward-char (forward-p)
  (if forward-p
      (forward-character-command nil)
      (backward-character-command nil)))

(defcommand "Forward Form Or Word" (p)
  "Step to the next form or word"
  "Если мы в комментарии или строке, перейти на слово вперёд. Иначе, перейти на форму вперёд"
  (declare (ignore p))
  (forward-or-backward-form-or-word t)
  (deactivate-region)
  (when oi::*do-editing-on-tcl-side*
    (oi::transfer-selection-to-clcon_text t))
  )

(defcommand "Backward Form Or Word" (p)
  "Step to the next form or word"
  "Если мы в комментарии или строке, перейти на слово вперёд. Иначе, перейти на форму вперёд"
  (declare (ignore p))
  (forward-or-backward-form-or-word nil)
  (deactivate-region)
  (when oi::*do-editing-on-tcl-side*
    (oi::transfer-selection-to-clcon_text t))
  )


(defun forward-or-backward-altering-selection (motion-fn forward-p)
  "Двигается по тексту, расширяя или сужая выделение. motion-fn - команда перемещения, к-рая принимает направление - forward-p"
  (perga-implementation:perga
   (let b (current-buffer))
   (let m (buffer-mark b))
   (let point (current-point))
   (cond
    ((region-active-p)
     (let initial-order (mark< point m))
     (funcall motion-fn forward-p)
     (let final-order (mark< point m))
     ; if we step above other end of selection, cancel selection completely
     (unless (eq initial-order final-order)
       (move-mark point m)
       (deactivate-region))
     )
    (t
     (push-buffer-mark (copy-mark point))
     (funcall motion-fn forward-p)
     (activate-region)))
   (when oi::*do-editing-on-tcl-side*
     (oi::transfer-selection-to-clcon_text t))
   ))


(defcommand "Forward Form Or Word Altering Selection" (p)
  "Step to the next form extracting/contracting selection"
  "If there is no selection, selects from point to the end of form. 
   If we are at the end of selection, expand it. If we are at the beginning of selection, contract it.
   Если мы в комментарии или строке, двигаться по словам, а не по формам"
  (declare (ignore p))
  (forward-or-backward-altering-selection 'forward-or-backward-form-or-word t)
  )

(defcommand "Backward Form Or Word Altering Selection" (p)
  "Step to the previous form extracting/contracting selection"
  "If there is no selection, selects from point to the beginning of form. 
   If we are at the end of selection, expand it. If we are at the beginning of selection, contract it.
   Если мы в комментарии или строке, двигаться по словам, а не по формам"
  (declare (ignore p))
  (forward-or-backward-altering-selection 'forward-or-backward-form-or-word nil)
  )

(defcommand "Forward Character Altering Selection" (p)
  "Перейти на букву вперёд, меняя выделение"
  "Перейти на букву вперёд, меняя выделение"
  (declare (ignore p))
  (forward-or-backward-altering-selection 'forward-or-backward-char t)
  )

(defcommand "Backward Character Altering Selection" (p)
  "Перейти на букву вперёд, меняя выделение"
  "Перейти на букву вперёд, меняя выделение"
  (declare (ignore p))
  (forward-or-backward-altering-selection 'forward-or-backward-char nil)
  )

(defcommand "Find Source" (p)
    "Find source with swank machinery. Note if there are several sources they're printed at the console as hyperlinks, no jumping"
    ""
  (multiple-value-bind (string symbol) (get-symbol-from-current-point)
    (let ((code
           (clco:server-lookup-definition (or symbol string)
                                           (odu::package-at-point)
                                           (odu::readtable-at-point))))
      code)))

(defcommand "Find Package" (p)
    "Find package source with swank machinery. Note if there are several sources they're printed at the console as hyperlinks, no jumping"
    ""
  (clco:server-lookup-definition (odu::package-at-point)
                                 (odu::package-at-point)
                                 (odu::readtable-at-point)))

(defun get-system-from-file-options (buffer)
  "Get system name from first line" ; ПРАВМЯ - не совсем ясно, зачем здесь продублирована реализация odu::process-file-options, 
    ; по идее это buffer-variable или что-то вроде этого
  (let* ((string
          (line-string (mark-line (buffer-start-mark buffer))))
         (found (search "-*-" string)))
    (declare (simple-string string))
    (when found
      (block do-file-options
        (let* ((start (+ found 3))
               (end (search "-*-" string :start2 start)))
          (unless end
            (loud-message "No closing \"-*-\".  Aborting file options.")
            (return-from do-file-options))
          (when (find #\: string :start start :end end)
            (do ((opt-start start (1+ semi)) colon semi)
                (nil)
              (setq colon (position #\: string :start opt-start :end end))
              (unless colon
                (loud-message "Missing \":\".  Aborting file options.")
                (return-from do-file-options))
              (setq semi (or (position #\; string :start colon :end end) end))
              (let* ((option (nstring-downcase
                              (trim-subseq string opt-start colon))))
                (declare (simple-string option))
                (when (string= option "system")
                  (return-from do-file-options (identity ; не это string-downcase ; он нам здесь нужен, чтобы прийти в соответствие с действиями asdf:coerce-name 
                                                (trim-subseq string (1+ colon) semi))))
                (when (= semi end) (return nil))))))))))

(defun |Найти-имя-системы-из-имени-файла-или-переменных-буфера-либо-сообщение| (buffer)
  "Находим имя системы из имени файла или из переменных в первой строчке. Если не нашли, выдаём осмысленное сообщение"
  (let* ((pathname (oduvanchik-interface:buffer-pathname buffer))
         (system-name
          (cond
           ((string-equal (pathname-type pathname) "asd")
            (pathname-name pathname))
           (t
            (get-system-from-file-options buffer)))))
    ;; Регистры имён файлов в windows игнорируются, а 
    ;; символы в лиспе обычно в верхнем регистре. Поэтому вот такое преобразование,
    ;; вопреки тому, что имена систем нормализуются к нижнему регистру
    (cond
     ((or (not system-name)
          (string= system-name ""))
      (format *error-output* "~&Имя системы в редакторе определено, если текущий файл редактора имеет расширение .asd или когда система задана в первой строчке текущего файла, например, так: 
;; -*- Encoding: utf-8; System :decorate-function -*-
Сейчас имя не определено~%")
      nil)
     (t
      (intern (russian-budden-tools:string-upcase-cyr system-name) :keyword)))))

(defun server-lookup-system-definition (name-or-symbol package-name readtable-name)
  "name-or-symbol is a name of a lisp object or object itself which can have definition. Returns a string which must be evaluated in tcl to print hypertext menu of links OR to jump to a location directly"
  (let* ((dspecs-and-locations
          (remove-if-not (lambda (x) (eq (caar x) 'defconstant)) 
            (clco::swank-find-definitions-for-clcon name-or-symbol :package-name package-name :readtable-name readtable-name)))
         (l (length dspecs-and-locations)))
    (with-output-to-string (ou)
      (case l
        (0 (clco::print-just-line ou (format nil "No definitions found for ~A" name-or-symbol)))
        (t
         (when (> l 1)
           (clco::write-code-to-show-console ou))
         (dolist (dal dspecs-and-locations)
           (destructuring-bind (dspec loc) dal
             (cond
               ((= l 1)
                (clco::write-code-to-pass-to-loc ou loc :fix-offset-p t))
               (t
                (let ((link-text (prin1-to-string dspec)))
                  (clco::write-one-dspec-and-location link-text loc ou :fix-offset-p t))))))
         (when (> l 1)
           (clco::write-code-to-see-console-end ou))
         )))))

(defcommand "Find System" (p)
    "Find system (.asd) source with swank machinery. Note if there are several sources they're printed at the console as hyperlinks, no jumping"
    ""
  (let* ((system-name-as-symbol (|Найти-имя-системы-из-имени-файла-или-переменных-буфера-либо-сообщение| (current-buffer))))
    (cond
     (system-name-as-symbol
      (server-lookup-system-definition system-name-as-symbol
                                       (odu::package-at-point)
                                       (odu::readtable-at-point)))
     (t
      "::ProcedureNop"))))

(defcommand "Udalitq fajjly rezulqtata sborki sistemy" (p)
  "Удалить файлы результата сборки системы" ""
  (let* ((system-name (|Найти-имя-системы-из-имени-файла-или-переменных-буфера-либо-сообщение| (current-buffer))))
    (when system-name
      (format t "~&~A~%" (swank:delete-system-fasls system-name)))
    ))

(defcommand "Compile System" (p)
    "Find system (.asd) source with swank machinery and compile it. Note if there are several sources they're printed at the console as hyperlinks, no jumping"
    ""
  (let* ((system-name (|Найти-имя-системы-из-имени-файла-или-переменных-буфера-либо-сообщение| (current-buffer))))
    (if system-name 
        (clco:load-system-for-tcl system-name)      
        (format t "Система ~a не найдена~%" system-name))))

(defcommand "Find Symbol" (p)
    "Find symbol with swank machinery."
    ""
  (multiple-value-bind (string symbol) (get-symbol-from-current-point)
    (multiple-value-bind (symbol2 found)
        (clco::parse-name-or-symbol-to-symbol (or symbol string)
                                              :package-name (odu::package-at-point) 
                                              :readtable-name (odu::readtable-at-point))
      (cond 
       ((not found) "")
       ((find #\: (format nil "~s" symbol2)) (format nil "~s" symbol2))
       (t (format nil "~a:~s" (package-name (symbol-package symbol2)) symbol2))))))

(defcommand "Edit File Name Under Cursor" (p)
  ""
  ""
  (let ((result (РАСПОЗНАТЬ-ИМЯ-ФАЙЛА-В-ТЕКУЩЕЙ-ТОЧКЕ-РЕДАКТОРА-ИЛИ-ПЕРЕД-НЕЙ)))
    result))

(defcommand "Hyperdoc Lookup" (p)
    "Hyperdoc lookup"
    ""
  (multiple-value-bind (string symbol) (get-symbol-from-current-point)
    (let ((code
           (clco:server-hyperdoc-lookup (or symbol string)
                                        (odu::package-at-point)
                                        (odu::readtable-at-point))))
      code)))

(defcommand "Describe All" (p)
    "Describe All"
    ""
  (multiple-value-bind (string symbol) (get-symbol-from-current-point)
    (multiple-value-bind (symbol2 found)
        (clco::parse-name-or-symbol-to-symbol (or symbol string)
                                              :package-name (odu::package-at-point) 
                                              :readtable-name (odu::readtable-at-point))
      (if found (cons string (describe-all symbol2)) (list string)))))

(defcommand "Open Url" (p)
    "Open Url"
    ""
    (clco::open-url p))

(defvar *table-cltl2* nil)

(defun tcl-links (l)
  (let ((n 0) res)
    (dolist (x l)
      (incf n)
      (push (format nil "::tkcon::WriteActiveText $b \"~a \" end \"::edt::OpenUrl http://filonenko-mikhail.github.io/cltl2-doc/ru/~a\";" n x) res))
    (apply #'concatenate 'string (reverse res))))

(defun describe-all (symbol)
  (unless *table-cltl2*
    (setf 
     *table-cltl2* 
     (parser-cltl2-ru::parse 
      (cl-html-parse:parse-html 
       (drakma:http-request 
        "http://filonenko-mikhail.github.io/cltl2-doc/ru/symbols.html")))))
  (list (format nil "~a" (ignore-errors (swank::arglist symbol)))
        (when (eq (symbol-package symbol) (find-package :cl))
          (let ((entry (find-if 
                        (lambda (x) 
                          (string= 
                           (parser-cltl2-ru::entry-expression x) 
                           (string-downcase (symbol-name symbol)))) 
                        *table-cltl2*)))
            (when entry
              (mapcar (lambda (x) (concatenate 'string 
                                               (format nil "$b RoInsert end \"~a \";" (parser-cltl2-ru::entry-line-name x))
                                               (tcl-links 
                                                (parser-cltl2-ru::entry-line-links x))))
                      (parser-cltl2-ru::entry-types entry)))))
        (with-output-to-string (s) 
          (let ((*standard-output* s)) 
            (describe symbol)))
        (clco::server-lookup-definition-as-list symbol)))
              
                
        

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

(defcommand "Complete Symbol With Budden Tools"
     (p) "Complete Symbol With Local Package Nicknames and advanced readtable-case"
         "Complete Symbol With Local Package Nicknames and advanced readtable-case"
  (declare (ignorable p))
  ;; получаем исходный текст, который нужно завершить
  (let* ((str (get-symbol-from-current-point :previous 2 :return-symbol-too nil))
         (str-len (length str)))
    (budden-tools:show-exprt `(Строка-после-разбора ,str))
    (cond ; ВЕВЕРСИИ:|*версия*|
     ((= str-len 0)
      (beep)
      ;(indent-command nil)
      ;(beginning-of-line-command nil)
      )
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
             (simple-listbox-menu completion-list :title "Comletions:")))
        (unless (string= choice "")
          (replace-str-with choice)))
      )))))

