; -*- coding: utf-8 ; Encoding: utf-8 ; system :clcon-server ; -*- 
(in-package :clco)


(defun find-readtable-or-use-default (readtable-name)
  (multiple-value-bind (result error)
                       (ignore-errors (named-readtables:find-readtable readtable-name))
    (typecase error
     (error
      (warn "Error when finding readtable: ~S" error)
      (named-readtables:find-readtable nil))
     (t
      result))))


(defun parse-name-or-symbol-to-symbol (name-or-symbol &key package-name (readtable-name nil))
  "Given a data about symbol, tries its best to return the symbol Returns two values: symbol and found state"
  (etypecase name-or-symbol
    (symbol
     (values name-or-symbol t))
    (string
     (let* ((package-or-nil (find-package package-name))
            (swank::*buffer-package* 
             (cond
              (package-or-nil
               package-or-nil
               )
              (t
               (error "Package ~S not found" package-name)
               )))
            (swank::*buffer-readtable* (find-readtable-or-use-default readtable-name)))
       (swank::find-definitions-find-symbol-or-package name-or-symbol)))))
  

(defun swank-find-definitions-for-clcon (name-or-symbol |Функция-возвращающая-список-мест| &rest keyargs &key package-name (readtable-name nil))
  "Return a list ((DSPEC LOCATION) ...) of definitions for NAME-OR-SYMBOL. If name-or-symbol is string, we also need package-name and readtable-name. Package-name is a string, readtable-name is a keyword or nil. DSPEC is a string and LOCATION a source location. See also swank:find-definitions-for-emacs"
  (declare (ignorable package-name readtable-name))
  (multiple-value-bind (symbol found)
                       (apply #'parse-name-or-symbol-to-symbol name-or-symbol keyargs)
    (when found
      (funcall |Функция-возвращающая-список-мест| symbol))))

(defun yar-name (pathname)
  (let ((name (namestring pathname)))
    (parse-namestring (subseq name  0 (- (length name) 5)))))
;  (translate-pathname pathname "*.lisp" "*"))

(defun fix-offset-2 (pathname offset) ; корректирует и скачет к лиспу. 
  "Имеется числовой offset, к-рый вернул file-position. Давайте попробуем превратить его в 
  row-col-offset. См. также BUDDEN-TOOLS::input-stream-position-in-chars"
  (КАРТЫ-ИСХОДНИКОВ-ЛИЦО:fix-offset-2 pathname offset))

(defun |Скакнуть-от-Лиспа-к-Яру| (file offset-in-chars)
  "Если месту сопоставлен исходник Яра, вернуть файл и значение как множ.знач. Если не сопоставлен, выругаться и вернуть свои аргументы. На данный момент просто ищет в картах модуля :КАРТЫ-ИСХОДНИКОВ-ЛИЦО"
  (perga-implementation:perga
   (let targets (budden-tools::l/find-sources-in-file file offset-in-chars :strict t))
   (cond
    (targets
     (let target (car targets))
     (let target-file (КАРТЫ-ИСХОДНИКОВ-ЛИЦО:SLO-SOURCE-В-ИМЯ-ФАЙЛА (first target))) ; FIXME Записывать позиции единообразно, например, с помощью olm. 
     (let offset (second target))
     (values target-file offset))
    (t
     (let *print-readably* nil)
     (warn "Скакнуть-от-Лиспа-к-Яру: этого места нет в картах исходников ~S ~S"
           file offset-in-chars)
     (values file offset-in-chars)))))
  
(defun edit-file-at-offset-code (file offset fix-offset-p |Скакнуть-от-Лиспа-к-Яру|)
  (when fix-offset-p
    (multiple-value-bind (newfile offset-15) (fix-offset-2 file offset)
      (setf file newfile offset offset-15)))
  (when |Скакнуть-от-Лиспа-к-Яру|
    (multiple-value-setq (file offset) (|Скакнуть-от-Лиспа-к-Яру| file offset)))
  (incf offset +Надо-добавить-для-перехода-от-колонки-к-колонке-Tk+)
  (let* ((escaped-file (tcl-escape-filename file))
         (offset-2 (format nil "{1.0+ ~A chars}"
                           offset
                           )))
    (format nil "::tkcon::EditFileAtOffset ~A ~A" escaped-file offset-2)))

(defun print-one-hyperlink-tcl-source (stream text file offset &key (index "output") fix-offset-p (|Скакнуть-от-Лиспа-к-Яру| t))
  "Generates tcl code which prints out one hyperlink"
  (let* ((escaped-text (cl-tk:tcl-escape text))
         (edit-file-code (edit-file-at-offset-code file offset fix-offset-p |Скакнуть-от-Лиспа-к-Яру|)))
    (format stream "::tkcon::WriteActiveText $w ~A ~A {~A}; $w insert ~A \\\n; "
            escaped-text
            index
            edit-file-code
            index)))

(defun print-just-line (stream text &key (index "output"))
  (format stream "::tkcon::WritePassiveText $w ~A ~A; $w insert ~A \\\n; " (cl-tk:tcl-escape text) index index))


(defun parse-location-into-file-and-pos (location)
  "returns either values of file and position or nil"
  (cond
    ((and (eq (car location) :location)
          (eq (car (second location)) :file)
          (eq (car (third location)) :position))
     (let ((file (second (second location)))
           (position (second (third location))))
       (values file position)))
    (t nil)))  

(defun write-one-dspec-and-location (link-text location stream &key (index "output") fix-offset-p (|Скакнуть-от-Лиспа-к-Яру| t))
  "It is also used by compilation-error browse, some arbitrary string is passed instead of dspec. Beware! 
  fix-offset-p - magic boolean value, set experimentally
  "
  (let ((printed-dspec link-text))
    (multiple-value-bind (file position)
        (parse-location-into-file-and-pos location)
      (cond
        ((and file position)
         (print-one-hyperlink-tcl-source stream printed-dspec file position :index index :fix-offset-p fix-offset-p :|Скакнуть-от-Лиспа-к-Яру| |Скакнуть-от-Лиспа-к-Яру|))
        (t ; something wrong with location
         (print-just-line stream printed-dspec :index index))))))

(defun write-code-to-pass-to-loc (stream loc &key (mode :text) fix-offset-p (|Скакнуть-от-Лиспа-к-Яру| t))
  "Writes code which would help to pass to location. 
   If mode = :text we will insert the code into text widget. 
   If mode = :eval we will eval the code in the context where $w contains some widget. 
   This widget is required as a parent of tk_messageBox which we show when we unable to locate
   and which is activated after showing message
   "
  (multiple-value-bind (file offset)
                       (parse-location-into-file-and-pos loc)
    (cond
     ((and file offset)
      (format stream "~A; " 
              (edit-file-at-offset-code file offset fix-offset-p |Скакнуть-от-Лиспа-к-Яру|))
      )
     (t
      (let* ((qLocation (cl-tk:tcl-escape (prin1-to-string loc)))
             (message (format nil "Не умею перейти к определению ~A" qLocation)))
        (ecase mode
          (:text 
           (print-just-line stream message))
          (:eval
           (format
            stream
            "tk_messageBox -parent $w -message {~A} ~% focus $w" message)
           )))
      ))))

(defconstant +Надо-добавить-для-перехода-от-колонки-к-колонке-Tk+ -1)

(defmethod editor-budden-tools:goto-xy (pathname row col)
  (check-type row integer)
  (check-type col integer)
  ;; переводим в систему координат, понятную tk
  (incf col +Надо-добавить-для-перехода-от-колонки-к-колонке-Tk+)
  (let* ((escaped-file (tcl-escape-filename pathname))
         (command (format nil "tkcon::EditFileAtOffset ~A ~A.~A" escaped-file row col)))
    (eval-in-tcl command :nowait nil)))


(defun edit-string-at-row-and-col (string &key (row 1) (col 0) (prefix "edit-string-at-offset.") (type "txt"))
  "Сохраняет строку в файл и редактирует его. Файл удаляется после открытия - не факт, что это не вызовет проблем" 
  (perga-implementation:perga
   (check-type row integer)
   (check-type col integer)
   (let filename (uiop/stream::get-temporary-file :prefix (budden-tools:|Закодировать-строку-в-имя-файла| prefix) :type type)) ; не экспортировано... ждём проблем...
   ;; Здесь есть состояние гонки с другим экземпляром clco? ПРАВЬМЯ
   (budden-tools:save-string-to-file string filename)
   (editor-budden-tools:goto-xy filename row col)
   (delete-file filename)
   filename))

(defun write-code-to-pass-to-file-line-char (stream file line char)
  "Writes code which would pass to file at line and char. Lines start from 1, chars start from 0. See also write-code-to-pass-to-loc 
"
  (check-type line alexandria:positive-integer)
  (check-type char alexandria:NON-NEGATIVE-INTEGER)
  (let ((escaped-file (tcl-escape-filename file))
        (offset (format nil "~A.~A" line char)))
    (format stream "tkcon::EditFileAtOffset ~A ~A" escaped-file offset)))


(defun write-code-to-show-console (stream)
  (format stream "::tkcon::FocusConsole; "))
  
(defun write-code-to-see-console-end (stream)
  (format stream "::$::tkcon::PRIV(console) see end; "))

(defun server-lookup-definition (name-or-symbol &optional (package-name (package-name *package*)) (readtable-name (named-readtables:readtable-name *readtable*)))
  (|Обслужить-команду-поиска-связей-символа| name-or-symbol #'swank/backend:find-definitions "Определения символа ~S не найдены" package-name readtable-name))

(defun |Обслужить-команду-поиска-связей-символа| (name-or-symbol |Функция-возвращающая-места| |Формат-сообщения-об-отсутствии-находок| &optional (package-name (package-name *package*)) (readtable-name (named-readtables:readtable-name *readtable*)))
  "По аналогии с server-lookup-definition"
  (perga-implementation:perga
   (let dspecs-and-locations
     (swank-find-definitions-for-clcon
      name-or-symbol |Функция-возвращающая-места|
      :package-name package-name :readtable-name readtable-name))
   (let l (length dspecs-and-locations))
   (:@ with-output-to-string (ou))
   (case l
     (0 (print-just-line ou (format nil |Формат-сообщения-об-отсутствии-находок| name-or-symbol)))
     (t
      (when (> l 1)
        (write-code-to-show-console ou))
      (dolist (dal dspecs-and-locations)
        (:@ destructuring-bind (dspec loc) dal)
        (cond
         ((= l 1)
          (write-code-to-pass-to-loc ou loc :|Скакнуть-от-Лиспа-к-Яру| t))
         (t
          (let link-text (prin1-to-string dspec))
          (write-one-dspec-and-location link-text loc ou :|Скакнуть-от-Лиспа-к-Яру| t))))))
   (when (> l 1)
     (write-code-to-see-console-end ou))))

(defun print-one-hyperlink-tcl-source-for-describe (stream text file offset &key (index "output") fix-offset-p (|Скакнуть-от-Лиспа-к-Яру| t))
  "Generates tcl code which prints out one hyperlink"
  (let* ((escaped-text (cl-tk:tcl-escape text))
         (edit-file-code (edit-file-at-offset-code file offset fix-offset-p |Скакнуть-от-Лиспа-к-Яру|)))
    (format stream "::tkcon::WriteActiveText $b ~A ~A {~A}; $b RoInsert ~A \\\n; "
            escaped-text
            index
            edit-file-code
            index)))

(defun print-just-line-for-describe (stream text)
  (format stream "$b RoInsert end ~A; $b insert end \\\n; " (cl-tk:tcl-escape text)))


(defun write-one-dspec-and-location-for-describe (link-text location stream &key (index "end") fix-offset-p)
  "It is also used by compilation-error browse, some arbitrary string is passed instead of dspec. Beware! 
  fix-offset-p - magic boolean value, set experimentally
  "
  (let ((printed-dspec link-text))
    (multiple-value-bind (file position)
        (parse-location-into-file-and-pos location)
      (cond
        ((and file position)
         (print-one-hyperlink-tcl-source-for-describe stream printed-dspec file position :index index :fix-offset-p fix-offset-p))
        (t ; something wrong with location
         (print-just-line stream printed-dspec :index index))))))

(defun server-lookup-definition-as-list (name-or-symbol &optional (package-name (package-name *package*)) (readtable-name (named-readtables:readtable-name *readtable*)))
  "name-or-symbol is a name of a lisp object or object itself which can have definition. Returns a string which must be evaluated in tcl to print hypertext menu of links OR to jump to a location directly"
  (let* ((dspecs-and-locations
          (swank-find-definitions-for-clcon name-or-symbol #'swank::find-definitions :package-name package-name :readtable-name readtable-name))
         (l (length dspecs-and-locations)))
    (with-output-to-string (ou)
      (case l
        (0 (print-just-line-for-describe ou (format nil "No definitions found for ~A" name-or-symbol)))
        (t (dolist (dal dspecs-and-locations)
           (destructuring-bind (dspec loc) dal
             (let ((link-text (prin1-to-string dspec)))
               (write-one-dspec-and-location-for-describe link-text loc ou :fix-offset-p t)))))))))
         

;; (:location (:file "/home/denis/setup/sbcl-1.2.7/src/code/eval.lisp")
;;  (:position 4849)
;;  (:snippet "(defun simple-eval-in-lexenv (original-exp lexenv)
;;   (declare (optimize (safety 1)))
;;   ;; (aver (lexenv-simple-p lexenv))
;;   (incf *eval-calls*)
;;   (sb!c:with-compiler-error-resignalling
;;     (let ((exp (macroexpand original-exp lexenv)))
;;       (handler-bind "))


(defun ldbg-edit-frame-source-location (frame-id parent)
  "We have frame id. Make IDE to open that location. Parent is a widget. If we unable to locate to source, we will issue a message with this widget as a parent"
  (edit-swank-format-location (swank:frame-source-location frame-id) parent))

(defun edit-swank-format-location (location parent)
  "location - из EMACS. parent - widget отладчика (см. примеры). Если мы не можем попасть в исходник, мы сообщаем об этом, а видгет является родителем сообщения"
  (assert (listp location))  
  (let ((code (with-output-to-string (ou)
                (write-code-to-pass-to-loc ou location :mode :eval :|Скакнуть-от-Лиспа-к-Яру| t))))
    (eval-in-tcl (format nil "set w ~A; ~A" parent code))
    ))

(defun ldbg-edit-interpreted-frame-source-location (frame-id parent)
  (let ((data (find-data-representing-interpreted-frame-source frame-id)))
    (edit-datas-source-location-or-err data parent)))

(defun edit-datas-source-location-or-err (data parent)
  (let ((dsl (get-definition-source-location-of-data data)))
    (edit-definition-source-location-or-err dsl parent)))

(defun edit-definition-source-location-or-err (dsl parent)
  "parent нужен для ошибки"
  (assert (typep dsl '(or null sb-c::definition-source-location)))
  (let (ds swank-location) ; swank-location - то, что понятно swank (в виде s-выражения)
    (when dsl
      (setq ds (convert-definition-source-location-to-definition-source dsl)
            swank-location (swank/sbcl::definition-source-file-location ds)))
    (edit-swank-format-location swank-location parent)))
    
(defun find-interpreted-frame-definition-source-location (frame-id)
  "Возвращает sb-c:definition-source-location или nil"
  (get-definition-source-location-of-data
   (find-data-representing-interpreted-frame-source frame-id)))

(defun find-data-representing-interpreted-frame-source (frame-id)
  "Возвращает исходник интерпретируемого кода, имея на входе frame-id"
  (ignore-errors (swank::eval-in-frame 'current-form frame-id))
  )

(defun get-definition-source-location-of-data (data)
  "Если чтение было пропатчено для записи в *my-locations-hash* в момент чтения данного исходника, возвращает definition-source-location"
  (and data
       (gethash data
                ; здесь это ещё не defvar, им станет потом.
                (symbol-value '*my-locations-hash*))))

(defun convert-definition-source-location-to-definition-source (dsl)
  "Я не смог найти, где в SBCL такое преобразование делается, поэтому попробую написать это руками. В SBCL как-то слегка помоечно - много
  похожих структур, но связь между ними нелегко ищется"
  (sb-introspect::make-definition-source
   :pathname (pathname (sb-c:definition-source-location-namestring dsl))
   :form-path (list (sb-c:definition-source-location-toplevel-form-number dsl))
   :form-number (sb-c:definition-source-location-form-number dsl)))


(defun ldbg-edit-local-var-source-location (frame-id local-no parent)
  "Открывает редактор на исходнике данных из этого кадра"
  (let* ((data (swank::frame-var-value frame-id local-no)))
    (edit-datas-source-location-or-err data parent)))

(defun inspector-goto-source (parent)
  (edit-datas-source-location-or-err (swank::istate.object swank::*istate*) parent))

#|(defun ldbg-edit-interpreted-source (frame-id parent)
  "Переходим к интерпретируемому коду (требуются патчи для sb-int::load-as-source)"
  (swank::converting-errors-to-error-location
    (swank/sbcl::code-location-source-location
     (sb-di:frame-code-location (nth-frame index)))))

  ) |#

(defun open-url (url)
  #+windows
  (budden0::cmd-c "start ~A" url)
  #-windows
  (uiop:run-program (format nil "xdg-open ~A" url))
  ; (warn "Sorry, clco::open-url can not (yet) automatically open~%~A~%Please send me a patch~%" url)
  nil)

;get-token-prefix

(defun server-hyperdoc-lookup (name-or-symbol &optional (package-name (package-name *package*)) (readtable-name (named-readtables:readtable-name *readtable*)))
  "name-or-symbol is a name of a lisp object or object itself which can have definition. Returns a string which must be evaluated in tcl to print hypertext menu of links OR to jump to a location directly"
  (perga-implementation:perga
    (:@ multiple-value-bind (symbol found) 
          (parse-name-or-symbol-to-symbol name-or-symbol :package-name package-name :readtable-name readtable-name))
    (cond
     ((not found)
      (oi:message "Didnt' find symbol ~S" name-or-symbol))
     (t
      (let urls-and-types
       (hyperdoc:lookup (symbol-package symbol) (symbol-name symbol)))
      (let urls
        (mapcar 'cdr urls-and-types))
      (let unique-urls
        (remove-duplicates urls :test 'equal))
      (dolist (url unique-urls)
        (open-url url))
      nil
      ))))


