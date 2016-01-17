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


(defcommand "Hyperdoc Lookup" (p)
    "Hyperdoc lookup"
    ""
  (multiple-value-bind (string symbol) (get-symbol-from-current-point)
    (let ((code
           (clco:server-hyperdoc-lookup (or symbol string)
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
             (simple-listbox-menu completion-list :title "Comletions:")))
        (unless (string= choice "")
          (replace-str-with choice)))
      )))))

