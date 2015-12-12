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


(defun completions-menu-run (list &key (owner "") (title "odu::call-scrollable-menu"))
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

