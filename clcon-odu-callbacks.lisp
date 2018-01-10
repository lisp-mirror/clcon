;; -*- coding: utf-8 ; Encoding: utf-8 ; system :clcon-server ; -*-
;; Commands which oduvanchik calls back into tcl
(in-package :oduvanchik)
(named-readtables::in-readtable :oduvanchik-ext-readtable)

(defun bell-with-tcl (&optional stream)
  (declare (ignore stream))
  "Value for oi::*beep-function*"
  (clco:eval-in-tcl "bell")
  )


(defun decorated-message (fn string &rest args)
  "odu::decorated-message . Original oi::message in echo.lisp"
  (declare (ignore fn))
  (let* ((new-string (concatenate 'string "odu message: " string))
         (text (apply 'format nil new-string args))
         (q-text (cl-tk:tcl-escape text))
         (code
          (with-output-to-string (code)
            (CLCO:WRITE-CODE-TO-SEE-CONSOLE-END code)
            (clco:write-code-to-show-console code)
            (format code "::tkcon::WritePassiveText [::tkcon::CurrentConsole] ~A output~%" q-text)
            (CLCO:WRITE-CODE-TO-SEE-CONSOLE-END code))))
    (clco:eval-in-tcl code)))

(defun decorate-oduvanchik-message ()
  "To be called when starting oduvanchik from clcon"
  (cl-advice:define-advice oi::message 'decorated-message))


(defun simple-listbox-menu (list &key (owner "") (title "odu::call-scrollable-menu"))
  "Call-scrollable-menu"
  (let*
      ((qlist (mapcar 'cl-tk:tcl-escape list))
       (qtitle (cl-tk:tcl-escape title))
       (cmd (format nil "::completions_menu::run [list~{ ~A~}] -owner [list ~A] -title ~A" qlist owner qtitle)))
    (clco:eval-in-tcl cmd :nowait nil)
    ))


(defmethod oi::ask-user-about-editor-condition ((condition editor-error))
  "editor-error is normal. If it has message, show it. If not, just beep. Then abort it (that is, ignore)"
  (let* ((message (format nil "~A" condition)))
    (cond
     ((string= message "")
      (beep))
     (t
      (print `(message ,message) *trace-output*)
      (let*
          ((qmessage (cl-tk:tcl-escape message))
           (cmd
            (format nil "tk_messageBox -message ~A -parent [::edt::c_text]" qmessage)
            ))
        (clco:eval-in-tcl cmd :nowait nil))
      ))
    :abort))

(defmethod oi::ask-user-about-editor-condition ((condition serious-editor-error))
  (let* ((variants '("debug" "abort command (Esc key)" "quit lisp"))
         (message (format nil "~A" condition))
         (qmessage (cl-tk:tcl-escape message))
         (cmd
          (format nil "tk_messageBox -message ~A" qmessage))
         (dummy1 (clco:eval-in-tcl cmd :nowait nil))
         (users-choice 
          (simple-listbox-menu variants :title "Serious error in the editor"))
         (variant-index
          (position users-choice variants :test 'equal)))
    (declare (ignore dummy1))    
    (ecase variant-index
      ((nil) :abort)
      (0 :debug)
      (1 :abort)
      (2 :quit))))
