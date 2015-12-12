;; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-
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
         (q-text (cl-tk:tcl-escape text)))
    (clco:eval-in-tcl (format nil "::tkcon::FocusConsole; puts ~A" q-text))))

(defun decorate-oduvanchik-message ()
  "To be called when starting oduvanchik from clcon"
  (decorate-function:decorate-function 'oi::message 'decorated-message))


(defun simple-listbox-menu (list &key (owner "") (title "odu::call-scrollable-menu"))
  "Call-scrollable-menu"
  (let*
      ((qlist (mapcar 'cl-tk:tcl-escape list))
       (qtitle (cl-tk:tcl-escape title))
       (cmd (format nil "::completions_menu::run [list~{ ~A~}] -owner [list ~A] -title ~A" qlist owner qtitle)))
    (clco:eval-in-tcl cmd :nowait nil)
    ))

(defmethod oi::ask-user-about-editor-condition :around (condition)
  (let* ((variants '("debug" "abort command (Esc key)" "quit lisp"))
         (users-choice 
          (simple-listbox-menu variants :title "Serious error in the editor"))
         (variant-index
          (position users-choice variants :test 'equal)))
    (ecase variant-index
      ((nil) :abort)
      (0 :debug)
      (1 :abort)
      (2 :quit))))
