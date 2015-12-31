;; code from unpublished sw system, weak-hash-table.lisp

(unless (find-package :budden0)
  (defpackage :budden0 (:use :cl :budden-tools) (:export #:winmerge-strings)))

(in-package :budden0)


(defun call-bat (bat args &key (wait t) redirect-output directory)
  "Единственный надёжный способ запустить внешнюю программу из-под SBCL. Не знаю, как задать текущий каталог, видимо, нужно связывать *default-pathname-defaults*. Возвращает код возврата, если wait = t"
  (let ((arglist nil))
    (check-type redirect-output (or null string))
    (when redirect-output
      (setf arglist (append arglist `("-s" ,redirect-output))))
    (setf arglist (append arglist (list bat) args))
    (apply #'sb-ext:run-program
           "c:/clcon/bin/util/CallBatFromGuiDetached.exe"
           arglist
           (dispatch-keyargs-simple wait directory)
           )))

(defun cmd-c (format &rest args) (asdf/run-program:run-program
                                  (apply #'format nil format args)
                                  :force-shell t
                                  ))

(defun winmerge-strings (l r &key 
                           (lfile (cl-user::at-clcon-root ".winmerge-left"))
                           (rfile (cl-user::at-clcon-root ".winmerge-right"))
                           (read-result t)
                           external-format)
  "Starts merge program for couple of strings"
  (apply 'save-string-to-file l lfile (dispatch-keyargs-simple external-format))
  (apply 'save-string-to-file r rfile (dispatch-keyargs-simple external-format))
  #+lispworks (system:call-system-showing-output (format nil "c:\\utils\\winmerge.bat ~A ~A" lfile rfile) :wait t)
    #+sbcl
    (call-bat "c:\\utils\\winmerge.bat" (list (uiop/filesystem:native-namestring lfile) (uiop/filesystem:native-namestring rfile)) :wait t)
    (if read-result
        (values
         (budden-tools:read-file-into-string lfile)
         (budden-tools:read-file-into-string rfile))))
