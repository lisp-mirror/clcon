; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*- 
(in-package :clco)

(defun standard-readtable ()
  (with-standard-io-syntax *readtable*))


(defun swank-find-definitions-for-clcon (name)
  "Return a list ((DSPEC LOCATION) ...) of definitions for NAME.
DSPEC is a string and LOCATION a source location. NAME is a string. See also swank:find-definitions-for-emacs. FIXME must be called in the context of current buffer"
  (let ((swank::*buffer-package* (find-package :common-lisp-user))
        (swank::*buffer-readtable* (standard-readtable)))
    (multiple-value-bind (symbol found)
        (swank::find-definitions-find-symbol-or-package name)
      (when found
        (swank::find-definitions symbol))
    )))

(defun print-one-hyperlink-tcl-source (stream text file line)
  "Generates tcl code which prints out one hyperlink"
  (format stream "puts ~A"
          (cl-tk:tcl-escape
           (format nil "~A ~A ~A"
                   text file line))))


(defun server-lookup-definition (text)
  "Returns a string which must be evaluated in tcl to print hypertext menu of links"
  (with-output-to-string (ou)
    (print-one-hyperlink-tcl-source ou "myproc" "/s2/clcon/buka.tcl" 1)
  ))


#| 

 (defmethod on-lookup-definition ((text ltk:text))
  (let* ((name (get-current-token text))
         (definitions (swank-find-definitions-for-ale name)))
    (cond
      ((null definitions)
       (info-message "no src location found")
       )
      ((second definitions) ; more than one definition
       (info-message "multiple definitions"))
      (t
       (budden-navigate-to-definition (first definitions))))))


 (defun budden-navigate-to-definition (swank-def)
  "swank-def is one definition from swank-find-definitions-for-ale"
  (destructuring-bind (dspec location) swank-def
    (declare (ignore dspec))
    (cond
      ((and (eq (car location) :location)
            (eq (car (second location)) :file)
            (eq (car (third location)) :position))
       (let ((file (second (second location)))
             (position (second (third location))))
         (budden-open-file-at-position file position))))))
|#
