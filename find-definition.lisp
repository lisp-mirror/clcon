; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*- 
(in-package :clco)

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

(defun print-one-hyperlink-tcl-source (stream text file offset)
  "Generates tcl code which prints out one hyperlink"
  (let ((escaped-text (cl-tk:tcl-escape text))
        (escaped-file (tcl-escape-filename file))
        (offset-2 (format nil "{0.0+ ~A chars}" offset))
        )
    (format stream "::tkcon::WriteActiveText $w ~A output {::tkcon::EditFileAtOffset ~A ~A}; $w insert output \\\n; "
            escaped-text
            escaped-file
            offset-2)))

(defun print-just-line (stream text)
  (format stream "::tkcon::WritePassiveText $w ~A output; $w insert output \\\n; " (cl-tk:tcl-escape text)))


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

(defun write-one-dspec-and-location (dspec location stream)
  "It is also used by compilation-error browse, some arbitrary string is passed instead of dspec. Beware!"
  (let ((printed-dspec (prin1-to-string dspec)))
    (multiple-value-bind (file position)
        (parse-location-into-file-and-pos location)
      (cond
        ((and file position)
         (print-one-hyperlink-tcl-source stream printed-dspec file position))
        (t ; something wrong with location
         (print-just-line stream printed-dspec))))))

(defun write-code-to-pass-to-loc (stream loc)
  (multiple-value-bind (file offset)
      (parse-location-into-file-and-pos loc)
    (cond
      ((and file offset)
       (let ((escaped-file (tcl-escape-filename file))
             (offset-2 (format nil "{0.0+ ~A chars}" offset)))
         (format stream "tkcon::EditFileAtOffset ~A ~A" escaped-file offset-2)))
      (t
       (print-just-line stream "Unable to locate to definition")
       ))))
  

(defun server-lookup-definition (text)
  "Returns a string which must be evaluated in tcl to print hypertext menu of links"
  (let* ((dspecs-and-locations
          (swank-find-definitions-for-clcon text))
         (l (length dspecs-and-locations)))
    (with-output-to-string (ou)
      (case l
        (0 (print-just-line ou "No definitions found"))
        (t
         (dolist (dal dspecs-and-locations)
           (destructuring-bind (dspec loc) dal
             (cond
               ((= l 1)
                (write-code-to-pass-to-loc ou loc))
               (t
                (write-one-dspec-and-location dspec loc ou))))))))))



;сюда нужно перенести код из oduvanchik по выделению символа и применить его для вызова лиспового комплишена из clcon

;get-token-prefix


#| 

(defun budden-open-file-at-position (file position)
  (check-type position fixnum)
  (open-file (correct-path file))
  (budden-goto-position (get-current-text-ctrl *buffer-manager*) position))
  
(defmethod budden-goto-position ((text ltk:text) position)
  "clone of goto"
  (check-type position integer)
  (let ((cursor-pos (format nil "{0.0+ ~A chars}" position)))
    (ltk::scroll-to text cursor-pos)
    (ltk::set-cursor-pos text cursor-pos)
    (ltk::force-focus text)))


able::budden-open-file-at-position

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
