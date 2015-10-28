; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*- 
(in-package :clco)

(defun swank-find-definitions-for-clcon (name package-name)
  "Return a list ((DSPEC LOCATION) ...) of definitions for NAME.
DSPEC is a string and LOCATION a source location. NAME is a string. See also swank:find-definitions-for-emacs. FIXME must be called in the context of current buffer"
  (let* ((package-or-nil (find-package package-name))
         (swank::*buffer-package* 
          (cond
           (package-or-nil
            package-or-nil
            )
           (t
            (error "Package ~S not found" package-name)
            )))
         (swank::*buffer-readtable* (standard-readtable)))
    (multiple-value-bind (symbol found)
        (swank::find-definitions-find-symbol-or-package name)
      (when found
        (swank::find-definitions symbol))
    )))

(defun print-one-hyperlink-tcl-source (stream text file offset &key (index "output"))
  "Generates tcl code which prints out one hyperlink"
  (let ((escaped-text (cl-tk:tcl-escape text))
        (escaped-file (tcl-escape-filename file))
        (offset-2 (format nil "{1.0+ ~A chars}" offset))
        )
    (format stream "::tkcon::WriteActiveText $w ~A ~A {::tkcon::EditFileAtOffset ~A ~A}; $w insert ~A \\\n; "
            escaped-text
            index
            escaped-file
            offset-2
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

(defun write-one-dspec-and-location (link-text location stream &key (index "output"))
  "It is also used by compilation-error browse, some arbitrary string is passed instead of dspec. Beware!"
  (let ((printed-dspec link-text))
    (multiple-value-bind (file position)
        (parse-location-into-file-and-pos location)
      (cond
        ((and file position)
         (print-one-hyperlink-tcl-source stream printed-dspec file position :index index))
        (t ; something wrong with location
         (print-just-line stream printed-dspec :index index))))))

(defun write-code-to-pass-to-loc (stream loc &key (mode :text))
  "Writes code which would help to pass to location. 
   If mode = :text we will insert the code into text widget. 
   If mode = :eval we will eval the code in the context where $w contains some widget. 
This widget is required as a parent of tk_messageBox which we show when we unable to locate
and which is activated after showing message box. See also write-code-to-pass-to-file-line-char
"
  (multiple-value-bind (file offset)
      (parse-location-into-file-and-pos loc)
    (cond
      ((and file offset)
       (let ((escaped-file (tcl-escape-filename file))
             (offset-2 (format nil "{1.0+ ~A chars}" offset)))
         (format stream "tkcon::EditFileAtOffset ~A ~A" escaped-file offset-2)))
      (t
       (let* ((qLocation (cl-tk:tcl-escape (prin1-to-string loc)))
              (message (format nil "Unable to locate to definition ~A" qLocation)))
         (ecase mode
           (:text 
            (print-just-line stream message))
           (:eval
            (format
             stream
             "tk_messageBox -parent $w -message {~A} ~% focus $w" message)
            )))
       ))))



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
  

(defun server-lookup-definition (text package-name)
  "text is a name of a lisp object which can have definition. Returns a string which must be evaluated in tcl to print hypertext menu of links OR to jump to a location directly"
  (let* ((dspecs-and-locations
          (swank-find-definitions-for-clcon text package-name))
         (l (length dspecs-and-locations)))
    (with-output-to-string (ou)
      (case l
        (0 (print-just-line ou (format nil "No definitions found for ~A" text)))
        (t
         (when (> l 1)
           (write-code-to-show-console ou))
         (dolist (dal dspecs-and-locations)
           (destructuring-bind (dspec loc) dal
             (cond
               ((= l 1)
                (write-code-to-pass-to-loc ou loc))
               (t
                (let ((link-text (prin1-to-string dspec)))
                  (write-one-dspec-and-location link-text loc ou)))))))))))


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
  (let ((location (swank:frame-source-location frame-id)))
    (when location
      (let ((code (with-output-to-string (ou)
                    (write-code-to-pass-to-loc ou location :mode :eval))))
        (eval-in-tcl (format nil "set w ~A; ~A" parent code))
        ))))



;get-token-prefix


#| 

(defun budden-open-file-at-position (file position)
  (check-type position fixnum)
  (open-file (correct-path file))
  (budden-goto-position (get-current-text-ctrl *buffer-manager*) position))
  
(defmethod budden-goto-position ((text ltk:text) position)
  "clone of goto"
  (check-type position integer)
  (let ((cursor-pos (format nil "{1.0+ ~A chars}" position)))
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
