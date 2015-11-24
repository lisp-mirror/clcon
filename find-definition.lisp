; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*- 
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

(defun swank-find-definitions-for-clcon (name-or-symbol &key package-name (readtable-name nil))
  "Return a list ((DSPEC LOCATION) ...) of definitions for NAME-OR-SYMBOL. If name-or-symbol is string, we also need package-name and readtable-name. Package-name is a string, readtable-name is a keyword or nil. DSPEC is a string and LOCATION a source location. See also swank:find-definitions-for-emacs"
  (etypecase name-or-symbol
    (symbol
     (swank::find-definitions name-or-symbol))
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
       (multiple-value-bind (symbol found)
                            (swank::find-definitions-find-symbol-or-package name-or-symbol)
         (when found
           (swank::find-definitions symbol))
         )))))

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
       (let* ((escaped-file (tcl-escape-filename file))
              (offset-15 (editor-budden-tools::fix-offset-2 file offset))
              (offset-2 (format nil "{1.0+ ~A chars}" offset-15)))
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

(defmethod editor-budden-tools:goto-xy (pathname row col)
  (check-type row integer)
  (check-type col integer)
  (let* ((escaped-file (tcl-escape-filename pathname))
         (command (format nil "tkcon::EditFileAtOffset ~A ~A.~A" escaped-file row col)))
    (eval-in-tcl command :nowait nil)))

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
  "name-or-symbol is a name of a lisp object or object itself which can have definition. Returns a string which must be evaluated in tcl to print hypertext menu of links OR to jump to a location directly"
  (let* ((dspecs-and-locations
          (swank-find-definitions-for-clcon name-or-symbol :package-name package-name :readtable-name readtable-name))
         (l (length dspecs-and-locations)))
    (with-output-to-string (ou)
      (case l
        (0 (print-just-line ou (format nil "No definitions found for ~A" name-or-symbol)))
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
                  (write-one-dspec-and-location link-text loc ou))))))
         (when (> l 1)
           (write-code-to-see-console-end ou))
         )))))


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


