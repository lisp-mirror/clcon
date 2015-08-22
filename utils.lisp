; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*- 
(in-package :clco)

(defun standard-readtable ()
  (with-standard-io-syntax *readtable*))

(defun tcl-escape-filename (filename)
  (cl-tk:tcl-escape (SWANK/BACKEND:PATHNAME-TO-FILENAME filename)))

#| This is just an example 
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
|#


