; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*- 
(in-package :clco)

#| 
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

|#

