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


(defun extract-function-docstring (body)
  "Returns two values: docstring and real body"
  (let ((head (car body))
        (tail (cdr body)))
    (cond
      ((null tail)
       (values nil body))
      ((stringp head)
       (values head tail))
      (t
       (values nil body)))))

(defmacro def-patched-swank-fun (swank-fun args &body body)
  "Specify that we redefine function from swank. It seems that we can also redefine functions
defined by swank::defslimefun this way"
  (multiple-value-bind (docstring real-body)
      (extract-function-docstring body)
    (declare (ignore docstring))
    ; FIXME - save docstring somehow. 
    `(defun ,swank-fun ,args ,@real-body)))
                             
