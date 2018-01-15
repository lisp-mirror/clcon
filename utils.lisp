; -*- coding: utf-8 ; Encoding: utf-8 ; system :clcon-server ; -*- 
(in-package :clco)

(defvar *general-mutex* (bt:make-lock "clcon-server::*general-mutex*")) 

(defun standard-readtable ()
  (with-standard-io-syntax *readtable*))

(defparameter *clcon-source-directory*
  (asdf:component-pathname (asdf:find-system :clcon-server)))

(defun tcl-escape-filename (filename)
  (cl-tk:tcl-escape (SWANK/BACKEND:PATHNAME-TO-FILENAME filename)))

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
                             
(defmacro --> (object slot)
  "bubububu"
  `(slot-value ,object ',slot))

(defun terminate-lisp ()
  #+(and CCL OS-WINDOWS) (budden0::cmd-c "taskkill /PID ~D /F" (ccl::getpid))
  #+(and SBCL OS-WINDOWS) (budden0::cmd-c "taskkill /PID ~D /F" (sb-posix:getpid))
  #-OS-WINDOWS (swank:quit-lisp)
  )
