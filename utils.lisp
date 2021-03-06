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


(defun build-patched-swank-fun-advice-function-definition (swank-fun args body)
  (multiple-value-bind (docstring real-body)
                       (extract-function-docstring body)
    (let* ((function-name-string
            (with-standard-io-syntax
             (let ((*package* (find-package :keyword)))
               (format nil "(~S ~S)" 'def-patched-swank-fun swank-fun))))
           (function-name
            (intern function-name-string :clco))
           (original-fn (gensym "ORIGINAL-FN")))
      (values
       function-name
       `(defun ,function-name (,original-fn ,@args)
          ,@(when docstring (list docstring))
          (declare (ignore ,original-fn))
          ,@real-body)))))
  

(defmacro def-patched-swank-fun (swank-fun args &body body)
  "Specify that we redefine function from swank. It seems that we can also redefine functions
defined by swank::defslimefun this way"
  (multiple-value-bind (wrapper-function-name wrapper-function-definition)
                       (build-patched-swank-fun-advice-function-definition swank-fun args body)
    `(progn
       ,wrapper-function-definition
       (cl-advice:define-advice ,swank-fun ',wrapper-function-name :advice-name def-patched-swank-fun))))


(defun y-or-n-p--decorated (fn format &rest args)
  (declare (ignore fn))
  (apply #'y-or-n-p-in-emacs format args))

(cl-advice:define-advice y-or-n-p 'y-or-n-p--decorated)
                             
(defmacro --> (object slot)
  "bubububu"
  `(slot-value ,object ',slot))

(defun terminate-lisp ()
  #+(and CCL OS-WINDOWS) (budden0::cmd-c "taskkill /PID ~D /F" (ccl::getpid))
  #+(and SBCL OS-WINDOWS) (budden0::cmd-c "taskkill /PID ~D /F" (sb-posix:getpid))
  #-OS-WINDOWS (swank:quit-lisp)
  )
