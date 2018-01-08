; -*- coding: utf-8 ; Encoding: utf-8 ; system :clcon-server ; -*- 

(in-package "SB-IMPL")

(defparameter clco:*my-locations-hash* (make-hash-table :test 'eq :weakness :key))

(defun record-sources-to-my-locations-hash ()
  (when (boundp 'sb-c::*source-paths*)
    (maphash (lambda (key value)
               (setf (gethash key clco:*my-locations-hash*)
                     (let ((sb-c::*current-path* value))
                       (sb-c::make-definition-source-location))))
           sb-c::*source-paths*)))
    
(defun decorated-find-source-paths (original-fn form tlf-num)
  "Оригинал - в c:/clcon/sbcl/1.3.4/source/src/compiler/ir1tran.lisp"
  (multiple-value-prog1 (funcall original-fn form tlf-num)
    (record-sources-to-my-locations-hash)))

(cl-advice:portably-without-package-locks
 (cl-advice:define-advice sb-c::find-source-paths 'decorated-find-source-paths))

  
