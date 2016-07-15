; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*- 

(in-package "SB-IMPL")

(defparameter clco:*my-locations-hash* (make-hash-table :test 'eq :weakness :key))

(defun record-sources-to-my-locations-hash ()
  (when (boundp 'sb-c::*source-paths*)
    (maphash (lambda (key value)
               (setf (gethash key clco:*my-locations-hash*)
                     (let ((sb-c::*current-path* value))
                       (sb-c::make-definition-source-location))))
           sb-c::*source-paths*)))
    

;; sb-impl::eval-tlf is defined in c:/clcon/sbcl/1.3.4/source/src/code/eval.lisp
(defun decorated-eval-tlf (original-fn original-exp tlf-index &optional lexenv)
  (record-sources-to-my-locations-hash)
  (funcall original-fn original-exp tlf-index lexenv))

(decorate-function:portably-without-package-locks
 (decorate-function:decorate-function 'sb-impl::eval-tlf 'decorated-eval-tlf))


