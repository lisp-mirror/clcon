; -*- coding: utf-8 ; Encoding: utf-8 ; system :clcon-server -*- 
(in-package :swank/rpc)

;; there are functions we patch. Their copies with swank-original- prefix are given here
;; we could also use decorate-function

(defun swank/rpc-original-prin1-to-string-for-emacs (object package)
  (with-standard-io-syntax
    (let ((*print-case* :downcase)
          (*print-readably* nil)
          (*print-pretty* nil)
          (*package* package)
          ;; Emacs has only double floats.
          (*read-default-float-format* 'double-float))
      (prin1-to-string (switch-to-double-floats object)))))


