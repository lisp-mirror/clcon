; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*- 
(in-package :clco)

(defun compile-file-for-tcl (filename load-p &rest options)
  "Later we must add load-p logic if compilation failed. Now we just ignore it. Otherwise we would need
to decide how to organise dialog between parties. So we just compile, and return code to be evaluated to print result with hyperlinks"
  (let* ((compilation-result (apply #'swank:compile-file-for-emacs filename load-p options))
         (notes (swank::compilation-result-notes compilation-result))
         (i 0))
    (with-output-to-string (ou)
      (print-just-line ou "========= MESSAGES ===========")
      (dolist (note notes)
        (unless (= i 0)
          (print-just-line ou "---------------------------"))
        (print-just-line ou (getf note :message))
        (let ((location (getf note :location)))
          (when location
            (write-one-dspec-and-location "Source" location ou)))
        (incf i)))
    ))


