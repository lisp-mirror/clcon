; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*- 
(in-package :clco)

(defun calc-details-code (note)
  (let* ((title (getf note :message))
         (location (getf note :location))
         (source-context (getf note :source-context))
         (references (getf note :references))
         (details-code
          (with-output-to-string (ou)
            (cond
              ((not (or location references source-context))
               nil) ; string will be empty
              (t
               (print-just-line ou title :index "end")
               (when location
                 (write-one-dspec-and-location "Go to Source" location ou :index "end")
                 (print-just-line ou (format nil "~%") :index "end"))
               (when source-context
                 (print-just-line ou source-context :index "end"))
               (when references
                 (print-just-line ou
                                  (format nil "~%References: ~S" references))
                                  ))))))
    (format nil "::erbr::AppendData ~A ~A"
            (cl-tk:tcl-escape title)
            (cl-tk:tcl-escape details-code))))
  

(defun compile-file-for-tcl (filename load-p &rest options)
  "Later we must add load-p logic if compilation failed. Now we just ignore it. Otherwise we would need
to decide how to organise dialog between parties. So we just compile, and return code to be evaluated to print result with hyperlinks"
  (let* ((compilation-result (apply #'swank:compile-file-for-emacs filename load-p options))
         (notes (swank::compilation-result-notes compilation-result))
         )
    (print notes)
    (when notes
      (eval-in-tcl "::erbr::SwankBrowseErrors1 {}")
      (dolist (note notes)
        (eval-in-tcl (calc-details-code note))
        ))))


