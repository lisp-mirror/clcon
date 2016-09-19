;; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-
;; Backend of compilation error browser, see also error-browser.erbr.tcl
(in-package :clco)

(defun calc-details-code (note serial)
  (let* ((title (getf note :message))
         (severity (getf note :severity))
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
                 (print-just-line ou (format nil "~%") :index "end")
                 )
               (when source-context
                 (print-just-line ou source-context :index "end"))
               (when references
                 (print-just-line ou
                                  (format nil "~%References: ~S" references)
                                  :index "end")
                 )))))
         (code-to-jump-to-location
          (cond
            (location
             (with-output-to-string (ou2)
               (write-code-to-pass-to-loc ou2 location :mode :eval)))
            (t
             "{}")))
         )
    (format nil "::erbr::AppendData ~A ~A ~A ~A ~A"
            serial
            (cl-tk:tcl-escape (string-downcase (string severity)))
            (cl-tk:tcl-escape title)
            (cl-tk:tcl-escape details-code)
            (cl-tk:tcl-escape code-to-jump-to-location))))
  

(defun compile-lisp-file-for-tcl (filename load-p &rest options)
  "Value of oduvanchik::compile-and-load-buffer-file-function for Lisp mode"
  (let* ((pathname (pathname filename))
         (compilation-result (apply #'swank:compile-file-for-emacs pathname load-p options))
         (success (swank::compilation-result-successp compilation-result))
         (notes (swank::compilation-result-notes compilation-result))
         (serial 0)
         )
    (when (and success load-p)
      (load (swank::compilation-result-faslfile compilation-result) :verbose t))
    (when (or notes (not success))
      (eval-in-tcl (format nil "::erbr::SwankBrowseErrors1 ~A" (CLCO::CONVERT-OBJECT-TO-TCL compilation-result)))
      (dolist (note notes)
        (eval-in-tcl (calc-details-code note (incf serial)))
        ))
    ))


(setf (oduvanchik-interface:variable-value 'oduvanchik::compile-and-load-buffer-file-function :mode "Lisp")
      'compile-lisp-file-for-tcl)


(defun compile-tcl-file-for-tcl (filename load-p &rest options)
  "Value of oduvanchik::compile-and-load-buffer-file-function for Tcl mode"
  (declare (ignore load-p options))
  (let* ((q-pathname (clco::tcl-escape-filename filename)))
    (format t "~%Loading ~A into tcl...~%" q-pathname)
    (eval-in-tcl (format nil "namespace eval :: source ~A" q-pathname)
                 )))


(setf (oduvanchik-interface:variable-value 'oduvanchik::compile-and-load-buffer-file-function :mode "Tcl")
      'compile-tcl-file-for-tcl)


(defun compile-file-for-tcl (clcon_text filename)
  "Backend for ::edt::CompileAndLoadTheFile . Rather untraditional c-s dialog. We normally do such things in tcl. It can be sometimes more convenient to edit though as we're in Lisp"
  (let ((l (length filename)))
    (when (string= (subseq filename (- l 3) l) "asd")
      (let* ((system-name 
              (subseq filename 
                      (+ 1 (position #\/ filename :from-end t)) 
                      (- l 4)))
             (system (ignore-errors (asdf:find-system system-name))))
        (cond
         ((not system) (format t "Вы пытаетесь загрузить систему ~A (из файла ~S), но asdf не нашёл такой системы. См. руководство clcon.~%" system-name filename))
         ((not (string= (namestring (asdf:system-definition-pathname system)) filename))
          (format t "Вы пытаетесь загрузить систему ~A (из файла ~S), но asdf нашёл её в файле ~S" system-name filename (namestring (asdf:system-definition-pathname system))))
         (t 
            (clco:load-system-for-tcl system))))
      (return-from compile-file-for-tcl nil)))
  (let* ((buffer (oi::clcon_text-to-buffer clcon_text))
         (mode (first (oi::buffer-modes buffer)))
         (fn (oduvanchik-interface:variable-value 'oduvanchik::compile-and-load-buffer-file-function :mode mode)))
    (funcall fn filename t)))


(defun load-system-for-tcl (system-name &rest options)
  ""
  (let* ((compilation-result (apply #'swank:operate-on-system-for-emacs system-name 'asdf:load-op options))
         (success (swank::compilation-result-successp compilation-result))
         (notes (swank::compilation-result-notes compilation-result))
         (serial 0)
         )
    (when (or notes (not success))
      (eval-in-tcl (format nil "::erbr::SwankBrowseErrors1 ~A" (CLCO::CONVERT-OBJECT-TO-TCL compilation-result)))
      (dolist (note notes)
        (eval-in-tcl (calc-details-code note (incf serial)))
        ))
    ))
