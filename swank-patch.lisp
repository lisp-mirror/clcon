; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*- 
(in-package :clco)

(defparameter *log-file* (make-pathname :name "swank-text-log" :type "log" :defaults *default-pathname-defaults*))

(defun log-to-file (format &rest args)
  (declare (ignorable format args))
  (with-open-file (out *log-file* :direction :output :if-exists :append :if-does-not-exist :create)
    (format out format args)))

(defstruct (connection-extra-data (:conc-name ced-))
  tcl-connection-p
  )

(defvar *connections-extra-data* (swank::make-weak-key-hash-table)
  "Associates extra-data to connections")

(defun get-connection-extra-data (connection)
  (gethash connection *connections-extra-data*))

(defun ensure-connection-extra-data (connection)
  (or (get-connection-extra-data connection)
      (setf (gethash connection *connections-extra-data*)
            (make-connection-extra-data))))

(defun tcl-connection-p (connection)
  (let ((ced (gethash connection *connections-extra-data*)))
    (and ced (ced-tcl-connection-p ced))))

(defun set-tcl-connection-p (connection new-value)
  (let ((ced (ensure-connection-extra-data connection)))
    (setf (ced-tcl-connection-p ced) new-value)))

(defsetf tcl-connection-p set-tcl-connection-p)

(defun note-this-is-tcl-connection ()
  (log-to-file (prin1-to-string swank::*emacs-connection*))
  (assert swank::*emacs-connection*)
  (setf (tcl-connection-p swank::*emacs-connection*) t))

;(defvar *tcl-connection* nil "Tcl connection currently in use. Note that we initially planned to use one connection per tool, but it looks to contradict to SWANK/SLIME architecture. Will see")
;(defun tcl-connection-p (connection)
;  (gethash connection *tcl-connections*))

#|(defun convert-object-to-tcl (object package)
  "If possible, convert object to tcl commands. FIXME: use target"
  (log-to-file "entered convert-object-to-tcl")
  (swank::dcase object
    ((:write-string string &optional target)
     (log-to-file "processing write-string of ~S" string)
     (assert (keywordp target))
     (format nil "!puts target=~A;puts ~A"
             target
             (cl-tk:tcl-escape string))
     )
    (t
     nil)))|#


#|(defun convert-object-to-tcl-inner (x ou)
  (format ou "{")
  (etypecase x
    (cons
     (format ou "list")
     (dolist (y x)
       (format ou " ")
       (convert-object-to-tcl-inner y ou)
       )
     )
    (string
     (format ou "~A\"" (cl-tk:tcl-escape x))
     #| (map nil
          (lambda (y)
            (case y
              ((#\\ #\")
               (write-char #\\ ou)))
            (write-char y ou))
          x) |#)
    (integer
     (format ou "~D" x))
    (symbol
     (format ou "~A" x)))
  (format ou "}")) |#

(defun my-symbol-tcl-form (s ou)
  (let* ((package (symbol-package s))
         (package-string
          (if package (package-name package) "NIL"))
         (name (symbol-name s)))
    (format ou "y{~A ~A} "
            (cl-tk:tcl-escape package-string)
            (cl-tk:tcl-escape name))))
  

(defun my-tcl-form (val ou level)
  "package must be bound to swank::*swank-io-package*, and with-standard-io-syntax must be around"
  (flet ((f (tag a)
           (format ou "~A~A " tag a)))
    (etypecase val
      (keyword (f #\: (cl-tk:tcl-escape (string-downcase (symbol-name val)))))
      (string (f #\s (cl-tk:tcl-escape val)))
      (number (f #\n val))
      (symbol (my-symbol-tcl-form val ou))
      (list
       (format ou (if (= level 0) "l" "{l"))
       (dolist (y val)
         (my-tcl-form y ou (+ level 1)))
       (format ou (if (= level 0) "" "} "))))))

(defun convert-object-to-tcl-inner (x ou)
                                        ;(prin1 (cl-tk::tcl-form x)  ou)
  (my-tcl-form x ou 0))

(defun convert-object-to-tcl (object)
  (with-standard-io-syntax
    (let ((*package* swank::*swank-io-package*))
      (with-output-to-string (ou)
        (convert-object-to-tcl-inner object ou)))))
      


; test                                        
(assert (string-equal
         (clco::convert-object-to-tcl '(:write-string "asdfasdf" :repl-result))
         "l:write-string sasdfasdf :repl-result "))

(assert (string-equal 
         (clco::convert-object-to-tcl '(:return
                                        (:ok "(format destination control-string &rest format-arguments)")
                                        160))
         "l:return {l:ok s(format\\ destination\\ control-string\\ &rest\\ format-arguments) } n160 "))


#| examples of dcase: (defun sentinel-serve (msg)
  (dcase msg
    ((:add-connection conn)
     (push conn *connections*))
    ((:close-connection connection condition backtrace)
     (close-connection% connection condition backtrace)
     (sentinel-maybe-exit))
    ((:add-server socket port thread)
     (push (list socket port thread) *servers*))
    ((:stop-server key port)
     (sentinel-stop-server key port)
     (sentinel-maybe-exit)))) 


               (dcase (wait-for-event 
                                  `(or (:emacs-rex-rt . _)
                                       (:sldb-return ,(1+ level))))
                 ((:emacs-rex-rt &rest args) (apply #'eval-for-emacs args))
                 ((:sldb-return _) (declare (ignore _)) (return nil)))
 
|#
     

(defun swank/rpc::prin1-to-string-for-emacs (object package)
  "Note that some events can be passed to tcl connection before we know it is a tcl connection. There can be a trouble parsing that kind of event on tcl side. Good practive would be to at least warn on tcl side if that kind of event would be received"
  (when (eq (car object) :write-string)
    (log-to-file "entered prin1-to-string-for-emacs with ~S" swank::*emacs-connection*)
    )
  (cond
    ((tcl-connection-p swank::*emacs-connection*)
     (convert-object-to-tcl object)
     )
    (t
     (swank/rpc::swank/rpc-original-prin1-to-string-for-emacs object package))))

