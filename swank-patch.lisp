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

(defun convert-object-to-tcl (object package)
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
     nil)))


; test                                        
(assert (string-equal
         (clco::convert-object-to-tcl '(:write-string "asdfasdf" :repl-result) *package*)
         "!puts target=repl-result;puts asdfasdf"))


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
  (when (eq (car object) :write-string)
    (log-to-file "entered prin1-to-string-for-emacs with ~S" swank::*emacs-connection*)
    )
  (cond
    ((tcl-connection-p swank::*emacs-connection*)
     (or
      (convert-object-to-tcl object package)
      (swank/rpc::swank/rpc-original-prin1-to-string-for-emacs object package)
      ))
    (t
     (swank/rpc::swank/rpc-original-prin1-to-string-for-emacs object package))))

