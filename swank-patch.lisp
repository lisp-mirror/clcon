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

(defun my-symbol-tcl-form (s ou)
  "We expected to send t and nil only. Otherwise warn and send garbage"
  (let* ((package (symbol-package s))
         (package-string
          (if package (package-name package) "NIL"))
         (name (symbol-name s)))
    (declare (ignorable package package-string))
    (cond
      ((member s '(nil t))
       (format ou "yCOMMON-LISP:~A "
               (cl-tk:tcl-escape name)))
      (t
       (warn "Do we really need to pass symbol ~S?" s)
       (format ou "sERROR-IN-LISP--MY-SYMBOL-TCL-FORM "
               )))))
  

(defun my-tcl-form (val ou level)
  "package must be bound to swank::*swank-io-package*, and with-standard-io-syntax must be around"
  (flet ((f (tag a)
           (format ou "~A~A " tag a)))
    (etypecase val
      ; note that keywords are downcased
      (keyword (f #\: (cl-tk:tcl-escape (string-downcase (symbol-name val)))))
      (string (f #\s (cl-tk:tcl-escape val)))
      (number (f #\n val))
      ; while other symbols are printed as is (upcased)
      (symbol (my-symbol-tcl-form val ou))
      ; top-level lists are not wrapped with {} as they are strings already
      (list
       (format ou "{l")
       (dolist (y val)
         (my-tcl-form y ou (+ level 1)))
       (format ou "} ")))))

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
         "{l:write-string sasdfasdf :repl-result } "))

(assert (string-equal 
         (clco::convert-object-to-tcl '(:return
                                        (:ok "(format destination control-string &rest format-arguments)")
                                        160))
         "{l:return {l:ok s(format\\ destination\\ control-string\\ &rest\\ format-arguments) } n160 } "))


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
     

(defvar +newline-string+ (make-string 1 :initial-element #\Newline))

(def-patched-swank-fun swank/rpc::read-form (string package)
  (let ((new-string
         (cond
           ((tcl-connection-p swank::*emacs-connection*)
            (budden-tools:search-and-replace-seq
             'string string "\\n" +newline-string+
             :all t :test 'equal))
           (t
            string))))
    (swank/rpc::swank/rpc-original-read-form new-string package)))
  
(def-patched-swank-fun swank/rpc::prin1-to-string-for-emacs (object package)
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


(def-patched-swank-fun swank::wait-for-event/event-loop (connection pattern timeout)
  (let ((swank::*emacs-connection* connection))
    (swank::swank-original-wait-for-event/event-loop connection pattern timeout)))

; insert here: (let ((*swank-connection* connection)) ... ) 
(def-patched-swank-fun swank::read-loop (connection)
  (let ((swank::*emacs-connection* connection))
    (swank::swank-original-read-loop connection)))
