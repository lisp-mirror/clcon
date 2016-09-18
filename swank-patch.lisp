; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*- 
(in-package :clco)

(named-readtables::in-readtable nil)

(defparameter *log-file* (make-pathname :name "swank-text-log" :type "log" :defaults *default-pathname-defaults*))

(defun log-to-file (format &rest args)
  (declare (ignorable format args))
  ;; 454321
  ;;(with-open-file (out *log-file* :direction :output :if-exists :append :if-does-not-exist :create)
  ;;  (format out format args))
  )

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
    (typecase val
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
       (format ou "} "))
      (t
       ; break здесь ставить нельзя
       (format ou "sERROR-IN-LISP--MY-TCL-FORM"
       )))))

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


(def-patched-swank-fun swank/sbcl::definition-source-file-location (definition-source)
  (swank/sbcl::with-definition-source (pathname form-path character-offset plist
                           file-write-date) definition-source
    (let* ((namestring (namestring (translate-logical-pathname pathname)))
           (pos (or (and form-path
                         (or
                          #+#.(swank/backend:with-symbol 'definition-source-form-number 'sb-introspect)
                          (and (sb-introspect:definition-source-form-number definition-source)
                               (ignore-errors (swank/sbcl::file-form-number-position definition-source)))
                          (ignore-errors
                           (swank/sbcl::source-file-position namestring file-write-date
                                                 form-path))))
                    character-offset
                    0 ; здесь отличие - пусть хоть файл откроется
                    ))
           (snippet (swank/sbcl::source-hint-snippet namestring file-write-date pos)))
      (swank/sbcl::make-location `(:file ,namestring)
                     ;; /file positions/ in Common Lisp start from
                     ;; 0, buffer positions in Emacs start from 1.
                     `(:position ,(1+ pos))
                     `(:snippet ,snippet)))))



(defun swank/sbcl::nth-frame (index)
  (do ((frame swank/sbcl::*sldb-stack-top* (and frame (sb-di:frame-down frame)))
       (i index (1- i)))
      ((zerop i) frame)))

(defvar *filter-frames* nil)

(def-patched-swank-fun swank::backtrace (start end)
  "Return a list ((I FRAME PLIST) ...) of frames from START to END.

I is an integer, and can be used to reference the corresponding frame
from Emacs; FRAME is a string representation of an implementation's
frame."
  (loop for frame in (swank::compute-backtrace start end)
        for i from start
        when (or (not *filter-frames*) (funcall *filter-frames* frame))
        collect (list* i (swank::frame-to-string frame)
               (ecase (swank::frame-restartable-p frame)
                 ((nil) nil)
                 ((t) `((:restartable t)))))))


(def-patched-swank-fun swank::ed-in-emacs (&optional what (in-which-connection :default))
  "Edit WHAT in Emacs.

WHAT can be:
  A pathname or a string,
  A list (PATHNAME-OR-STRING &key LINE COLUMN POSITION),
  A function name (symbol or cons),
  NIL. 

  in-which-connection может быть либо :default, тогда в (default-connection, 
  либо :first-but-not-this, тогда выбирается самая ранняя, но не текущая сессия связи с клиентом 
"
  (flet ((canonicalize-filename (filename)
           (swank::pathname-to-filename (or (probe-file filename) filename))))
    (let ((target 
           (etypecase what
             (null nil)
             ((or string pathname) 
              `(:filename ,(canonicalize-filename what)))
             ((cons (or string pathname) *)
              `(:filename ,(canonicalize-filename (car what)) ,@(cdr what)))
             ((or symbol cons)
              `(:function-name ,(prin1-to-string what)))))
          (connection
           (ecase in-which-connection
             (:default
              (or swank::*emacs-connection*
                  (swank::default-connection)
                  (error "Нет связи с клиентом SWANK")))
             (:first-but-not-this
              (cond
               (swank::*emacs-connection*
                (find-if (lambda (conn) (not (eq conn swank::*emacs-connection*))) swank::*connections*))
               (swank::*connections*
                (first swank::*connections*))
               (t (error "Нет подходящего соединения с клиентом SWANK"))))))
          )
      (cond ((eq connection swank::*emacs-connection*) (swank::send-oob-to-emacs `(:ed ,target)))
            (t
             (swank::with-connection (connection)
               (swank::send-oob-to-emacs `(:ed ,target))))))))

(defvar *post-message-thread* nil "Процесс специально для раздачи сообщений в очереди. Некое подобие пула тредов из одного треда. Подразумевается, что будем вызывать из ::tkcon::EvalInSwankAsync c идентификатором треда {:post-message-thread}, и каждое сообщение будет быстренько класть сообщение в ту или иную очередь")

(defun ensure-post-message-thread (connection)
  "По образцу swank::spawn-worker-thread"
  (or *post-message-thread*
      (setf *post-message-thread*
            (swank::spawn (lambda ()
                            (post-message-thread-function connection))
                          :name "post-message-thread"))))

(defun post-message-thread-function (connection)
  (swank::with-bindings
   swank::*default-worker-thread-bindings*
   (swank::with-top-level-restart (connection nil)
    (loop
      (apply #'swank::eval-for-emacs 
             (cdr (swank::wait-for-event `(:emacs-rex-rt . _))))))))

(defmethod swank::thread-for-evaluation ((connection swank::multithreaded-connection) (id (eql :post-message-thread)))
  (ensure-post-message-thread connection))

(defvar *globally-disable-sldb* nil "Полностью отключить SLDB и отлаживаться текстом. На самом деле переменная не глобальная и можно связывать её в отдельных потоках")

(defun make-caller-releasing-foreground (fn)
  "Для вызова консольного отладчика"
  (lambda ()
    (unwind-protect
        (funcall fn)
      (sb-thread:release-foreground))))

;;; 

(defmacro with-redirection-to-black-console (&body body)
  `(let ((*standard-input* cl-user::*initial-standard-input*)
         (*standard-output* cl-user::*initial-standard-output*)
         (*error-output* cl-user::*initial-standard-output*)
         (*trace-output* cl-user::*initial-standard-output*)
         (*debug-io* cl-user::*initial-terminal-io*)
         (*query-io* cl-user::*initial-terminal-io*)
         (*terminal-io* cl-user::*initial-terminal-io*))
     ,@body))

;;;FIXME ПРАВЬМЯ - скопировать исходную ф-ю куда надо
(DEFUN SWANK::DEBUG-IN-EMACS (CONDITION)
      (if *globally-disable-sldb*
          (with-redirection-to-black-console 
           (swank::invoke-default-debugger condition))
          (LET ((SWANK::*SWANK-DEBUGGER-CONDITION* CONDITION)
                (SWANK::*SLDB-RESTARTS* (COMPUTE-RESTARTS CONDITION))
                (*SLDB-QUIT-RESTART*
                 (AND *SLDB-QUIT-RESTART* (FIND-RESTART *SLDB-QUIT-RESTART*)))
                (*PACKAGE*
                 (OR
                  (AND (BOUNDP 'SWANK::*BUFFER-PACKAGE*)
                       (SYMBOL-VALUE 'SWANK::*BUFFER-PACKAGE*))
                  *PACKAGE*))
                (SWANK::*SLDB-LEVEL* (1+ SWANK::*SLDB-LEVEL*))
                (SWANK::*SLDB-STEPPING-P* NIL))
            (SWANK::FORCE-USER-OUTPUT)
            (SWANK/BACKEND:CALL-WITH-DEBUGGING-ENVIRONMENT
             (LAMBDA () (SWANK::SLDB-LOOP SWANK::*SLDB-LEVEL*))))))


(defun break-thread-in-black-console (thread)
  (sb-thread:interrupt-thread (sb-thread:main-thread)
                              (lambda ()
                                (sb-thread:release-foreground thread)))
  (sb-thread:interrupt-thread thread 
                              (lambda ()
                                (unwind-protect
                                    (let ((*globally-disable-sldb* t))
                                      (break))
                                  (sb-thread:release-foreground)))))

