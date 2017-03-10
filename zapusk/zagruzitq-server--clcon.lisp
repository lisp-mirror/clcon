;; -*- Encoding: utf-8; Coding: utf-8 ; -*-
;; Скрипт сборки образа - windows32 - загружает clcon, но не запускает
;; (C) Denis Budyak 2015
;; MIT License

(in-package :cl-user)

(eval-when (:compile-toplevel)
  (error "Не компилируй ~S, загружай его с помощью LOAD" *compile-file-truename*))


;;;;;;;;;;;;;;;;;; Trying to send all tracing to SBCL console ;;;;;;;;;;;;

;; It is useful for debugging 
(defvar *initial-standard-output* *standard-output*)
(defvar *initial-standard-input* *standard-input*)
(defvar *initial-terminal-io* *terminal-io*)

(defun redirect-trace-output-to-inferior-lisp (c)
  (setf (slot-value c 'swank::trace-output) *initial-standard-output*))

(push 'redirect-trace-output-to-inferior-lisp swank::*new-connection-hook*)


;;;;;;;;;;;;;;;;;; End of trying to send all tracing to SBCL console ;;;;;;;;;;;;

;; Enable stepping everywhere else (this code is duplicated in .sbclrc)
(proclaim '(optimize (debug 3) (compilation-speed 0) (speed 0) (space 0) (safety 3)))

(sb-ext:RESTRICT-COMPILER-POLICY 'debug 3)
(sb-ext:RESTRICT-COMPILER-POLICY 'safety 3)
;; uncomment next two lines protect from further calls to restrict-compiler-policy forever
;;(defun ignore-all (&rest args) (declare (ignore args)))
;;(setf (symbol-function 'sb-ext:restrict-compiler-policy) #'ignore-all)

(asdf:load-system :uiop) ;; loading uiop is simple
(map () 'load ;; loading asdf/defsystem is tricky
     (mapcar 'asdf:component-pathname
             (asdf::required-components :asdf/defsystem :keep-component 'asdf:cl-source-file)))


(defun load-from-here (filename)  
  (let ((*default-pathname-defaults* *clcon-root*))
    (load filename)))

;; end of piece from my init.lisp


(EVAL-WHEN (:EXECUTE :LOAD-TOPLEVEL :COMPILE-TOPLEVEL) 
  (SETQ *READTABLE* (COPY-READTABLE *READTABLE*))
  (SETF (READTABLE-CASE *READTABLE*) :UPCASE))

; :clcon-oduvan means clcon uses oduvan. 
(pushnew :clcon-oduvan *features*)
(pushnew :oduvanchik *features*)
; If :oduvan-invisible set, we can build and start on MS Windows
; If :oduvan-invisible is not set, we can run only on Linux, but
; we also see normal Oduvanchik's GUI
; I usually develop with :oduvan-invisible disabled
(pushnew :oduvan-invisible *features*)
 
; May be needed sometimes for the debugging. 
; (pushnew :oduvan-use-sleep-in-dispatch *features*)
 
; comment next line if code highlight causes problems
(pushnew :oduvan-enable-highlight *features*)
(pushnew :clcon *features*)

;; piece from swank
#+sbcl 
(defun file-newer-p (new-file old-file)
  "Returns true if NEW-FILE is newer than OLD-FILE."
  (> (file-write-date new-file) (file-write-date old-file)))

#+sbcl 
(defun requires-compile-p (source-file)
  (let ((fasl-file (probe-file (compile-file-pathname source-file))))
    (or (not fasl-file)
        (file-newer-p source-file fasl-file))))

#+sbcl 
(defun compile-file-if-needed (filename &key load)
  (let ((pathname (SWANK/BACKEND:FILENAME-TO-PATHNAME filename)))
    (cond ((requires-compile-p pathname)
           (let ((compilation-result (compile-file pathname)))
             (when load (load compilation-result))))
          (t
           (or (not load)
           (load (compile-file-pathname pathname)))))))
;; end of piece from swank

; from budden-tools
(asdf:load-system :decorate-function) 
(load (compile-file (at-clcon-root "lp/budden-tools/asdf-3.1.4-tools.lisp")))

(asdf:load-system :iterate-keywords)
(asdf:load-system :alexandria)
(asdf:load-system :cl-fad)


;;;;;;;;;;;;;;;;;; Carefully loading buddens-readtable ;;;;;;;;;;;;;;;;;
;; we need it for editor-budden-tools, which we need in clcon. Sorry :) 
;; Note: if you want that buddens-readtable features supported your 
;; national language, use tune-russian-letters-for-buddens-readtable-a below
;; as a template
(asdf:load-system :budden-tools)

#+win32 (budden-tools:def-toplevel-progn "load winmerge-strings" ()
  (load (at-clcon-root "lp/clcon/winmerge-strings.lisp")))

(BUDDEN-TOOLS:def-toplevel-progn "load :see-packages system" ()
  (asdf:load-system :see-packages) 
  (asdf:load-system :russian-budden-tools)
  )

(defun tune-russian-letters-for-buddens-readtable-a ()
  (assert (null (named-readtables:find-readtable :buddens-readtable-a)))
  (setf budden-tools::*def-symbol-reamacro-additional-name-starting-characters*
        (append budden-tools::*def-symbol-reamacro-additional-name-starting-characters*
                russian-budden-tools::*cyrillic-characters*)))

(tune-russian-letters-for-buddens-readtable-a)


(BUDDEN-TOOLS:def-toplevel-progn "load some systems" ()
  (asdf:load-system :perga) 
  (asdf:load-system :buddens-readtable)
  (asdf:load-system :editor-budden-tools)   
  )

;; Нужность закомментаренного кода неочевидна
;;(defun test-defpackage-l2 ()
;;  (assert
;;   (budden-tools::packages-seen-p (named-readtables:find-readtable :buddens-readtable-a)))
;;  (load (compile-file (at-clcon-root "lp/budden-tools/defpackage-l2-test.lisp"))))

;; (test-defpackage-l2)

;;;;;;;;;;;;;;;;;; Setting print-pretty to t  ;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setf *print-pretty* t)
;(setf *print-circle* t)

;;;;;;;;;;;;;;;;; Loading editor backend ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((*compile-print* nil))
  (asdf::load-system
   #+oduvan-invisible :oduvanchik.tty
   #-oduvan-invisible :oduvanchik.clx
   ))


;;;;;;;;;;;;;;;;;; Loading server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:load-system :clcon-server)
; (asdf:load-system :lime) ; enable it just to navigate through sources

;;;;;;;;;; функция для запуска клиента ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun zapustitq-klienta--clcon ()
  #-(or win32 linux) #.(error "Не умею запустить-клиента--clcon на этой платформе")
  ;; CallBatFromGuiDetached.exe is used to bypass problems with run-program
  (let ((cmd 
        #+win32 (format nil "c:\\yar\\bin\\util\\CallBatFromGuiDetached.exe c:\\yar\\bin\\util\\clcon-client.cmd -swank-port ~A" *clcon-swank-port*)
        #+linux (format nil "sh /y/d/yar/bin/util/clcon-client.sh -swank-port ~A" *clcon-swank-port*)
        ))
  (uiop/run-program:run-program cmd)))


