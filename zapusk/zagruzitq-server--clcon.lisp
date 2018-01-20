;; -*- Encoding: utf-8; Coding: utf-8 ; -*-
;; Скрипт сборки образа - windows32 - загружает clcon, но не запускает
;; (C) Denis Budyak 2015
;; MIT License

(in-package :cl-user)

(eval-when (:compile-toplevel)
  (error "Не компилируй ~S, загружай его с помощью LOAD" *compile-file-truename*))
  
;;;;;;;;;;;;;;;;;; SBCL: enabling finding sources ;;;;;;;;;;;;

#+SBCL
(defun zagruzitq-server--clcon--set-sbcl-home-location () 
 (let* ((sbcl-home-string (sb-posix:getenv "SBCL_HOME")))
  ;(break "~S" sbcl-home-string)
  (unless (member (elt sbcl-home-string (- (length sbcl-home-string) 1)) '(#\\ #\/))
     (setq sbcl-home-string (concatenate 'string sbcl-home-string "/")))
  (sb-ext::set-sbcl-source-location
   ;; run-sbcl.sh changes SBCL_HOME, so this is an empirical fix to approach actual location of sources
   (merge-pathnames #+linux "../../" #-linux "source/" (pathname sbcl-home-string)))))
   
#+SBCL
(zagruzitq-server--clcon--set-sbcl-home-location)

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
#+sbcl (proclaim '(optimize (debug
                      1 
                      ; 3 ; disable for temp because we're debugging stepper
                      )
                     (compilation-speed 0) (speed 0) (space 0) (safety 3)))

#+sbcl (sb-ext:RESTRICT-COMPILER-POLICY 'safety 2)
;; uncomment next two lines protect from further calls to restrict-compiler-policy forever
;;(defun ignore-all (&rest args) (declare (ignore args)))
;;(setf (symbol-function 'sb-ext:restrict-compiler-policy) #'ignore-all)

;;(asdf:load-system :uiop) ;; loading uiop is simple
;;(map () 'load ;; loading asdf/defsystem is tricky
;;     (mapcar 'asdf:component-pathname
;;             (asdf::required-components :asdf/defsystem :keep-component 'asdf:cl-source-file)))

(setf asdf::*asdf-verbose* t)

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

(assert (find-package :swank) () "Nam nuzhnyy funkcii is Swank: swank::file-newer-p, swank::requires-compile-p, swank::compile-file-if-needed")

(load (at-clcon-root "lp/budden-tools/cl-advice/load-cl-advice.lisp"))
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

#+os-windows (budden-tools:def-toplevel-progn "load winmerge-strings" ()
  (load (at-clcon-root "lp/clcon/winmerge-strings.lisp")))

(BUDDEN-TOOLS:def-toplevel-progn "load :buddens-reader system" ()
  (asdf:load-system :buddens-reader)
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
  #-(or os-windows linux) (let "Ne umeyu zapustitq-klienta--clcon na ehtoyi platforme")
  #-(or CCL SBCL) (let "Ne umeyu zapustitq-klienta--clcon is ehtoyy realizacii lispa")
  (let* ((util-directory
          (uiop/filesystem:native-namestring
           (putq-otnositelqno-kornya-yara "bin/util")))
         #+(and CCL OS-WINDOWS) (util-directory (substitute #\\ #\/ util-directory)) ; Нет, ccl:native-translated-namestring тоже не работает
         (lisp-name #+CCL "Кложа" #+#:YSBCL "ЯСБЦЛ" #+(and SBCL (not #:YSBCL)) "СБЦЛ" #-(or SBCL CCL) "Nevedomyyyi-lisp")
         (version #+SBCL (lisp-implementation-version)
                  #+CCL (format nil "~D.~D-~A" ccl::*openmcl-major-version* ccl::*openmcl-minor-version* ccl::*openmcl-revision*))
         (lisp-name-and-version (format nil "~A-~A" lisp-name version))
         (cmd
          #+os-windows
          (format nil "~A\\CallBatFromGuiDetached.exe ~A\\clcon-client.cmd -swank-port ~A -lisp-title ~S" util-directory util-directory *clcon-swank-port* lisp-name-and-version)
          #+linux
          (format nil "sh ~A/clcon-client.sh -swank-port ~A -lisp-title ~S"
                  util-directory *clcon-swank-port*
                  lisp-name-and-version)))
  (uiop/run-program:run-program cmd)))


