;; -*- Encoding: utf-8; Coding: utf-8 ; -*-
;; Script to build clcon image. SBCL specific. Don't mind that lispworks is mentioned.
;; (C) Denis Budyak 2015
;; MIT License

(in-package :cl-user)

(eval-when (:compile-toplevel)
  (error "Your must not compile ~S, just load it as a source" *compile-file-truename*))


;;;;;;;;;;;;;;;;;; Trying to send all tracing to SBCL console ;;;;;;;;;;;;

;; It is useful for debugging 
(defvar *initial-standard-output* *standard-output*)

(defun redirect-trace-output-to-inferior-lisp (c)
  (setf (slot-value c 'swank::trace-output) *initial-standard-output*))

(push 'redirect-trace-output-to-inferior-lisp swank::*new-connection-hook*)


;;;;;;;;;;;;;;;;;; End of trying to send all tracing to SBCL console ;;;;;;;;;;;;

;; piece from my init.lisp
(defparameter *clcon-root* #+win32 (pathname "c:/clcon/") #+unix (pathname "/s2/sw/"))

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

(defun pred-dir (dir)
  (make-pathname :defaults dir :directory (butlast (pathname-directory *clcon-root*))))

(defun at-clcon-root (path) "Path relative to *clcon-root*"
  (merge-pathnames path *clcon-root*))


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
(pushnew :oduvan-invisible *features*)
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

(budden-tools:def-toplevel-progn "load winmerge-strings" ()
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

;;;;;;;;;;;;;;;;;; Starting server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; note this is already second swank server
; first was created in .sbclrc to be able to connect to from EMACS
(swank:create-server :port *clcon-swank-port* :dont-close t)

;;;;;;;;;;;;;;;;;; Starting editor backend ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(clco:start-oduvanchik)

;;;;;;;;;;;;;;;;;; Starting editor frontend ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CallBatFromGuiDetached.exe is used to bypass problems with run-program
(let ((cmd (format nil "c:\\clcon\\bin\\util\\CallBatFromGuiDetached.exe c:\\clcon\\bin\\clcon-client.cmd -swank-port ~A" *clcon-swank-port*)))
  (uiop/run-program:run-program cmd))

