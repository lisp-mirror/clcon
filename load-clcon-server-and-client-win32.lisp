;; -*- Encoding: utf-8; Coding: utf-8 ; -*-
;; Script to build clcon image. SBCL specific. Don't mind that lispworks is mentioned.
;; (C) Denis Budyak 2015
;; MIT License

(in-package :cl-user)

(eval-when (:compile-toplevel)
  (error "Your must not compile ~S, just load it as a source" *compile-file-truename*))

;; example of local projects. Do we need it? 
;;(ql:quickload :quickproject)
;;(quickproject:make-project "c:/clcon/lp/budden-tools" :depends-on '(:alexandria :cl-fad :split-sequence :cl-utilities :named-readtables :cl-ppcre :swank :closer-mop))
;;(quickproject:make-project "c:/clcon/lp/oduvanchik"  :depends-on '(:alexandria :trivial-gray-streams :iterate :babel :cl-ppcre :named-readtables :budden-tools :split-sequence))
;; 

;; piece from my init.lisp
(defparameter *clcon-root* #+win32 (pathname "c:/clcon/") #+unix (pathname "/s2/sw/"))

(proclaim '(optimize (debug 3) (speed 1) (compilation-speed 0) (space 0)))
(declaim (optimize (debug 3) (speed 1) (compilation-speed 0) (space 0)))

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

;(pushnew :budden *features*)
;(pushnew :russian *features*)

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
(asdf:load-system :budden-tools)

(budden-tools:def-toplevel-progn "load winmerge-strings" ()
  (load (at-clcon-root "lp/clcon/winmerge-strings.lisp")))

(BUDDEN-TOOLS:def-toplevel-progn "load :see-packages system" ()
  (asdf:load-system :see-packages) 
  (asdf:load-system :russian-budden-tools)
  )

(BUDDEN-TOOLS:def-toplevel-progn "load some systems" ()
  (asdf:load-system :perga) 
  (asdf:load-system :buddens-readtable)
  (asdf:load-system :editor-budden-tools)   
  )

; (import 'named-readtables:in-readtable :cl-user)

(setf *print-pretty* t)
;(setf *print-circle* t)

(let ((*compile-print* nil))
  (asdf::load-system
   #+oduvan-invisible :oduvanchik.tty
   #-oduvan-invisible :oduvanchik.clx
   ))

;(defun unix (program &rest args)
;  (sb-ext:run-program program args :search t :output *standard-output* :wait t))

(asdf:load-system :clcon-server)
; (asdf:load-system :lime) ; just to navigate through sources

; note this is already second swank server
; first was created in .sbclrc to be able to connect to from EMACS
(swank:create-server :port 4009 :dont-close t)

; start editor backend
(clco:start-oduvanchik)

;; start frontend. CallBatFromGuiDetached.exe is used to bypass problems
;; with run-program
(uiop/run-program:run-program "c:\\clcon\\bin\\util\\CallBatFromGuiDetached.exe c:\\clcon\\bin\\clcon-client.cmd") 

