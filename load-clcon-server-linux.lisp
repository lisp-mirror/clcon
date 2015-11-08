;;; Clcon server startup script for Linux

(in-package :cl-user)

;; Enable stepping in any code we compile (but see 'Starting SWANK' below) 
(proclaim '(optimize (debug 3) (compilation-speed 0) (speed 0) (space 0) (safety 3)))

(mapc 'require '(sb-bsd-sockets sb-posix sb-introspect sb-cltl2 asdf))

(setf sb-impl::*default-external-format* :utf-8)

#-quicklisp
(let ((quicklisp-init "/s2/lib/ql.sbcl.l/setup.lisp"
                                       ))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))


;;;;;;;;;;;;;;;;; Preliminary checks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(assert (eq sb-impl::*default-external-format* :utf-8) ()
  "Clcon server unable to load: we need (eq sb-impl::*default-external-format* :utf-8)")

(assert (find-package :ql) ()
  "Clcon server unable to load: we need QUICKLISP")

(assert (find-package :asdf) ()
  "Clcon server unable to load: we need ASDF")

;;;;;;;;;;;;;;;;;; Starting SWANK ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (find-package :swank)
  (error "Clcon server unable to load: swank is loaded already. Your plan:
 1. remove swank's fasls which are normally at ~A
 2. start sbcl with: 
 sbcl --no-userinit --load <this file>

 See also https://bitbucket.org/budden/clcon/wiki/Portion%20of%20my%20.emacs%20relevant%20to%20clcon
"
         "~/.slime/fasl"))

;; Disable stepping in SWANK 
(proclaim '(optimize (debug 3) (compilation-speed 3)))

;; Load SWANK
;; Note we currently need patched version of SLIME and NAMED-READTABLES
;; which can be loaded from
;; https://github.com/budden
;; If you want to use EMACS too, relevant portion of .emacs is in WIKI (thanks to GPL)
;; https://bitbucket.org/budden/clcon/wiki/Portion%20of%20my%20.emacs%20relevant%20to%20clcon
;; clone them into your quicklisp/local-projects, e.g.
;;
;; sudo apt-get install git
;; cd ~/quicklisp/local-projects
;; git clone https://github.com/budden/named-readtables
;; git clone https://github.com/budden/slime


(load (merge-pathnames "swank-loader.lisp" (ql:where-is-system :swank)))
; (setq swank-loader::*fasl-directory* "c:/clcon/fasl-cache/swank/")
(swank-loader:init)
(swank:swank-require '(swank-asdf swank-repl swank-fancy-inspector)) 

;; Enable stepping in all other places but SWANK
(proclaim '(optimize (debug 3) (compilation-speed 0) (speed 0) (space 0) (safety 3)))

;;;;;;;;;;;;;;;; Starting auxilary server for EMACS ;;;;;;;;;;;;;;;;;;;;;;;;
(swank:create-server :port 4005 :dont-close t)

;;;;;;;;;;;;;;;;;; Trying to send all tracing to SBCL console ;;;;;;;;;;;;

;; It is useful for debugging clcon
(defvar *initial-standard-output* *standard-output*)

(defun redirect-trace-output-to-inferior-lisp (c)
  (setf (slot-value c 'swank::trace-output) *initial-standard-output*))

(push 'redirect-trace-output-to-inferior-lisp swank::*new-connection-hook*)

;;;;;;;;;;;;;;;;;; Loading dependencies ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:load-system :budden-tools-asdf)
(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :cl-tk)
;; Patch to correct absent libraries:
(ql:quickload "named-readtables")
(ql:quickload "command-line-arguments") 
(ql:quickload "cl-utilities")
 
;;;;;;;;;;;;;;;;;; Set features  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If you change any feature, clear your fasl cache and restart server
;; Cache is normally in ~/.cache/common-lisp 
 
; enable oduvanchik backend. This feature will be removed soon
; as oduvanchik backends goes well
(pushnew :clcon-oduvan *features*)
 
; If :oduvan-invisible set, we can build and start on MS Windows
; If :oduvan-invisible is not set, we can run only on Linux, but
; we also see normal Oduvanchik's GUI
; I usually develop with :oduvan-invisible disabled
(pushnew :oduvan-invisible *features*)
 
; May be needed sometimes for the debugging. 
; (pushnew :oduvan-use-sleep-in-dispatch *features*)
 
;; For debugging editor with client-server communcation enabled for
;; only single buffer. 
;; (pushnew :use-oduvan-for-first-clcon_text-pathname-only *features*)
;; if you are using trunk, you can also grep "buf1" in *.tcl to disable some debugging code in the editor.
 
; comment next line if code highlight causes problems
(pushnew :oduvan-enable-highlight *features*)


;;;;;;;;;;;;;;;;;; Carefully loading buddens-readtable ;;;;;;;;;;;;;;;;;

(asdf:load-system :budden-tools)

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

;;;;;;;;;;;;;;;;;; Loading server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(asdf:load-system :clcon-server)


;;;;;;;;;;;;;;;;;; Starting editor backend ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(clco:start-oduvanchik)


;;;;;;;;;;;;;;;;;; Starting server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; This is clcon server
(swank:create-server :port 4009 :dont-close t)

 
