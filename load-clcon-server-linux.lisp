;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SERVER STARTing Script START
(in-package :cl-user)

(when (find-package :swank)
  (error "Unable to load: swank is loaded already. Your plan:
 1. remove swank initialization from initialization files
 2. remove swank's fasls which are normally at ~/.slime/fasl 
 3. retry"))

;; Disable stepping in SWANK 
(proclaim '(optimize (debug 3) (compilation-speed 3)))
(asdf:oos 'asdf:load-op 'swank)

;; trying to send all tracing to a single stream
(defvar *initial-standard-output* *standard-output*)

(defun redirect-trace-output-to-inferior-lisp (c)
  (setf (slot-value c 'swank::trace-output) *initial-standard-output*))

(push 'redirect-trace-output-to-inferior-lisp swank::*new-connection-hook*)

;; loading dependencies
(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :swank) ; don't need it if you have SLIME already
(ql:quickload :cl-tk)
;; Patch to correct absent libraries:
(ql:quickload "named-readtables")
(ql:quickload "command-line-arguments") 
(ql:quickload "cl-utilities")
 
;; Set some features. 
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
 
;; Setting paths and loading
;(pushnew #P"path/to/clcon-server-source/" asdf:*central-registry* :test 'equalp)

#| (let ((clcondir (merge-pathnames
                 (make-pathname
                  :directory
                  '(:relative "clcon/"))
                 ql:*quicklisp-home*))
      (oduvandir (merge-pathnames
                  (make-pathname  
                   :directory
                   '(:relative ".quicklisp/local-projects/oduvanchik/"))
                  (user-homedir-pathname)))
      )
  (pushnew clcondir asdf:*central-registry* :test 'equalp)
  (pushnew oduvandir asdf:*central-registry* :test 'equalp)
  ) |#

;in windows you can write like this:
;(pushnew #P"C:/xxx/xxx/clcon/" asdf:*central-registry* :test 'equalp)
 
(asdf:load-system :clcon-server)
(swank:create-server :port 4009 :dont-close t)
 
;;; end of script 
