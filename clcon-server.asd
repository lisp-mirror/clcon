
(defsystem :clcon-server
  :name "clcon-server"
  :version "0.2"
  :author "Denis Budyak"
  :licence "MIT"
  :description "clcon - Common Lisp IDE"
  :long-description "Tk - based set of development tools comprising CL IDE for Linux and Windows"
  :depends-on (:cl-tk :swank
                      #+(and :clcon-oduvan (not :oduvan-invisible)) :oduvanchik.clx
                      #+(and :clcon-oduvan :oduvan-invisible) :oduvanchik.tty
                      :bordeaux-threads :budden-tools :split-sequence)
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "swank-original-functions")
               (:file "swank-rpc-original-functions")
               (:file "find-definition")
               (:file "swank-patch")
               (:file "eval-in-tcl")
               (:file "swank-inspector" :description "inspector backend pieces")
               (:file "swank-compilation-errors")
               (:file "text2odu-from-tcl-to-queue" :description "Receive editor events from swank and post them to *text2odu-event-queue*"
                      )
               #+clcon-oduvan (:file "text2odu-test-utils")
               #+clcon-oduvan (:file "oduvanchik-key-bindings" :description "fake key bindings to enable event sending from clcon"
                                     )
               #+clcon-oduvan (:file "text2odu-dispatch-to-oduvan" :description "Dispatcher thread which moves events from *text2odu-event-queue* to oduvanchik editor")
               #+clcon-oduvan (:file "eval-text2odu-event" :description "processing events on editor side")
               #+clcon-oduvan (:file "do-editing-on-tcl-side" :description "Functions to send editing primitives to tcl. Functions are hanged on oduvanchik's hooks")
               (:file "highlight-from-oduvan-to-queue" :description "Posting to *highlight-event-queue*")
               (:file "highlight-from-queue-to-tcl" :description "Sending highighting events to lisp")
               (:file "start-and-shutdown-oduvanchik" :description "start and shutdown stuff")
               (:file "clcon-oduvanchik-commands" :description "Our lisp mode and other application-level oduvanchik commands for clcon"
                      )
               )
  )
