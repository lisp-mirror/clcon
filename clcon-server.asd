
(defsystem :clcon-server
  :name "clcon-server"
  :version "0.0"
  :author "Denis Budyak"
  :licence "MIT"
  :description "clcon - Common Lisp IDE"
  :long-description "Tk - based set of development tools comprising CL IDE for Linux and Windows"
  :depends-on (:cl-tk :swank #+clcon-oduvan :oduvanchik.clx :bordeaux-threads :budden-tools :split-sequence)
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
               (:file "text2odu" :description "outer inteface between clcon_text widget and oduvanchik"
                      )
               #+clcon-oduven (:file "text2odu-test-utils")
               #+clcon-oduvan (:file "oduvanchik-key-bindings" :description "fake key bindings to enable event sending from clcon"
                                     )
               #+clcon-oduvan (:file "oduvan1" :description "implementation of interface declared in text2odu")
               #+clcon-oduvan (:file "eval-text2odu-event" :description "processing events on editor side")
               #+clcon-oduvan (:file "do-editing-on-tcl-side" :description "Functions to send editing primitives to tcl. Functions are hanged on oduvanchik's hooks")
               )
  )
