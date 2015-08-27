
(defsystem :clcon-server
  :name "clcon-server"
  :version "0.0"
  :author "Denis Budyak"
  :licence "MIT"
  :description "clcon - Common Lisp IDE"
  :long-description "Tk - based set of development tools comprising CL IDE for MacOS X, Linux and Windows"
  :depends-on (:cl-tk :swank)
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
               ))
