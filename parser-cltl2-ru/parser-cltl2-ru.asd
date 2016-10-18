(defsystem :parser-cltl2-ru
  :name "parser-cltl2-ru"
  :version "0.1.0"
  :author "Roman Klochkov"
  :licence "MIT"
  :description "Parser of CLTL2"
  :long-description "Parser of http://filonenko-mikhail.github.io/cltl2-doc/ru/symbols.html"
  :depends-on (:drakma)
  :serial t
  :components ((:file "package")
               (:file "parser")))
