; -*- coding : utf-8 ; Encoding : utf-8 ; System :clcon-server -*- 

(defpackage :clco
  (:nicknames :clcon-server)
  (:use :cl :swank)
  (:export #:note-this-is-tcl-connection
           #:log-to-file
           #:server-lookup-definition)
  )

