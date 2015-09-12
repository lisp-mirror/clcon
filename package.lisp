; -*- coding : utf-8 ; Encoding : utf-8 ; System :clcon-server -*- 

(defpackage :clco
  (:nicknames :clcon-server)
  (:use :cl :swank)
  (:export #:note-this-is-tcl-connection
           #:log-to-file
           #:ldbg-edit-frame-source-location
           #:server-lookup-definition
           #:eval-in-tcl
           #:*clcon-source-directory*
                                        ; oduvan-backend
           #:make-oduvan-backend-buffer
           #:notify-oduvan-on-tcl-text-change
           )
  )

