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
           #:notify-oduvan-construct-backend-buffer
           #:nti ; notify-oduvan-tcl-text-insert
           #:notify-oduvan-tcl-text-delete
           #:notify-oduvan-destroy-backend-buffer

           #:start-oduvanchik ; start oduvanchik in another thread
           )
  )

