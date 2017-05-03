; -*- coding: utf-8 ; Encoding: utf-8 ; System :clcon-server -*- 

(defpackage :clco
  (:nicknames :clcon-server)
  (:use :cl :swank)
  (:export #:note-this-is-tcl-connection
           #:log-to-file
           #:ldbg-edit-frame-source-location
           #:server-lookup-definition
           #:server-hyperdoc-lookup
           #:eval-in-tcl
           #:*clcon-source-directory*
                                        ; oduvan-backend
           #:notify-oduvan-construct-backend-buffer
           #:ncm
           #:nhi ; NOTIFY-ODUVAN-TCL-Прислать-данные-о-раскраске
           #:nti ; notify-oduvan-tcl-text-insert
           #:notify-oduvan-tcl-text-delete
           #:notify-oduvan-destroy-backend-buffer
           #:call-oduvanchik-function-with-clcon_text 
           #:start-oduvanchik ; start oduvanchik in another thread
           #:restart-with-name-exists-p
           #:invoke-sldb-restart-by-name
           #:load-system-for-tcl ; can be removed at some point
           #:find-string-in-files ; can be removed
           #:files-by-glob-list   ; can be removed
 
           #:inspector-goto-source

           #:current-form         ; для отладки интерпретируемого кода
           #:*my-locations-hash*  ; для отладки интерпретируемого кода
           #:get-definition-source-location-of-data ; то же

           #:*globally-disable-sldb*
           #:otladitq--n--jj-potok-v-chjornojj-konsoli
           "Выпасть-в-ред"
           #:edit-string-at-row-and-col
           #:order-call-oduvanchik-from-itself
           )
  (:import-from :budden-tools
               budden-tools:nullable))


(defpackage :clco-oduvanchik-key-bindings
  (:use :cl :oduvanchik :oduvanchik-internals)
  (:export #:set-clco-oduvanchik-key-bindings
           #:*text2odu-key-event-f8*
           #:*f17-key-event
           #:*f18-key-event*))


;(defpackage :eval-text2odu-event
;  (:use :cl :clcon-server :oduvanchik :oduvanchik-internals))
