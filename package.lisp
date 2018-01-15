; -*- coding: utf-8 ; Encoding: utf-8 ; System :clcon-server -*- 

(named-readtables:in-readtable nil)

(def-merge-packages::! :clco
  (:nicknames :clcon-server)
  (:use :cl :swank)
  (:export "clco:note-this-is-tcl-connection
           clco:log-to-file
           clco:ldbg-edit-frame-source-location
           clco:server-lookup-definition
           clco:|Обслужить-команду-поиска-связей-символа|
           clco:server-hyperdoc-lookup
           clco:eval-in-tcl
           clco:*clcon-source-directory*
                                        ; oduvan-backend
           clco:notify-oduvan-construct-backend-buffer
           clco:ncm
           clco:nhi ; NOTIFY-ODUVAN-TCL-Лисп-зпт-пришли-раскраску
           clco:nti ; notify-oduvan-tcl-text-insert
           clco:notify-oduvan-tcl-text-delete
           clco:notify-oduvan-destroy-backend-buffer
           clco:call-oduvanchik-function-with-clcon_text 
           clco:start-oduvanchik ; start oduvanchik in another thread
           clco:restart-with-name-exists-p
           clco:invoke-sldb-restart-by-name
           clco:operate-on-system-for-tcl ; can be removed at some point
           clco:find-string-in-files ; can be removed
           clco:files-by-glob-list   ; can be removed
 
           clco:inspector-goto-source

           clco:current-form         ; для отладки интерпретируемого кода
           clco:*my-locations-hash*  ; для отладки интерпретируемого кода
           clco:get-definition-source-location-of-data ; то же

           clco:*globally-disable-sldb*
           clco:otladitq--n--jj-potok-v-chjornojj-konsoli
           clco:|Выпасть-в-ред|
           clco:edit-string-at-row-and-col
           clco:order-call-oduvanchik-from-itself
           clco:entire-buffer-string

           CLCO:PRINT-JUST-LINE
           CLCO:WRITE-CODE-TO-SEE-CONSOLE-END
           clco:write-code-to-show-console

           CLCO::WRITE-ONE-DSPEC-AND-LOCATION
           clco::write-code-to-pass-to-loc
           clco::write-one-dspec-and-location-for-describe
           clco:*filter-frames* ; фильтрация кадров в отладчике, см. руководство по clcon
           clco:terminate-lisp
            "
           )
  (:import-from :budden-tools
               budden-tools:nullable))


(def-merge-packages::! :clco-oduvanchik-key-bindings
  (:use :cl :oduvanchik :oduvanchik-internals)
  (:export #:set-clco-oduvanchik-key-bindings
           #:*text2odu-key-event-f8*
           #:*f17-key-event
           #:*f18-key-event*))


;(defpackage :eval-text2odu-event
;  (:use :cl :clcon-server :oduvanchik :oduvanchik-internals))
