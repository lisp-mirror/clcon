;; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-
;; Our lisp mode and other application-level oduvanchik commands for clcon. 
(in-package :oduvanchik)

;; Прежде всего, надо подумать, как лучше организовать работу.
;; Вообще и для поиска определений в частнотси.

(oduvanchik::defcommand "Find Source" (p)
    ""
    ""
  (let* ((s (symbol-string-at-point))
         (code (clco::server-lookup-definition s)))
    code))
  



