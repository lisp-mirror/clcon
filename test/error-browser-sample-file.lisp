; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*- 
(in-package :clco)

(defun bar () xxx
    (bar 75)
    )

(eval-when (:load-toplevel)
  (print "I'm loading"))