; -*- coding: utf-8 ; Encoding: utf-8 ; system :clcon-server ; -*- 
(in-package :clco)

(named-readtables::in-readtable nil)

(defmethod ql-dist::find-system-in-dist :around (name dist) 
(cond
  ((some 'russian-budden-tools:cyrillic-char-p (string name))
   nil)
  (t (call-next-method))))
