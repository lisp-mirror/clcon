; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*-
; see also text2odu.tcl, text2odu.lisp

; this file is only built if :clcon-oduvan feature present
; threads: we do not assume in which thread notification functions are called. 
; we have to design threading model for editor. E.g. one thread per buffer, one 
; dispatcher thread. Or no dedicated dispatcher thread.


(in-package :clco)

#| Useful functions:

 oduvanchik-internals::after-editor-initializations - run code after editor initialized


|#

