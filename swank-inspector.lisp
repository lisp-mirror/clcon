; -*- coding : utf-8 ; Encoding : utf-8 ; system :clcon-server ; -*- 
(in-package :clco)

#| 
 (defun print-one-hyperlink-tcl-source 
|#


#| Для find-definition мы не патчили swank, а написали новую функцию. Но find-definition -
  - это довольно просто. А здесь будет большой интерфейс. Может быть, стоит тогда пропатчить
  swank::istate>elisp ? 


  А также: предлагается собрать все патченные функции в одном файле. Но они будут лишь обёртками для 
  настоящих ф-й. Или наоборот, завести макрос def-patched-swank-fun , по которому мы сможем 
  их пересчитать

|#




