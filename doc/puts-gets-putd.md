PUTS и GETS
=========

clcon (tkcon) подменяет puts и gets
В этом участвуют следующие имена:

::puts        - эта процедура переименовывается в ::tkcon_tcl_puts
::tkcon_puts  - эта процедура создаётся снова и испольузется в качестве алиаса
::tkcon_tcl_puts - в неё переименовывается puts

Смысл изначально был в том, чтобы перехватить все вызовы подчинённых интерпретаторов и научиться выводить их в графическую консоль. Теперь в связи с тем, что у нас нет подчинённых интерпретаторов (вроде бы нет), эта деятельность частично могла потерять смысл.

Проблема для нас состоит в том, что при выводе в графическую консоль вызывается update, поэтому мы не можем более использовать puts для отладки. Для этого мы создаём putd. 

