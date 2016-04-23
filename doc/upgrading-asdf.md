Как я апгрейдил asdf.
1. Стёр asdf.fasl и uiop.fasl из контирбов.
2. Скачал последний asdf в c:\clcon\sbcl\asdf-3.1.6
3. Запустил sbcl:
  - cmd
  - set SBCL_HOME=c:\clcon\sbcl\1.3.4
  - sbcl
3. (load "c:/clcon/sbcl/asdf-3.1.6/tools/install-asdf.lisp")
4. Установил атрибуты "только для чтения" для c:\clcon\sbcl\1.3.0\contrib\asdf.fasl, uiop.fasl, asdf.lisp. 
