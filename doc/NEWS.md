Новости clcon

<<TRUNK>> от 0.3.6
=====
- clcon вошёл в состав Яр
- обновлена команда .fics
- восстановлена работа под Linux
- обновил версию SBCL до 1.3.8
- f2 - скопировать текущий символ в буфер обмена (Хотя работает странно)
- Alt-, - вернуться туда, где были до этого (например, после перехода к определению)
- В отладчике и инспекторе можно найти исходник интерпретируемого кода (не находящегося под quote)
- частично русифицирован интерфейс
- В отладчике Alt 2 7 - напечатать стек в консоль сервера
- В редакторе Alt 1 3 - открыть файл под курсором
- в инспекторе Alt 2 1 - скопировать в "*", чтобы можно было читать из листенера
- Можно открывать файлы .lisp, .asd, .md, .ярс из оболочки ОС (двойным щелчком в проводнике),
 для этого нужно выполнить `c:\yar\bin\util\открывать-lisp-md-tcl-asd-яром.reg`
- Ctrl-Shift-t - список нитей (тредов) с возможностью отлаживать большинство из них

0.3.6. от 0.3.5
=====
- исправлены некоторые ошибки в редакторе и подсветке
- внутри строк или комментариев control-влево, control-вправо ходят по словам (раньше они плохо работали).
- добавлен индикатор состояния связи с лиспом, который показывает, что редактор "заморожен", ожидая выполнения кода на стороне сервера. 
- в консоли добавлена команда "paste as linux filename", которая полезна в Windows для вставки имени файла из буфера обмена
- в руководстве описано, как поменять шрифт
- в архиве для windows стёрто множество ненужных библиотек в Quicklisp

0.3.5 от 0.3.4
=====
- Начата русификация документации
- Улучшена утилита CallBatWithGuiDetached.exe 
- call-bat позволяет перенаправлять в файл и вызывать команды в определённой директории (имеет значение только под Windows)

0.3.4 vs 0.3.3
=====
There is a feeling that quality degraded a bit since 0.3.3.
Note that 0.3.4 may be last Engligh-language based version of clcon. 
At any time I can start localization. 

- f1 calls hyperdoc lookup. So you need some systems which hard to find. If you are on linux, download windows release and take them from windows release. 
- lisp identifier search mode instead of word search found normally in text editors. Rather dumb but useful.
- control-shift-key-right, control-shift-key-left mark lisp forms
- some editor errors are captured without invokind debugger, user is allowed to dismiss such errors
- some bugs are fixed, new bugs are added


0.3.3 vs 0.3.2
=====
- lisp completion in the editor
- basic support of editor modes. Modes are auto-selected by file type. F7's action
depends on mode. There are tcl and lisp mode. 
- documented initialization file and its use
- Block cursor in the editor - better visible
- Highlight in the editor is better visible
- Control-u clears command line
- Basic multiline command support (use `Shift-Return` to enter subsequent lines and `Return` to eval) 
- At the console, Control-Enter pastes directory and, if pressed again, file name of current editor buffer
- Support of named-readtables by the editor, see [user manual](user-manual.md)
- Most of the tools have "Window" menu with shortcuts to switch to console, editor, debugger, error browser, editor buffer list. 
- Keyboard shortcuts like "control-s" work regardless of CAPS LOCK state and in Russian keyboard layout. 
- Completion now works as in [slime-c-p-c](https://common-lisp.net/project/slime/doc/html/Compound-Completion.html#Compound-Completion)
- Fixed some bugs in "New" and "Save As"
- Fixed #||# highlight
- Maybe improved highlight colors
- Fixed bug: quit from main console menu ignored unsaved files
- Fixed bug: incorrect code navigation in the error browser
- Windows: Switched to SBCL 1.3.0


0.3.2 vs 0.3.1 
=====
- "New" and "Save As" now obey tradition
- Fixed nasty bugs in Linux version - editor didn't work at all
- Updated Linux startup script and installation instructions

0.3.1 relative to 0.3.0
================================
- package is displayed in the console and is used to find source when editing a file; previously, cl-user was always assumed

- clco:load-system-for-tcl tries to collect compilation notes to a browser. If some errors are not handled by SWANK and debugger pops up, we have new "file" menu to open current asdf component and current system. 

- we are now based on a forked SWANK and forked NAMED-READTABLES to handle readtables more correctly.
No hotpatching was developed. I hope my pull requests
will be accepted and this inconvinience will go away.

- new primitives for file search: clco:find-string-in-files
and clco:files-by-glob-list

- fixed some really nasty bugs and introduced some new bugs 

- in the editor, open dialog uses directory of current file as initial directory

