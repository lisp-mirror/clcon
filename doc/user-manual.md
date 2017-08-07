Clcon 0.3.8 - Руководство пользователя


Аргументы, параметры, ключи, опции командной строки
--------------------
Most of [tkcon's command line options](http://tkcon.sourceforge.net/docs/start.html), are kept intact.
New options: 

`-putd-output-file <filename>` - file to print debug messages to. We can not print them to console correctly, as this calls update inside. 

`-putd-enabled 1` - enable printing lots of debug messages. `-putd-enabled 0` - to disable (default). putd-enabled can be set to 1 only when putd-output-file is specified.

You can also change this option in runtime from 'Prefs' menu. Changed value is not saved for future sessions. 

`-swank-ip ADDR` - assign an alternate IP address for swank server (default is 127.0.0.1).

`-swank-port NNNN` - assign alternate port for swank connection (default is 4009). Do not connect to the port use use for SLIME/EMACS interaction.

`-oduvan-backend 1` - supposes that oduvanchik runs on SWANK side and enables some lisp-specific editor features (implementation is under construction). You can also change it via Pres menu bar submenu.

Файл инициализации
--------------------

Windows: %HOME%\clcon.cfg

Linux: ~/.clconrc 

Чтобы отредактировать файл инициализации из самой среды, напишите в консоли

    .edit_initialization_file

Чтобы применить изменёнения файл конфигурации, выполните в редакторе с этим файлом команду меню `«Tcl/md»/Отправить текст в подчинённый интерпретатор`. Что применится, а что нет, зависит от конкретного случая - вам может понадобиться перезагрузить clcon, чтобы все изменения вступили в силу. 

### Установка шрифта
Впишите в файл инициализации: 
```
tkcon font "Courier" 10 bold
set ::Fonts [list {lucida 8} [tkcon font] {Courier 12 bold} ]
```
Здесь tkcon font устанавливает шрифт по умолчанию.
::Fonts - задаёт маленький, средний и большой шрифт для команды выбора шрифта, к-рая имеется во
многих окнах. 

Можно сменить шрифт в консоли:
```
.. tkcon font "Courier New" 10
```
или посмотреть разные шрифты:
```        
..source "$::tkcon::YarRoot/lp/clcon/examples/демонстрация-шрифтов.tcl"
#или
..source "$::tkcon::YarRoot/lp/clcon/examples/диалог-выбора-шрифтов.tcl"
```


### Как отличить латиницу от кириллицы?
Неизбежно написание текстов, в к-рых встречается и латиница, и кириллица. Как отличить? 

- в редакторе можно включить подчёркивание латиницы, Файл/Выделять латиницу
- под Windows шрифт oemfixed показывает кириллицу и латиницу чуть-чуть по-разному. Под Linux для этой цели можно попробовать Calibri, Roman (очень страшные)


### Действия при соединении со SWANK

Процедура ::AttachSwankHook вызывается, если она определена, после соединения с сервером swank.

```
proc ::AttachSwankHook {} {
   ::tkcon::SendEventToSwank "(named-readtables:in-readtable :buddens-readtable-a)" {}
}
```

### Избранное
В консоли есть меню "избранное". Чтобы заполнить его, добавьте в инициализационный файл данные для построения меню, например: 
```
set ::tkcon::OPT(Izbrannoe) {
  "1.Привет" {..puts "Hello"} 
  "2.Редактировать мой файл" {.edit "c:/my-file.lisp"}
}
```
Нечётные элементы - это заголовки (будет подчёркнута первая буква для ускоренного запуска - Alt-6 Подчёркнутая-буква ), чётные - это команды, как
они вводятся в консоли. 

Автодополнение, продолжение и автоподстановка
---------------------------
Completion of lisp symbols works in console, use `Ctrl-Space` to complete lisp symbol prefix (may contain package or part of package name).

Только в консоли, используйте `Ctrl-F3` для автодополнения имени файла (Unix-style, names containing space may not work). 

Только в консоли, используйте `Tab` для дополнения идентификатора tcl. Чтобы это работало при вводе команд tcl, которые вводятся, начиная с `..`, поставите пробел после `..`. Также рекомендуется всегда ставить префикс пр-ва имён, например, `::edt:: (и далее нажимаем Tab)`

Pressing Control-Return places unix-styled directory of currently selected editor buffer into the command prompt. If you press Control-Return again, file name will be added to directory to form complete file name. 

Команда "Find source" - найти определение
-------------------
Переход к определению работает и в редакторе, и в консоли. 

Для нахождения идентификатора лиспа - `Alt-.` . Если лисп знает только одно определение с таким именем, оно сразу открывается. Если несколько - то на консоли печатается несколько зелёных подчёркнутых "гиперссылок", по которым нужно щёлкнуть мышью (извините, клавиатуру для этого нельзя использовать). 

Поиск идентификатора для tcl - `Control-F9`. Нужен идентификатор с пр-вом имён, например, `::edt::cBi`. 

Искать в файлах, поиск в файлах, find in files
-------------
Искать в файлах можно только через командную строку. Это команды языка tcl, поэтому во избежание недоразумений используйте для вашей стркои поиска [закавычивание из tcl кавычками](c:/yar/doc/книги/tcl-tk/i_gu14.html#5), [или фигурными скобками](c:/yar/doc/книги/tcl-tk/i_gu14.html#6). Если ваш аргумент достаточно невинен, т.е. tcl воспринимает его "как есть", то кавычки и скобки не нужны. Например, если ваше слово состоит из букв, цифр, подчёркиваний, тире, звёздочек и плюсиков, то его не нужно закавычивать. 

## .иия
Сокращения для "искать в исходниках Яра". Принимает один аргумент - строку поиска, ищет без учёта регистра.

## .иф
`.иф [ -типы имена-типов ] root-dir search-string` или `.иия [ -типы имена-типов ] корневая-директория строка-поиска` ищет в файлах с данными расширенями, находящихся внутри дерева корневой директории заданную строку. По умолчанию расширения - это `{lisp asd}`. Пример:
`.иф -типы {tcl lisp} c:/yar/lp/clcon swank`

### presets for .finf
Good IDEs have "presets" for finding in some places. To imitate this,
you can create your own commands at initialization file, e.g.

```
proc ::clconcmd::finf_budden_tools {searchString} {
  ::clconcmd::finf c:/clcon/lp/budden-tools $searchString
}
```

## Функции лиспа для поиска

Есть функции лиспа для поиска. Например, так можно искать одну из двух строк в исходниках 

```
(clco::present-text-filtering-results
 (union
  (clco::filter-many-files (clco::clcon-sources) "wesppt")
  (clco::filter-many-files (clco::clcon-sources) "WrapEventScriptForFreezedText") :test 'equalp))
```

это довольно криво, т.к. множества при слиянии сортируются как попало. Некоторое утешение состоит в том, что 
в таблице результатов можно отсортировать по колонке "файл". Правильным подходом было бы сортировать результаты
в порядке следования исходных файлов в (clcon::clcon-sources), а внутри каждого файла сортировать по строке (дубликаты убираются с помощью union). 

Где вызывается функция?
-------------------------
Редактор/Меню/Лисп/кто вы&зывает функцию

Имейте в виду, что вызовы через apply символа с большой вероятностью не получится найти таким способом.


Подключение к/отключение от SWANK
--------------------------------------

Use two items on Console menubar item

Зелёные и подчёркнутые фрагменты текста - это гиперссылки
---------------------------------

Some of output is in green. Green regions are clickable (sorry, some of them are only clickable with mouse now). 

Переключение между окнами
-----------------------

Most of the tools are equipped with "Window" menu which allows to switch between tools. Current keyboard shortcuts:

- `Ctrl-F12` - switch to buffer list
- `Ctrl-.` - switch to console
- `Ctrl-Shift-E` - switch to editor
- `Ctrl-Shift-T` - переключиться в список нитей (тредов)

Команды среды разработки IDE (ИСР)
------------

Команды среды разработки вводятся в REPL и начинаются с одной точки (.) 

`.insp*` (или `.ин`) вызывает инспектор для просмотра лисповой [переменной "*"](http://filonenko-mikhail.github.io/cltl2-doc/ru/clmse108.html#dx133-176005), в которой обычно находится результат последнего вычисления REPL. 

`.tcsoh filename.tcl` loads tcl file from directory where clcon.tcl script is located into main IDE tcl interpeter. Also note we
have file/Reload some of IDE sources which reloads all sources excluding clcon.tcl, record_definition.tcl and named_args.tcl. 

`.hist` или `.ист` shows command history. It accepts an optional string argument. If supplies, it is treated as glob pattern to filter history event. Asteriks are added at the left and right sides of pattern prior to filtering.

`.NNN` where NNN is a decimal number re-runs command from history with that number

`.o` (Латинская) или `.о` (Русская)  - run oduvanchik command in visible editor buffer. Command must be written with dashes and without '-command' suffix, e.g. `.o indent-new-line`

`.fics string` или `.иия` ишёт строку в исходных текстах Яра, регистр букв не имеет значения

`.finf [ -types имена-типов ] root-dir search-string` или `.иия [ -типы имена-типов ] корневая-директория строка-поиска` ищет в файлах с данными расширенями, находящихся внутри дерева корневой директории заданную строку. Например, 
`.иф -типы {tcl lisp} c:/yar/lp/clcon swank`

`.apr string` или `.апр` вызывает [лисповый а пропос](http://www.lispworks.com/documentation/HyperSpec/Body/f_apropo.htm), т.е. поиск символов, в имени которых содержится подстрока (регистр не имеет значения)
Если нужно подобрать символ в конкретном пакете, пользуйтесь лиспом:
```
(apropos "Имя" :Пакет)
(cl-ppcre:regex-apropos "^Символ-.+" '(П-Я-ГРАМОТЕЙ) :case-insensitive nil)
```

`.tapr string` или `.тапр строка` вызывает а пропос для tcl (аналогично `.апр`)

`.help` lists available IDE commands (no real help, sorry :) )

It is recommended to use your initialization file to define new IDE commands. 

To view the source of the IDE command, type it as `::clconcmd::tapr` and press `Control-F9` on the text.

Вызов команд Tcl 
-----------
Place two dots and a space (`.. `) directly at the IDE prompt to pass arbitrary tcl command to tcl interpreter. Use Control-Alt-U to complete tcl names. E.g. this will display tk's message box. 

```
.. tk_messageBox -title "clcon" -message "Wow, it works!" -parent $::tkcon::PRIV(console)
```

Открытие файлов для редактирования
-------------------------
Use "File" menu, Control-o keyboard shortcut or
.edit IDE command: 
`.edit <filename>`
Press Ctrl-F3 to complete filename (at least it will work undex *nix).

Редактирование файлов
-------------
clcon понимает только кодировку utf-8 и юниксовый стиль завершения строк (знак с кодом 10). 
Но для правильной работы swank нужно указать в первой строке файла кодировку, например:

```
;; -*- coding: utf-8; system: my-system;  -*-
```

Если этого не сделать, поиск определения и отладчик будут "промахиваться" мимо исходного текста, а в отдельных случаях
отладчик вообще не найдёт исходный текст. 


`Ctrl-z` - undo

`Ctrl-y` - redo

`Ctrl-k` - delete text to the end of line

Сохранение файлов
-------------------------
WARNING! 

There is a bug in tab switching code so tab names at some point can mismatch real file name of the file being edited. Also note that as some part of IDE crash, editor might become crashed too. Be careful! 

If you have crashed swank connection, first of all try to disable "Oduvan-backend" flag at prefs menu in the console. After that, try Secret/Unfreeze command if your editor appears hanged up. With two that measures, you have good chances to save your work. But, again, don't rely upon IDE. 
Normally, to save files, use "File" menu or Control-s keyboard shortcut. 

Открытие файла сторонним редактором
-------------------------------------
Возможности текстового редактора clcon ограничены, поэтому есть пункт меню Файл/Открыть этот файл другим редактором. Он же пригодится, когда нужно видеть два файла рядом. 

Поддержка именованных таблиц чтения (named-readtables)
-------------------------
We have support for named readtables in files. Clcon only recognizes keywords as readtable-names. If the file contains a line

`(optional-package:in-readtable :readtable-name)`

lines below that line are considered to be in that readtable. You can have several in-readtable statements in the file though it is not recommended, every `in-readtable` statement acts up to the next `in-readtable` statement. Note that the code before first "in-readtable" statement is assumed to be in readtable nil, but when compiling a file, it will use current readtable indeed. 

If 

  i) there is no `in-readtable` statements above current line, or closest `in-readtable` statement specifies readtable named NIL, 

  ii) an in-package form is present above this line 

swank:*readtable-alist* variable is used to determine a readtable to use from the latest `in-package` form above the line. This is a caveat as we must have obeyed explicit `(in-readtable nil)`. Hope to fix it later. 

Note that is *readtable-alist* is modified, clcon does not note the change until it occasionally recalculates cached value of readtable for current line. 
To ensure that the change is noted, close and reopen the file. 

Also note that canonical :named-readtables implementation has a cludge that can modify *readtable-alist* very frequently and chaotically. This cludge is removed in forked version of named readtables library installed with clcon. 

Readtable names are upcased regardless of everything when in the editor. When file is being compiled, in-readtable statement parsing 
depends on actual current *readtable* around compilation. 

Opening files with "in-readtable" forms have side effect - uppercased readtable names are interned into keyword package. 

Компиляция
----------
Компиляция одного файла - F7 
Компиляция системы (clco:load-system-for-tcl система . опции)

Отладчик
--------
You fall into a debugger when something unusual happens, e.g. you divide at zero. You see the stack. 
Open stack frames and watch locals. Possible actions in a debugger:

###View a condition
Condition is similar to 'exception' or 'error' in other languages. You see printed representation 
of a condition in an upper part of the debugger window. Press at highlighted text and you can inspect
a condition with an inspector.

###View stack
Stack frames are expandable. As you expand frame, you see local variables and catch tags (catch tag is like "catch" or "on error" 
construct in other languages).

Note: in SBCL, local variable names sometimes are wiped away by compiler. See SBCL manual on how to enable variable names.

###Inspect stack contents
You can press \<Return\> or double click on a frame or local. For frame, debugger would try to find source of a function. 
For local variables, inspector would appear. 

###Invoke restarts 
Restart is a way to leave debugger and continue program execution. Usually there are 
several ways to leave a debugger, they are listed at `Restarts` menu. One of them is default
and marked with an asteric `*`. If you close debugger window with a cross or by pressing `Alt-F4`,
default restart will be invoked. 

###Eval in frame
Evals code in the context of the frame selected on the stack and prints result at the console. Can be invoked via debugger "stack" menu.

###Return from frame
Prompts for lisp expression and returns it from the frame as if it was normal return. Found at "Stack" debugger menu. 

###Пошаговая отладка, watch expressions
Stepping is described in [demo tour](demo-tour.md). No watch expressions are available (but there are some other tools, e.g. trace). 

###Breakpoints
Просто вставьте [break](http://www.lispworks.com/documentation/lw60/CLHS/Body/f_break.htm) в нужном месте, перекомпилируйте файл, в к-ром определена ф-я и упадёте в отладчик. Поскольку в Яре можно перекомпилировать файлы лиспа по-модульно, это (почти всегда) не составляет проблемы. Дальше можно перейти в режим пошаговой отладки через меню отладчика Стек/Перейти в режим ходьбы. Есть также функция `cl-user::b2`, к-рая сразу переходит в пошаговую отладку. Также есть trace с break, см. руководство по SBCL.  

###Tracing (debug messages)
If you want to print debug messages as your code runs, use [trace](http://www.lispworks.com/documentation/lw60/CLHS/Body/m_tracec.htm).
Specific CL implementations usually have extensions for trace, read your Lisp's manual!

Also you can just insert calls to [some of printing functions](http://www.lispworks.com/documentation/lw60/CLHS/Body/f_wr_pr.htm) 
into your function and recompile the file where it is contained. Lisp is so great so you need not restart your program. As you enter
your function next time, new definition applies.  

Ошибки Tcl 
----------
Many tcl errors printed in read are clickable with mouse. Error stack is shown by the click. Some procs in the stack are clickable.

Множественные консоли 
---------------------------------
All is simple - don't use them. They are broken. I plan remove them soon. 

Что делать, если мой REPL (ЦЧВП) повис?
------------------------
menu bar/Connection/Disconnect from swank
menu bar/Connection/Connect to swank
- this can help

as you disconnect, you get into tcl prompt. Don't issue any commands, otherwise your history would mess up
with tcl commands which you can later run as lisp commands. I plan to remove tcl console functionality at all,
as we have tcl escapes alredy.

Многострочные команды лиспа
------------------
To type in multiline command, separated lines with `Shift-Return` instead of just `Return`. 
Clcon currently have no smart parser which could determine end of the command automatically. 
Pressing `Return` sends current command to interpreter. If this is an unfinished Lisp command, 
error would show up with the debugger. 

In theory, you can alos paste multiline commands. But if text in the clipboard ends with 
newline character, pasting would invoke immediate evaluation. Combined with the fact that 

Также читайте руководство по tkcon
----------------------
clcon is fork of tkcon. Many of tkcon's functionality is destroyed, but some still work (and will work in the future).

tkcon's manual is [here](http://tkcon.sourceforge.net/docs/index.html)

It can also be found in ActiveTCL help file.
 
## What if I want to contribute
Read issues

Список нитей (тредов, потоков исполнения) и отладка потоков
------------------
Ctrl-Shift-T. 
Отладка потоков почти не работает (через пень-колоду). Более-менее помогает, если сразу запустить какой-то поток на отладку. Можно перейти из графического отладчика в консольный, если выполнить в кадре стека 

`(sldb-break-with-default-debugger t)`

но поскольку ввод-вывод всё равно через swank, то пользы мало. Есть технология отладка потоков в консоли с перенаправление ввода/вывода, см [отладка-без-clcon](отладка-без-clcon.md)

Подсветка синтаксиса
------------------
Поддерживается lisp и tcl. По умолчанию файлы красятся в лисп. Также у нас есть подсветка открывающей скобки для данной закрывающей в лиспе. 

Справка по лисповым символам
----------------------------
Just press F1 on a symbol in the console or in the editor. Some lisp 
libraries support online help too. To learn which ones do support, try
`.apr hyperdoc. You'll see `*hyperdoc-base-uri*` and/or `hyperspec-lookup`
symbols in some packages. For theese packages, there is a probability to
get help via pressing f1. 

Фильтрация кадров при отладке
-----------------------------
Если надо видеть не все кадры отладки, можно связать переменную clco:*filter-frames*. Она должна содержать функцию одного аргумента. Аргумент -- фрейм.
Результат -- истина, если надо фрейм показывать, иначе ложь.

Например, чтобы скрыть фреймы функции ew::mlt-do-jmp

```
(setf clco::*filter-frames* 
  (lambda (x) 
    (let* ((*break-on-signals* nil)(r t)) 
      (ignore-errors 
        (when (eq (sb-di::debug-fun-name (sb-di::frame-debug-fun x)) 
                  (intern "MLT-DO-JMP" 'ew)) 
          (setf r nil))) 
      r))
```

Запуск команд ОС
----------------
(uiop/run-program:run-program "dir" :output t :external-format :cp866)

Открыть URL
-----------
(clco::open-url "http:/ya.ru")

Загрузка систем asdf
--------------------

clcon+Яр настроен искать системы asdf во всех поддиректориях директории установки Яр. Если есть две одноимённые системы, то я надеюсь, что имеет приоритет та, у которой более короткий путь к файлу (не проверял, но это похоже на правду, судя по виду файла system-index.txt, к-рый составляет quicklisp). 

Если открыть файл asd, то команда редактора Файл/Сохранить, компилировать и загрузить этот файл  загрузит данную систему. 

Имя системы угадывается по имени файла. Загрузка системы происходит через функцию clco:load-system-for-tcl, которая собирает сообщения и поэтому может работать медленно. Если вас это не устраивает, загружайте систему из консоли, как обычно, asdf:load-system . 

Если asdf не видит систему, на это могут быть следующие причины:

* вы только что создали файл системы и ещё её не загружали. В этом случае нужно стереть все файлы system-index.txt в `*yar-root*` и поддиректориях.
* в файле asd имя системы задано с ошибкой.
* файл не находится внутри *yar-root*. Пример проверки: `(budden-tools:subdir-p "c:/yar/lp/budden-tools/budden-tools.asd" *yar-root*)` Для настройки asdf для этого случая см. руководство asdf или quicklisp.
* уж и не знаю. Можно проверить, видит ли asdf вашу систему: `(ql:where-is-system :my-system)`

Также может оказаться, что вы хотите компилировать систему, вызвав команду "Сохранить, компилировать и загрузить этот файл" в файле *.asd, но Яр видит систему с таким именем где-то в другом файле. В этом случае Яр скажет вам об этом и не станет ничего делать. Т.е., нужно понимать, что не существует команды "загрузить систему из этого файла". Существует только команда "загрузить систему с таким именем", а asdf сам решает, где взять эту систему. Мы всего лишь для удобства передаём asdf имя системы, равное имени файла, и защищаем вас от недоразумений, если asdf находит эту систему на там, где вы хотите. 

К сожалению, asdf - программа довольно низкого качества, и данные простые проверки не спасут вас от проблем. Например, легко можно создать ситуацию, когда есть два файла, видимых asdf и quicklisp. В одном реально определена asd-система x, а другой называется x.asd, но в нём никакой системы нет. При определённых условиях получится, что система определена сразу в двух местах, потому что в разных полях объекта asdf/defsystem:system будут указаны разные пути к файлу). Будьте предельно аккуратны во всём, что касается asdf и да хранит вас Бог!

поиск зависимых систем asdf
-----------------------
```
(asdf::systems-that-depend-on-system "транслятор-яра-в-лисп")
```

Файлы markdown, работа-с-документами-md, работа-с-документами-markdown
---------------
При редактировании файлов markdown можно переходить к определениям лиспа и tcl с помощью обычных сочетаний клавиш. Нажатие F7 генерирует файл html и открывает его в вашем браузере. 

