Clcon 0.3.9 - Демонстрационный сеанс
===============

Clcon находится в состоянии "альфа" и не всё в нём хорошо работает. То, что работает хорошо, можно увидеть в данном сеансе. Также есть [видео](https://www.youtube.com/watch?v=nMhwvZ56jHU) по мотивам демонстрационного сеанса. 


Вычисления в консоли (REPL)
-------------
Для начала попробуйте что-нибудь вычислить в консоли:

     (print "Привет, мир!")

Приветствие напечатается дважды, как и должно быть в лиспе. Для ввода многострочных команд используйте сочетание Shift-Return ( Shift-Enter ) (то есть,  одновременное нажатие Shift и Return (Enter)).

Apropos, completion, find definition, help
-------------
Keyboard accelerators are shown in "Edit" menu.

### Lisp
В консоли, напечатайте `.апр -lookup-`, чтобы увидеть все символы, в имени к-рых содержится подстрока "-lookup-" . 
Далее, в консоли напечатайте `clco:s-l-d` и нажмите `Ctrl-Пробел`. Имя расширится в `clco:server-lookup-definition` с помощью алгоритма 'составного автодополнения', (swank-c-p-c contrib). Теперь нажмите `Alt-.` Откроется редактор и в нём - исходный текст ф-ии, к-рую мы напечатали. Также Alt-. работает внутри редактора. Если определений больше одного (напр., для функции PRINT), в консоли напечатается несколько зелёных гиперссылок, каждая из которых по клику левой кнопкой мыши ведёт в одно из мест, относящихся к определению функции.

Наконец, установите курсор на слово `defun` и нажмите `f1`. Откроется окно со справкой по функции (в редакторе окно будет намного более подробным, чем в консоли). 

### Яр
Вернитесь в консоль с помощью `Control-.`.
В консоли, если команда начинается с пробела, то она передаётся интерпретатору Яра (на самом деле это транслятор, но не суть). Напечатайте, используя `Shift-Enter` для перехода на следующую строку:

```
> опр функ Плюс-два (ю -- целое)
тело
  ю + 2
кно
> Плюс-два(1)
; напечатается 3
```

### Tcl
Если команда начинается с одной точки, то это команда clcon, которую выполняет интерпретатор tcl. 
В консоли, очистите введённую строчку с помощью `Control-u` и напечатайте `.тапр cap` (cap - латиницей), затем Enter, чтобы перечислить все процедуры, команды и переменные, имя к-рых содержит `cap`. Поставьте курсор на любую из процедур и нажмите `Control-F9`, чтобы попытаться перейти к определению.

Если команда начинается с двух точек, то две точки отрезаются, а остаток передаётся интерпретатору tcl. 

Return to the console with `Control-.` . Type a space and then `snit::Ca`. Then press `Tab` . Name will expand to 
`::snit::Capitalize`. Press `Control-F9` and jump to a source of tcl proc. Note this is source from the library, not the source of clcon. Rather convenient. We only now support procs, not variables. Note that "find source" for tcl works for fully qualified names only. It only shows proper source if name starts from "::" (this is a bug).


### File name
Return to the console with `Control-.`. Type a space and then some partial file name in Unix style, e.g. `c:/win` under Windows or `/bi` under Unix
and then press `Control-F3`. Name will expand to `c:/windows` or `/bin`. 

Pressing `Control-Return` at the console inserts path to the file currently selected in the editor. Pressing `Control-Return` again adds file name. 

Switching between windows
-----------
In the editor, press `Control-.` to switch back to console. In the console, press `Control-Shift-h` to switch to editor. Press `Control-Tab`, `Control-Shift-Tab` to navigate through editor buffers. Press `Control-F12` to see buffer list widget. In the widget, buffers are sorted by MRU.
	
Compiling file from editor
------------
At the console, open test/error-browser-sample-file.lisp file for editing.
There are at least two ways to do that: via file open dialog (Control-O)
or via console ed command:
``.edit ~/yar/lp/clcon/test/error-browser-sample-file.lisp`` or 
``.edit $::tkcon::YarRoot/lp/clcon/test/error-browser-sample-file.lisp``
for Windows file release. If Linux, while typing in filename, use Control-F3 for completion.

File will open in the editor. From the menu, choose `Lisp/Compile and load`. Two new windows will pop up: list of compiler notes 
and error details. Also there will be some text above list of compiler notes. It will state that compilation is failed, 
but you can try load generated fasl file if you press "!".

You can browse through notes with arrow keys. As you press <space>, source code for error will be shown in the editor. 
Once you switched to editor, it is convenient to scroll through messages with Alt-F7/F8. 

Компиляция системы с удобной навигацией по предупреждениям описана в руководстве пользователя. 

Отладчик и степпер
------
Напечатайте в консоли:

``(load (compile-file (merge-pathnames "test/dbgtest.lisp" clcon-server:*clcon-source-directory*)))``

Возникнут две функции, f и g, f вызывает g, а в код g вставлено `(break)`. 
Вызовите в консоли `(f 5)`. Откроется отладчик. Вы можете листать стек. 
Если нажать стрелку вправо на клавиатуре, покажутся локальные переменные данного кдара стека. Нажатие клавиши `Ввод` на кадре стека ведёт к его исходнику, а нажатие на локальной переменной открывает инспектор для просмотра этой переменной. 

Также можно нажать `ctrl-f` для поиска в стеке. Для продолжения поиска, нажмите `f3`, не закрывая окна поиска.

Можно вычислять выражения в контексте выбранного кадра стека. Например, вы можете выделить стрелками верхний кадр стека (для функции `G`) и выбрать в меню `Стек/Выполнить в кадре и лепо вывести` (горячие клавиши - `alt-2 5`). 

Появится окошко с заголовком "выполнить в кадре" - там можно ввести выражение. Текущий пакет указан в заголовке кадра. Введите выражение, нажмите `Ввод`. Если вы не ошиблись, покажется консоль и в ней - результат вашего вычисления. `Control-Shift-d` вернёт в отладчик.

Также есть меню команд перезапуска (рестаров, restarts). Рестарты - это способы продолжения выполнения программы, доступные в данном вызове отладчика. 
Если просто закрыть окно отладчика, будет вызвана команда перезапуска по умолчанию, отмеченная звёздочкой в меню команд перезапуска. 

### Теперь попробуем пошаговый отладчик

Закройте отладчик, верните предыдущую `(f 5)` с помощью `ctrl-стрелка вверх` в консоли и запустите её снова. Когда появится отладчик, нажмите f11 (Ходьба/Зайди). Появится исходник функции g в некоторой точке. Теперь в меню "ходьба" есть пункты "Беги", "Выбегай", "Переступи" и "Зайди". Они аналогичны командам "Continue", "Step out", "Step over" и "Step in". Их поведение напоминает любой пошаговый отладчик, но есть две проблемы:

- шаги делаются иногда слишком редко
- нельзя "выбежать" из функции, если ранее мы не заходили в неё с помощью команды "зайди". в этом случае отладчик обидится и предложит вместо этого продолжить ходьбу с помощью "Переступи". Это неприятно, но так сделан SBCL. Горячие клавиши для вызова команд ходьбы см. в меню "Ходьба". Эти команды работают также и в редакторе. 
- окно отладчика не запоминает свою позицию - если вы его подвинете, оно при следующем шаге снова возникнет на старом месте. Используйте пункт меню окно/save window position и далее на предложение ввести 1,5 или 9 нажмите 1. 

IDE commands. 
---------------------
We like menus, but console-based ideology is nice too. So we have some kind of IDE command language. IDE commands start with `.`
Type `.help` to see list of those. 

Standalone inspector
----------
Eval something, say `'defun`, at the console. And then type in `.ин` (this is IDE command) to inspect `*`, that is, result of last REPL evaluation. You can see the symbol's properties and follow hyperlinks. 

Invoking tcl
--------
When you enter line starting from `.. `, at the IDE prompt, the line will be treated as tcl command. E.g. type in ``.. tk_messageBox -message "Ура!"`` to try. There is also a way to invoke tcl from lisp. Type in `(clcon-server:eval-in-tcl "tk_messageBox -message WOW")` at the console and you'll see message box, which was invoked from the lisp side. 

Editing files
---------
All files are now highlighted according to lisp mode, and you can even edit them! Beware that editor is extremely complex inside and it can still contain uncovered bugs - be sure to save your files frequently. 

When the file is modified, its tab is marked with asterik. Also asterik is shown in the buffer list (invoked by `Control-F12`).  

Interesting commands while editing lisp are "indent" (`Tab` key) and "indent new line"  (`Shift-Return`). Also you can note highlight of opening paren when you stay at closing one. Also we have "Lisp" menu with several lisp mode commands (not currently bound to keyboard, but should work). 

Also we have rather lame tcl indentation. It is invoked with `Control-Return`. Tcl find source in the editor ignores current namespace, so it is of limited use. 

Find in files
--------
Type at the console: 
`.иия buf1`
(Here "иия" is an abbreviation for искать-в-исходниках-Яра, хотя функция называется "find-in-clcon-sources")
And you'll see some kind of "grep browser". Press <space> or <return> to jump to a source. Difference is that <return> closes grep browser while <space> does not. It can only fine simple strings for now with case ignored. Note this command is processed by tcl interpreter, so use tcl's quoting rules. The most simple way to get more or less predictible results when some funny characters are present is to wrap your search string into `{}`. 

More searching examples are in the [user manual](user-manual.md).

List declarations in current file
---------
Press `f12` in the editor buffer, or choose "Show current file declarations" from the Edit menu of Editor. 
Again, press `Space` or `Return` to go to source. `Return` closes declaration list. 

Code to extract declarations says tcl from lisp, but is extremely simple - no warranty. Also note that declarations are extracted from
 the file, not from the buffer. So if file is open in the editor and modified, locations will be inexact. 
Also widget's design is not that perfect. At least I use it sometimes and found it useful. 

Дополнительные команды навигации: пакет, система, файл под курсором
----------------------------------------
Наберите в консоли `clco:s-l-d` и нажмите Ctrl-Пробел. Символ расширится в 
`clco:server-lookup-definition`. Перейдите к определению с помощью `Alt-.`. Теперь из меню "лисп" редактора выберите "перейти к определению пакета" - попадете в определение пакета, 
указанного в форме in-package этого файла. вернитесь назад (`Alt-,`). Из меню "лисп" редактора выберите "система/перейти к определению системы" - попадете в определение asd системы. Название системы берётся из первой строки файла, которая может иметь следующий вид:
`;; -*- system :clcon-server ; -*-` . Среда не проверяет, что этот файл действительно относится к этой системе. В определении системы встаньте на любое имя файла, например, на букву t в слове `"utils"`, и выберите в меню редактора Файл/Редактировать файл, имя которого под курсором (`Alt-1 3`). Откроется файл utils.lisp (при нахождении внутри файла asd среда подразумевает расширение .lisp). 

