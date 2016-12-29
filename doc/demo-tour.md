Clcon 0.3.8 - Демонстрационный сеанс
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
At the console, enter `.apr -lookup-` to list all symbols containing "-lookup-" substring in their name. 
At the console, type in `clco:s-l-d` and press `Ctrl-Space`. Name will expand to clco:server-lookup-definition with the help of compound completion algorithm (swank-c-p-c contrib). Now press `Alt-.` Editor window will pop up and source of function you typed will be seen. You can press Alt-. at
any lisp definition in the editor and jump to its source. If there are more then one definition (e.g. for PRINT function), list of "hyperlinks"
will show up at the console. Click on either of them with mouse to open an appropriate source. In the editor, completion works in similar way. 

Finally set the cursor at the word `defun` and press `f1`. Web browser should open with hyperspec article about `defun`.  

### Tcl
Return to the console with `Control-.`.
Clean up current command with `Control-u` and enter `.tapr cap` to list all tcl procs, commands and vars containing substring `cap` in their name. Position a keyboard cursor at any of them and press Control-F9 to go to its source. 
Return to the console with `Control-.` . Type a space and then `snit::Ca`. Then press `Control-Alt-u` . Name will expand to 
`::snit::Capitalize`. Press `Control-F9` and jump to a source of tcl proc. Note this is source from the library, not the source of clcon. Rather convenient. We only now support procs, not variables. Note that "find source" for tcl works for fully qualified names only. It only shows proper source if name starts from "::" (this is a bug).

### File name
Return to the console with `Control-.`. Type a space and then some partial file name in Unix style, e.g. `c:/win` under Windows or `/bi` under Unix
and then press `Control-F3`. Name will expand to `c:/windows` or `/bin`. 

Pressing `Control-Return` at the console inserts path to the file currently selected in the editor. Pressing `Control-Return` again adds file name. 

Switching between windows
-----------
In the editor, press `Control-.` to switch back to console. In the console, press `Control-Shift-e` to switch to editor. Press `Control-Tab`, `Control-Shift-Tab` to navigate through editor buffers. Press `Control-F12` to see buffer list widget. In the widget, buffers are sorted by MRU.
	
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

Warning! Due to random positioning of windows, some views may be unpleasant to work with, so you might want to 
resize/reposition them manually. Your feedback and patches to solve this are welcome. 

Компиляция системы с удобной навигацией по предупреждениям описана в руководстве пользователя. 

Debugger and stepper
------
At the console, type in the following:

``(load (compile-file (merge-pathnames "test/dbgtest.lisp" clcon-server:*clcon-source-directory*)))``

This would create two functions, `f` and `g`. Run `(f 5)` at the console. Debugger will pop up. This is real sldb, SWANK-based debugger with most of its features implemented. You can browse through the stack. As you press `Right Arrow` at any of the frames, you will see locals. Pressing `Return` on the frame leads you to frame's source, while pressing `Return` on the local opens an inspector. Yes, native SWANK-based inspector.

There is also search in the stack list. Tree without a search is a dense forest! Type `Ctrl-F`, type in `)` and press `F3` or `Return` to continue search.

You can evaluate values in the context of stack frame. Select topmost stack frame (of function `G`) in the frame list and choose `Stack/Eval in frame` from debugger menu bar. New window titled "eval in frame"
will pop up. Note package is prompted at window's title. Type `y` in the window and press `Return`. Console will be activated and result of your evaluation 
will be printed there. Press `Control-Shift-d` to return to the debugger.

Also we have "Restarts" menu at menubar. We could call either of them.
Or we could just close debugger window with cross or with closing command of your Window Manager (e.g. Alt-F4),
to call default restart, marked by asterik in restarts menu. Let's invoke "continue" restart and watch result of `f` (20) at the console. 

Now let's try stepper. 

Warning! There are problems in stepper backend when you step out of the frame where you turned stepping mode on. Press "continue" if stepper shows up unexpectedly. Submit bug to SBCL tracker :) 

With `Control-Up` at the console, bring up last command `(f 5)` to the prompt, and press `Return` to call it again. As debugger occurs, position windows so that
you can see debugger and the editor at the same time. Choose `Stack/Switch to stepping mode` from menu bar. Editor window will pop up and current source will be highlighted. Press "F10" (Step next, or "Step over") watch how execution proceeds. Also note that stack and locals are shown in the debugger window. Press "f10" one more time, watch how execution proceeds. Then press "F5" (Continue, or resume to normal execution) to quit stepper mode. 

IDE commands. 
---------------------
We like menus, but console-based ideology is nice too. So we have some kind of IDE command language. IDE commands start with `.`
Type `.help` to see list of those. 

Standalone inspector
----------
Eval something, say `'defun`, at the console. And then type in `.insp*` (this is IDE command) to inspect `*`, that is, result of last REPL evaluation. You can see the symbol's properties and follow hyperlinks. 

Invoking tcl
--------
When you enter line starting from `.. `, at the IDE prompt, the line will be treated as tcl command. E.g. type in ``.. tk_messageBox -message "Wow!"`` to try. There is also a way to invoke tcl from lisp. Type in `(clcon-server:eval-in-tcl "tk_messageBox -message WOW")` at the console and you'll see message box, which was invoked from the lisp side. 

Editing files
---------
All files are now highlighted according to lisp mode, and you can even edit them! Beware that editor is extremely complex inside and it can still contain uncovered bugs - be sure to save your files frequently. 

When the file is modified, its tab is marked with asterik. Also asterik is shown in the buffer list (invoked by `Control-F12`).  

Interesting commands while editing lisp are "indent" (`Ctrl-Space` key) and "indent new line"  (`Shift-Return`). Also you can note highlight of opening paren when you stay at closing one. Also we have "Lisp" menu with several lisp mode commands (not currently bound to keyboard, but should work). 

Also we have rather lame tcl indentation. It is invoked with `Control-Return`. Tcl find source in the editor ignores current namespace, so it is of limited use. 

Find in files
--------
Type at the console: 
`.fics buf1`
(Here "fics" is an abbreviation for "find-in-clcon-sources")
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
указанного в форме in-package этого файла. вернитесь назад (`Alt-,`). Из меню "лисп" редактора выберите "перейти к определению системы" - попадете в определение asd системы. Название системы берётся из первой строки файла, которая может иметь следующий вид:
`;; -*- system :clcon-server ; -*-` . Среда не проверяет, что этот файл действительно относится к этой системе. В определении системы встаньте на любое имя файла, например, на букву t в слове `"utils"`, и выберите в меню редактора Файл/Редактировать файл, имя которого под курсором (`Alt-1 3`). Откроется файл utils.lisp (при нахождении внутри файла asd среда подразумевает расширение .lisp). 

