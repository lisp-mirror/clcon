Clcon demo tour
===============

Clcon is a building site. Excavators buzz. Pits, traps, naked electric wires wait you there.
But we'll try to lead you safe way through all interesting features of clcon.

Console evaluation
-------------
First of all try evaluate something:

     (dotimes (i 10) (print i) (sleep 0.5))

Note! You can not yet enter multi-line commands. As you press returns, input is sent to lisp REPL. So don't try to break your command into pretty several lines.

After you pressed return, you see that printing occurs with intervals. This experiment shows that we have async I/O.

Completion and find definition
-------------
Completion and find definition works best in the console. Keyboard accelerators are shown in "Edit" menu.

### Lisp
At the console, type in `clco:serv` and press `Tab`. Name will expand to `clco:server-lookup-definition`. 
Now press `Alt-.` Editor window will pop up and source of function you typed will be seen.

### Tcl
Return to the console with `Control-.` . Type a space and then `snit::Ca`. Then press `Control-Alt-u` . Name will expand to 
`::snit::Capitalize`. Press `Control-F9` and jump to a source of tcl proc. We only now support procs, not variables. 

### File name
Return to the console with `Control-.`. Type a space and then some partial file name in Unix style, e.g. `c:/win` under Windows or `/bi` under Unix
and then press `Control-F3`. Name will expand to `c:/windows` or `/bin`. 

Switching between windows
-----------
In the editor, press `Control-.` to switch back to console. In the console, press `Control-Shift-e` to switch to editor.

Compiling file from editor
------------
At the console, open test/error-browser-sample-file.lisp file for editing.
There are at least two ways to do that: via file open dialog (Control-O)
or via console ed command:
``.ed /s2/clcon/test/error-browser-sample-file.lisp`` or 
``.ed c:/clcon/quicklisp/local-projects/clcon/test/error-browser-sample-file.lisp``
for Windows file release. While typing in filename, use Control-F3 for completion.

File will open in the editor. From the menu, choose `Lisp/Compile and load`. Two new windows will pop up: list of compiler notes 
and error details. Also there will be some text above list of compiler notes. It will state that compilation is failed, 
but you can try load generated fasl file if you press "!".

You can browse through notes with arrow keys. As you press <space>, source code for error will be shown in the editor. 
Once you switched to editor, it is convenient to scroll through messages with Alt-F7/F8. 

Warning! Due to random positioning of windows, some views may be unpleasant to work with, so you might want to 
resize/reposition them manually. Your feedback and patches to solve this are welcome, see the debugger for a good sample.

Debugger and stepper
------
At the console, type in the following:

``(load (compile-file (merge-pathnames "test/dbgtest.lisp" clcon-server:*clcon-source-directory*)))``

This would create two functions, `f` and `g`. Run `(f 5)` at the console. Debugger will pop up. This is real sldb, SWANK-based debugger with most of its features implemented. You can browse through the stack. As you press `Right Arrow` at any of the frames, you will see locals. Pressing `Return` on the frame leads you to frame's source, while pressing `Return` on the local opens an inspector. Yes, native SWANK-based inspector.

There is also search in the stack list. Tree without a search is a dense forest! Type `Ctrl-F`, type in `)` and press `F3` or `Return` to continue search.

You can evaluate values in the context of stack frame. Choose `Stack/Eval in frame` from debugger menu bar. New window titled "eval in frame"
will pop up. Note package is prompted at window's title. Type `y` in the window and press `Return`. Console will be activated and result of your evaluation
will be printed there. 

Also we have "Restarts" menu at menubar. We could call either of them.
Or we could just close debugger window with cross or with closing command of your Window Manager (e.g. Alt-F4),
to call default restart, marked by asterik in restarts menu. Let's invoke "continue" restart and watch result of `f` (19) at the console. 

Now let's try stepper. 

Warning! To make stepper work correctly, you need to load swank with stepper support disabled. This is true with windows release, but it depends on your initialization file on Linux. Hope to document it later, or see windows file release as an example.

Bring up last command with `Control-Up` at the console, and press `Return` to call it again. As debugger occurs, choose `Stack/Switch to stepping mode` from menu bar. Editor window will pop up and current source will be highlighted. Press "F10" (Step next, or "Step over") watch how execution proceeds. Also note that stack and locals are shown in the debugger window. Press "f10" two more times, and then press "F5" (Continue, or resume to normal execution) to quit stepper mode. 

IDE commands. 
---------------------
We have some kind of IDE command language. IDE commands start with `.`
Type `.help` to see list of those.

One of useful commands is `.history`. It shows history of last commands you entered. You can invoke several last commands via history menu. 

Standalone inspector
----------
Eval something, say `'defun`, at the console. And then type in `.insp*` (this is IDE command) to inspect `*`, that is, result of last REPL evaluation. You can see the symbol's properties and follow hyperlinks. 

Invoking tcl
--------
Commands starting from `..` are interpreted by tcl. E.g. type in ``.. tk_messageBox -message "Wow!"`` to try. There is also a way to invoke tcl from lisp. Type in `(clcon-server:eval-in-tcl "tk_messageBox -message WOW")` at the console and you'll see message box, which was invoked from the lisp side. 
