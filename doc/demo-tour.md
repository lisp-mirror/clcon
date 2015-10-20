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
In the editor, press `Control-.` to switch back to console. In the console, press `Control-Shift-e` to switch to editor. Press `Control-Tab`, `Control-Shift-Tab` to navigate through editor buffers. Press `Control-F12` to see buffer list widget.

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
resize/reposition them manually. Your feedback and patches to solve this are welcome. 

Debugger and stepper
------
At the console, type in the following:

``(load (compile-file (merge-pathnames "test/dbgtest.lisp" clcon-server:*clcon-source-directory*)))``

This would create two functions, `f` and `g`. Run `(f 5)` at the console. Debugger will pop up. This is real sldb, SWANK-based debugger with most of its features implemented. You can browse through the stack. As you press `Right Arrow` at any of the frames, you will see locals. Pressing `Return` on the frame leads you to frame's source, while pressing `Return` on the local opens an inspector. Yes, native SWANK-based inspector.

There is also search in the stack list. Tree without a search is a dense forest! Type `Ctrl-F`, type in `)` and press `F3` or `Return` to continue search.

You can evaluate values in the context of stack frame. Select topmost stack frame (of function `G`) in the frame list and choose `Stack/Eval in frame` from debugger menu bar. New window titled "eval in frame"
will pop up. Note package is prompted at window's title. Type `y` in the window and press `Return`. Console will be activated and result of your evaluation 
will be printed there. 

Also we have "Restarts" menu at menubar. We could call either of them.
Or we could just close debugger window with cross or with closing command of your Window Manager (e.g. Alt-F4),
to call default restart, marked by asterik in restarts menu. Let's invoke "continue" restart and watch result of `f` (19) at the console. 

Now let's try stepper. 

Warning! To make stepper work correctly, you need to load swank with stepper support disabled. This is true with windows release, but it depends on your initialization file on Linux. Hope to document it later, or see windows file release as an example. Also there are problems in stepper backend when you step out of the frame where you turned stepping mode on. Submit bug to SBCL tracker :) 

Bring up last command with `Control-Up` at the console, and press `Return` to call it again. As debugger occurs, choose `Stack/Switch to stepping mode` from menu bar. Editor window will pop up and current source will be highlighted. Press "F10" (Step next, or "Step over") watch how execution proceeds. Also note that stack and locals are shown in the debugger window. Press "f10" one more time, watch how execution proceeds. Then press "F5" (Continue, or resume to normal execution) to quit stepper mode. 

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

Editing files
---------
You can see lisp files are highlighted according to lisp mode, and you can even try to edit them. But editing is permanently broken. Also IDE won't ask you when you are closing modified file. This is done intentially: IDE crashed frequently, so if you dare to edit files in it, you must be always careful and control state of your files. When file is modified, its tab is marked with asterik. Also asterik is shown in the buffer list (invoked by `Control-F12`). 

Find in files
--------
This is very new tool, and it is a bit lame, but I have find it useful already.
Type at the console: 
`(clco::find-in-clcon-sources "buf1")`
And you'll see some kind of "grep browser". It can only fine simple strings for now with case ignored. There is no way to call it from the menu. 
If you want other kinds of search, you are welcome to Common Lisp programming - just jump to `clco::find-in-clcon-sources` and watch how it is done. 
