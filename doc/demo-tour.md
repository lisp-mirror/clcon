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

Symbol completion
-------------
At the console, type in `clco:serv` and press `Tab`. If you are lucky, name will expand to `clco:server-lookup-definition`. 

Find definition
--------------
Now press `Alt-.` Editor window will pop up and source of function you typed will be seen.

Switching between windows
-----------
Some tools contain `Window` menu. Watch the menu to see how to switch to other tools. E.g. press `Control-.` to switch back to console.

Compilation conditions
------------
At the console, type in:
     ```(clco::compile-file-for-tcl (merge-pathnames "test/error-browser-sample-file.lisp" clcon-server:*clcon-source-directory*) nil)```

This function is a wireframe for a future "compile file" editor command. Compilation notes will be printed at the console. Additionally, compilation condition browser will pop up. As you scroll through notes in the list, error details occur at another window. Click on `[Go to source]` label at error details window and relevant source will open at the editor.

Switching between editor buffers
------------
We plan to make tabbed notebook for switching between windows. But for pro users, keyboard-based buffer switcher is desirable. We have one already. Press `Ctrl-F12` to view buffer list. At any of the buffer items, press `Return` or `Spacebar` to watch chosen buffer. Normally, buffer switcher must have incremental search or incremental filter facility, but it is still missing in our prototype. Surely, it will be there soon.

Debugger
------
At the console, type in the following:

     (load (compile-file (merge-pathnames "test/dbgtest.lisp" clcon-server:*clcon-source-directory*)))

This would create two functions, `f` and `g`. Run `(f 5)` at the console. Debugger will pop up. This is real sldb, SWANK-based debugger with most of its features implemented. You can browse through the stack. As you press `Right Arrow` at any of the frames, you will see locals. Pressing `Return` on the frame leads you to frame's source, while pressing `Return` on the local opens an inspector. Yes, native SWANK-based inspector.

There is also search in the stack list. Tree without a search is a dense forest! Type `Ctrl-F`, type in `)` and press `F3` to continue search (pressing `Return` will lead you to a known bug).

Also we have "Restarts" menu at menubar. If jou just close debugger window with cross or with closing command of your Window Manager (e.g. Alt-F4), default restart will be invoked.


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
Commands starting from `..` are interpreted by tcl. E.g. type in ``..puts "Wow!"`` to see string printed at the console. There is also a way to invoke tcl from lisp, but it has security hazard and can be disabled in the future. Type in `(clcon-server:eval-in-tcl "tk_messageBox -message WOW")` at the console and you'll see message box, which was invoked from the lisp side. For now, `eval-in-tcl` is used for quick-and-dirty implementation of some clcon features.


