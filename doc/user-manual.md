Clcon user manual
===========

Command line options
--------------------
Most of [tkcon's command line options](http://tkcon.sourceforge.net/docs/start.html), are kept intact.
New options: 

```-putd-enabled 1``` - enable printing lots of debug messages. ```-putd-enabled 0``` - to disable (default).

You can also change this option in runtime from 'Prefs' menu. Changed value is not saved for future sessions. 

```-swank-ip ADDR``` - assign an alternate IP address for swank server (default is 127.0.0.1).

```-swank-port NNNN``` - assign alternate port for swank connection (default is 4009). Do not connect to the port use use for SLIME/EMACS interaction. 

Completion
----------
Completion of lisp symbols works in console, use **Tab** to complete lisp symbol prefix (may contain package or part of package name).
Use **Ctrl-F3** to complete filename (Unix-style, names containing space may not work). 
Tcl completion is currently broken. 

Find source command
-------------------
Currently finding a source can't properly extract symbol. So to see how it work just type
defun 
or, say, 
print-object 
on the fresh lisp prompt and press Alt-. 
If there is a single source, you just go to source. 
If there are many, they are printed at console and you can click on either on them with mouse (no way to do that
with keyboard now, sorry for that, this is a bug)

Connecting/disconnecting to/from SWANK
--------------------------------------
Use two items on Console menubar item

Green text regions are hyperlinks
---------------------------------
Some of output is in green. Green regions are clickable (sorry, some of them are only clickable with mouse now). 

Switching between tools
-----------------------
Most of the tools are equipped with "Window" menu which allows to switch between tools. Current keyboard shortcuts:

- `Ctrl-F12` - switch to buffer list
- `Ctrl-.` - switch to console
- `Ctrl-Shift-E` - switch to editor

IDE Commands
------------
Place dot (.) in the first position of the command to invoke named IDE command. Currently there are only a few commands:

```.insp*``` call inspector to inspect ```*``` (result of previous REPL evaluation)

```.tcsoh filename.tcl``` loads tcl file from directory where clcon.tcl script is located into main IDE tcl interpeter 

```.bufferlist``` shows editor's buffer list. Or just press `Ctrl-F12` (in most of the windows)

```.history``` shows command history

```.NNN``` where NNN is a decimal number re-runs command from history with that number

```.help``` lists available IDE commands (no real help, sorry :) )

Tcl escapes
-----------
Place two dots (..) to pass arbitrary tcl command to tcl interpreter. E.g. this will display a message box.

```
..tk_messageBox -title "clcon" -message "Wow, it works!" -parent $::tkcon::PRIV(console)
```

Debugger
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

###TODO: Eval in frame
Immediate or "eval in frame" is usually present in debuggers. Just wait for the next release or send a patch :) 

###Stepping, watches
SBCL supports stepping, but it is not done yet and this can be hard to do. Watches are useless without stepping.

###Breakpoints
Just insert [break](http://www.lispworks.com/documentation/lw60/CLHS/Body/f_break.htm) at appropriate place to enter debugger.
Lisp allows you to recompile separate functions without restarting a program, so this is not too inconvinient.

###Tracing (debug messages)
If you want to print debug messages as your code runs, use [trace](http://www.lispworks.com/documentation/lw60/CLHS/Body/m_tracec.htm).

Also you can just insert calls to [some of printing functions](http://www.lispworks.com/documentation/lw60/CLHS/Body/f_wr_pr.htm) 
into your function and recompile it. 

Tcl errors
----------
Many tcl errors printed in read are clickable with mouse. Error stack is shown by the click. Some procs in the stack are clickable.

Multiple consoles, multiple tabs.
---------------------------------
All is simple - don't use them. They are broken. I plan remove them soon. 

What if my REPL hang up?
------------------------
menu ber/Connection/Disconnect from swank
menu bar/Connection/Connect to swank
- this can help

as you disconnect, you get into tcl prompt. Don't issue any commands, otherwise your history would mess up
with tcl commands which you can later run as lisp commands. I plan to remove tcl console functionality at all,
as we have tcl escapes alredy.

Multiline commands
------------------
Not supported, neither in lisp, nor in tcl. Don't try them

Also Read tkcon manual
----------------------
clcon is fork of tkcon. Many of tkcon's functionality is destroyed, but some still work (and will work in the future).

tkcon's manual is [here](http://tkcon.sourceforge.net/docs/index.html)

It can also be found in ActiveTCL help file.
 
## What if I want to contribute
Read issues
