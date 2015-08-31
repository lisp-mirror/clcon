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
Debugger is in early stage of development. Do not close debugger window - always invoke a restart. 

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
