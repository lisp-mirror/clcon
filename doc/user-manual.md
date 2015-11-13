Clcon <<TRUNK>> user manual
===========

Command line options
--------------------
Most of [tkcon's command line options](http://tkcon.sourceforge.net/docs/start.html), are kept intact.
New options: 

```-putd-output-file <filename>``` - file to print debug messages to. We can not print them to console correctly, as this calls update inside. 

```-putd-enabled 1``` - enable printing lots of debug messages. ```-putd-enabled 0``` - to disable (default). putd-enabled can be set to 1 only when putd-output-file is specified.

You can also change this option in runtime from 'Prefs' menu. Changed value is not saved for future sessions. 

```-swank-ip ADDR``` - assign an alternate IP address for swank server (default is 127.0.0.1).

```-swank-port NNNN``` - assign alternate port for swank connection (default is 4009). Do not connect to the port use use for SLIME/EMACS interaction.

```-oduvan-backend 1``` - supposes that oduvanchik runs on SWANK side and enables some lisp-specific editor features (implementation is under construction). You can also change it via Pres menu bar submenu.

Initialization file
-------------------
Windows: %HOME%\clcon.cfg

Linux: ~/.clconrc 

Completion
----------
Completion of lisp symbols works in console, use **Tab** to complete lisp symbol prefix (may contain package or part of package name).

Use **Ctrl-F3** to complete filename (Unix-style, names containing space may not work). 

Use Control-Alt-u to complete tcl name. Be sure to type in a space after ".."  

Find source command
-------------------
Find source command works best in console. Common-lisp package is assumed. Type 
`defun` or, say, `print-object` on the fresh lisp prompt and press Alt-. 
If there is a single source, you just go to source. 
If there are many, they are printed at console and you can click on either on them with mouse (no way to do that
with keyboard now).
Find source for tcl accepts fully-qualified identifier. Invoke it with Control-F9. 

Find in files
-------------
There is no GUI for find in files. 

## .fics
Abbrev for "find in clcon sources". Accepts one argument: tcl string.

## .finf
More general find in files command. 

Synopsys:

`.finf -dirs {c:/x/y other_unix_style_dirs} -types {c h cpp} searchString`

Default types are {asd lisp}. String is tcl string, use tcl quoting. 

### presets for .finf
Good IDEs have "presets" for finding in some places. To imitate this,
you can create your own commands at initialization file, e.g.

`proc ::clconcmd::finf_budden_tools {searchString} {::clconcmd::finf -dirs c:/clcon/lp/budden-tools $searchString}`

## Lisp functions for the search

There are also Lisp functions for the search. Example of searching for either of two strings:

`(clco::present-text-filtering-results (union (clco::filter-many-files (clco::clcon-sources) "wesppt") (clco::filter-many-files (clco::clcon-sources) "WrapEventScriptForFreezedText") :test 'equalp))`

This is rather lame, as lines are not sorted appropriately when merging two sets.

Example of searching string in files specified by globs (superseded by `.finf`)

`(clco:FIND-STRING-IN-FILES "f4" (clco:FILES-BY-GLOB-LIST "c:/clcon/lp/**/*.lisp" "c:/clcon/lp/**/*.asd"))`


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

```.tcsoh filename.tcl``` loads tcl file from directory where clcon.tcl script is located into main IDE tcl interpeter. Also note we
have file/Reload some of IDE sources which reloads all sources excluding clcon.tcl, record_definition.tcl and named_args.tcl. 

```.hist``` shows command history. It accepts an optional string argument. If supplies, it is treated as glob pattern to filter history event. Asteriks are added at the left and right sides of pattern prior to filtering.

```.NNN``` where NNN is a decimal number re-runs command from history with that number

```.o``` run oduvanchik command in visible editor buffer. Command must be written with dashes and without '-command' suffix, e.g. ```.o indent-new-line```

```.fics string``` finds a string in clcon sources (case insensitive)

```.apr string``` invokes lisp apropos (case insensitive)

```.tapr string``` invokes tcl apropos (case insensitive)

```.help``` lists available IDE commands (no real help, sorry :) )

It is recommended to use your initialization file to define new IDE commands. 

To view the source of the IDE command, type it as `::clconcmd::tapr` and press `Control-F9` on the text.

Tcl escapes
-----------
Place two dots and a space (`.. `) directly at the IDE prompt to pass arbitrary tcl command to tcl interpreter. Use Control-Alt-U to complete tcl names. E.g. this will display tk's message box. 

```
.. tk_messageBox -title "clcon" -message "Wow, it works!" -parent $::tkcon::PRIV(console)
```

Opening files for editing
-------------------------
Use "File" menu, Control-o keyboard shortcut or
.edit IDE command: 
```.edit <filename>```
Press Ctrl-F3 to complete filename (at least it will work undex *nix).

Saving files 
-------------------------
WARNING! 

As you close the IDE, there is sometimes no warning about unsaved files. Also there is a bug in tab switching code so tab names at some point can mismatch real file name of the file being edited. Also note that as some part of IDE crash, editor might become crashed too. Be careful! 

If you have crashed swank connection, first of all try to disable "Oduvan-backend" flag at prefs menu in the console. After that, try Secret/Unfreeze command if your editor appears hanged up. With two that measures, you have good chances to save your work. But, again, don't rely upon IDE. 
Normally, to save files, use "File" menu or Control-s keyboard shortcut. 

Named readtables support
-------------------------
We have support for named readtables in files. Clcon only recognizes keywords as readtable-names. If the file contains a line

`(optional-package:in-readtable :readtable-name)`

lines below that line are considered to be in that readtable. You can have several in-readtable statements in the file though it is not recommended, every `in-readtable` statement acts up to the next `in-readtable` statement. Note that the code before first "in-readtable" statement is assumed to be in readtable nil, but when compiling a file, it will use current readtable indeed. 

If line is affected by above lying `in-readtable` statement, and package is known at this point of the file, swank:*readtable-alist* variable 
is used to determine a readtable to use. 

Note that is *readtable-alist* is modified, clcon ignores the change until it occasionally recalculates cached value of readtable for current line. 
Also note that :named-readtables had a cludge that modified *readtable-alist* very frequently and chaotically. This cludge is removed in forked version
of named-readtables installed with clcon. 

To ensure that change is noted, close and reopen the file. 

Readtable names are upcased regardless of everything when in the editor. When file is being compiled, in-readtable statement parsing 
depends on actual current *readtable* around compilation. 

Opening files with "in-readtable" forms have side effect - uppercased readtable names are interned into keyword package. 

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

###Eval in frame
Evals code in the context of the frame selected on the stack and prints result at the console. Can be invoked via debugger "stack" menu.

###Return from frame
Prompts for lisp expression and returns it from the frame as if it was normal return. Found at "Stack" debugger menu. 

###Stepping, watches
SBCL supports stepping, but it is not done yet and this can be hard to do. Watches are useless without stepping.

###Breakpoints
Just insert [break](http://www.lispworks.com/documentation/lw60/CLHS/Body/f_break.htm) at appropriate place to enter debugger.
Lisp allows you to recompile separate functions without restarting a program, so this is not too inconvinient.

###Tracing (debug messages)
If you want to print debug messages as your code runs, use [trace](http://www.lispworks.com/documentation/lw60/CLHS/Body/m_tracec.htm).
Specific CL implementations usually have extensions for trace, read your Lisp's manual!

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
menu bar/Connection/Disconnect from swank
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


Thread list and debugging
------------------
We have no GUI to debug threads. But we can get list of threads with 
`(swank:list-threads)`, and then debug thread by its number in a list (not by thread id)
with `(swank:debug-nth-thread <N>)` . Numbers are zero-based (not counting header). 
 
Syntax highlighting
------------------
Code highlighting is now rather humble. All files are painted as lisp regardless of extension.
Also we have basic paren highlighting - when you stand after closing paren, editor highlights
appropriate opening paren. 
