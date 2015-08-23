# README #

## What's that
An attempt to adapt [tkcon](http://tkcon.sourceforge.net/) to Common Lisp (communication via SWANK)
MIT (or BSD) license (see tkcon's copyright)

## Supported platforms
Developed on Debian 8. Also seem to work on Windows 7 with ActiveTcl 8.6

## Installation and startup 
To test, start your lisp, load SWANK and do 

```
#!lisp
(swank:create-server :port 4009 :dont-close t)
```

Then set up path to :clcon-server system and load it
```
(push "path/to/clcon-server/" asdf:*central-registry*)
(asdf:load-system :clcon-server)
```

Then start 
```
wish clcon.tcl
``` 
on your shell (cmd on windows). It should connect automatically to swank. 
Then type in expression like 

```
#!lisp
"asdf"
```
or
```
#!lisp
(defun my-func (x) (map 'string 'identity (list #\\ #\" #\$)))
```

at the console and press Return. Expression should be evaluated on lisp side correctly
and result should be printed at clcon. 

cl-user package is assumed for all interaction. If you change package, consequences are undefined.

## Command line options
Most of [tkcon's command line options](http://tkcon.sourceforge.net/docs/start.html), are kept intact.
New options: 

```-putd-enabled 1``` - enable printing lots of debug messages. ```-putd-enabled 0``` - to disable (default).

```-swank-ip ADDR``` - assign an alternate IP address for swank server (default is 127.0.0.1).

```-swank-port NNNN``` - assign alternate port for swank connection. Do not connect to the port use use for SLIME/EMACS interaction. 

## Connecting/disconnecting to/from SWANK
Use two items on Console menubar item

## tcl escapes
Place dot (.) in the first position to send special IDE command. Currently there are only two commands:
```.insp lisp-expr``` should call inspector on lisp-expr, but currently there is only a stub of inspector
```.tcsoh filename.tcl``` loads tcl file from current directory into main IDE tcl interpeter 

Place two dots (..) to pass arbitrary tcl command to slave tcl interpreter (which can be wiped out soon from the IDE completely).
Place three dots (...) to pass tcl command to master tcl interpreter of the IDE. E.g.

```
...source path/swank-io.tcl
```
would load tcl code into main IDE's interpreter. 

## Tcl errors
Many tcl errors printed in read are clickable with mouse. Error stack is shown by the click. 

## Completion
Completion works in console, use Tab to complete lisp symbol prefix (may contain package or part of package name).
Use Ctrl-F3 to complete filename (Unix-style, names containing space may not work). 
Tcl completion is currently broken. 

## Find source command
Currently finding a source can't properly extract symbol. So to see how it work just type
defun 
or, say, 
print-object 
on the fresh lisp prompt and press Alt-. 
If there is a single source, you just go to source. 
If there are many, they are printed at console and you can click on either on them with mouse (no way to do that
with keyboard now, sorry for that, this is a bug)

## Also Read tkcon manual
Manual is [here](http://tkcon.sourceforge.net/docs/index.html)
 
## What if I want to contribute
Read issues

## License
MIT (or BSD) license (see tkcon's copyright)

## Roadmap
Is in tracker