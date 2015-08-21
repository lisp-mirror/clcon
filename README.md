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

## Tcl commands
Place dot (.) in the first position of line to send command to tkconsole instead of SWANK server. 
Many tkcon commands are supported, but some are broken. Beware that tkcon is rather powerful
and it had a way to run unix commands. I think I have broken it, but don't harm your system!

## Hot swapping development of tcl side
Well all we are lispers and we like incremental development. For that you can try

```
.tkcon main source path/swank-io.tcl
```

to reload swank-io.tcl which contains swank communication routines. 

## Completion
Completion works in console, use Tab to complete lisp symbol prefix (may contain package or part of package name).
Use Ctrl-F3 to complete filename (Unix-style, names containing space may not work). 

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