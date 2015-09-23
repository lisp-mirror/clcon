# README #

## What's that
**clcon** is a cross-platform Common Lisp IDE under construction.

Supported lisp implementation(s):

- SBCL

Supported platforms: 

- Debian 8 (32 bit) at x86 processor
- Windows 7 (32 bit)
- as of 2015-09-11, reported by pcchange90 to run on windows 7(64 bit) and sbcl(x86,not amd64)

Code is completely platform-independent and should run on other platforms too. Also it might run on other lisp implmentations, though I don't remember. Give it a try :) 

## Screenshots
[See here](https://bitbucket.org/budden/clcon/wiki/Screenshots)

## Project goals
Simple, one-click installable CL environment for beginners

## Current state
Overall state and quality is "working prototype".
It is likely that you fall into trouble at any place. 
So do not try to just "play around" with the IDE. Take
a [demo tour](doc/demo-tour.md) instead.

Features demonstrated are: 

- REPL with command history and history substitution
- Debugger (view stack, locals, goto source, invoke restarts, search)
- completion for lisp symbols and filenames in REPL
- find source in REPL
- lisp mode (auto-indent, moving through lists, mark defun etc.)
- inspector 
- compilation notes browser
- editor buffer list
- "Find" and "Find next" commands for text and treeview
- running of tcl code with special "escapes"

## Installation and startup 
You need:

### Dependencies, part 1

- fresh versions of [SBCL](http://www.sbcl.org/platform-table.html) 
- fresh version of tcl/tk. On Windows, you can download tcl/tk from [Activestate](http://www.activestate.com/activetcl/downloads). On Debian, use your package manager to download tk8.6 .
- [quicklisp](https://www.quicklisp.org/beta/) set up at your SBCL
- fresh version of [budden-tools](https://bitbucket.org/budden/budden-tools). Put it under local-projects directory of quicklisp, e.g. (for Linux)
```
cd ~/quicklisp/local-projects
hg clone https://bitbucket.org/budden/budden-tools
```  
- good version of clcon itself, put it where convenient. No "installation" procedure exists yet. 

### Choose if you want to try oduvanchik's backend
Oduvanchik's backend is linux-only and is not well tested for now. 
But if you want to give it a try, you should do Dependencies, part 2. 

### Dependencies, part 2
(Only if you choose to try oduvanchik's backend)
- put fresh version of [oduvanchik](https://bitbucket.org/budden/oduvanchik) under local-projects directory of quicklisp. E.g. (Linux)
  ```
  cd ~/quicklisp/local-projects
  hg clone https://bitbucket.org/budden/oduvanchik
  ```

### Start your SBCL and install dependencies. 

```
#!lisp
(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :swank) ; don't need it if you have SLIME already
(ql:quickload :cl-tk)
```

To load server-side code to lisp, use
```
#!lisp
; uncomment next line if you chose to use oduvanchik backend
; (pushnew :clcon-oduvan *features*)
(pushnew #P"path/to/clcon-server-source/" asdf:*central-registry* :test 'equalp)
;in windows you can write like this:
;(pushnew #P"C:/xxx/xxx/clcon/" asdf:*central-registry* :test 'equalp)
(asdf:load-system :clcon-server)
(swank:create-server :port 4009 :dont-close t)
```

After that, start IDE:

Unix:
```
wish clcon.tcl
```

Windows: double click clcon.tcl

It should start up the IDE and connect automatically to swank. 
Then type in expression like 

```
#!lisp
(print "Hello, world")
```

at the IDE's console and press Return. Expression should be evaluated on lisp side correctly
and result should be printed at clcon. 

cl-user package is assumed for all interaction. If you change package, consequences are undefined.

## Documentation

[Demo tour](doc/demo-tour.md) - narrow footpath through best views of all currently working features of clcon

[User manual](https://bitbucket.org/budden/clcon/src/default/doc/user-manual.md)

[Wiki](https://bitbucket.org/budden/clcon/wiki/) - contains [screenshots](https://bitbucket.org/budden/clcon/wiki/Screenshots), but otherwise is not very useful.  

[FAQ](https://bitbucket.org/budden/clcon/src/default/doc/FAQ.md)

[Docs directory, including some of the above](https://bitbucket.org/budden/clcon/src/default/doc/)

## License
MIT (or BSD) license (see [tkcon's](http://tkcon.sourceforge.net/) copyright)

## Roadmap
1. One-click installable distribution.
2. Hacking of missing features.
3. Code cleanup.