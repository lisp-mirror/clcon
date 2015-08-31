# README #

## What's that
**clcon** is a cross-platform Common Lisp IDE under construction. Supported platforms: 

- Debian 8 (32 bit) at x86 processor
- Windows 7 (32 bit)

Code is completely portable and should run on other platforms too. 

## Screenshots
[See here](https://bitbucket.org/budden/clcon/wiki/Screenshots)

## Current state
- REPL with command history and history substitution
- completion for lisp symbols and filenames in Repl
- find source in Repl
- swank-based inspector 
- compilation error browser
- editor buffer list
- you can still run tcl code with special "escapes"

## Installation and startup 
You need:

- fresh versions of [SBCL](http://www.sbcl.org/platform-table.html) 
- fresh version of tcl/tk. On Windows, you can download tcl/tk from [Activestate](http://www.activestate.com/activetcl/downloads). On Debian, use your package manager to download tk8.6 . 
- [quicklisp](https://www.quicklisp.org/beta/) set up at your SBCL

Next, start your SBCL and install dependencies. 

```
#!lisp
(ql:quickload :swank) ; don't need it if you have SLIME already
(ql:quickload :cl-tk) 
```

To load server-side code to lisp, use
```
#!lisp
(push "path/to/clcon-server-source/" asdf:*central-registry*)
(asdf:load-system :clcon-server)
(swank:create-server :port 4009 :dont-close t)
```

After that, start IDE
```
wish clcon.tcl
``` 
It should start up the IDE and connect automatically to swank. 
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
or
```
(dotimes (i 10) (print i) (sleep 0.5))
```

at the IDE's console and press Return. Expression should be evaluated on lisp side correctly
and result should be printed at clcon. 

cl-user package is assumed for all interaction. If you change package, consequences are undefined.

## User manual 
[User manual is here](https://bitbucket.org/budden/clcon/src/default/doc/user-manual.md)

[Wiki](https://bitbucket.org/budden/clcon/wiki/) contains screenshots, but otherwise is not very useful. 

## Developers manual 
[See here](https://bitbucket.org/budden/clcon/src/default/doc/)

## License
MIT (or BSD) license (see tkcon's copyright)

## Roadmap
Is in tracker