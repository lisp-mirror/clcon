# README #

## What's that
**clcon** is an attempt to adapt [tkcon](http://tkcon.sourceforge.net/) to Common Lisp (communication via SWANK)
MIT (or BSD) license (see tkcon's copyright)

## Supported platforms
**clcon** is being developed on Debian 8 at x86 processor. Also it is sometimes tested on 32-bit Windows 7 with ActiveTcl 8.6 and seem to work too.

## Current state
- REPL
- completion for lisp symbols and filenames in Repl
- find source in Repl
- command history
- stub of swank-based inspector
- you can still run tcl code with special "escapes"

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
or
```
(dotimes (i 10) (print i) (sleep 0.5))
```

at the console and press Return. Expression should be evaluated on lisp side correctly
and result should be printed at clcon. 

cl-user package is assumed for all interaction. If you change package, consequences are undefined.

## User manual 
[User manual is here](https://bitbucket.org/budden/clcon/src/default/doc/user-manual.md)

## Developers manual 
[See here](https://bitbucket.org/budden/clcon/src/default/doc/)

## License
MIT (or BSD) license (see tkcon's copyright)

## Roadmap
Is in tracker