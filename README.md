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
- GUI for [cl-stirling-engine](https://bitbucket.org/budden/cl-stirling-engine)
- Simple, one-click installable CL environment for beginners

## Current state
Overall state and quality is "working prototype".
It is likely that you fall into trouble at any place. 
So do not try to just "play around" with the IDE. Take
a [demo tour](doc/demo-tour.md) instead.

Features demonstrated are: 

- REPL with command history and history substitution
- Debugger (view stack, locals, goto source, invoke restarts, search)
- completion for lisp symbols and filenames in REPL
- lisp mode with syntax-highlight, find-source, auto indent (not covered by demo tour)
- inspector 
- compilation notes browser
- editor buffer list
- "Find" and "Find next" commands for text and treeview
- running of tcl code with special "escapes"
- recent file list

## Installation and startup
For windows, we even have [File release](https://bitbucket.org/budden/clcon/downloads/clcon-0.2.2.zip). 
Anyway, see [doc/INSTALL.md](https://bitbucket.org/budden/clcon/src/default/doc/INSTALL.md) for installation instructions. 

## Documentation

[Demo tour](doc/demo-tour.md) - narrow footpath through best views of all currently working features of clcon

[User manual](https://bitbucket.org/budden/clcon/src/default/doc/user-manual.md)

[Wiki](https://bitbucket.org/budden/clcon/wiki/) - contains [screenshots](https://bitbucket.org/budden/clcon/wiki/Screenshots), but otherwise is not very useful.  

[FAQ](https://bitbucket.org/budden/clcon/src/default/doc/FAQ.md)

[Docs directory, including some of the above](https://bitbucket.org/budden/clcon/src/default/doc/)

## License
MIT (or BSD) license (see [tkcon's](http://tkcon.sourceforge.net/) copyright)

## Roadmap
- fix bugs in lisp mode 
- paren highlighting
- use as a GUI for [cl-stirling-engine](https://bitbucket.org/budden/cl-stirling-engine)
- eval-in-frame
- cross-reference navigation
- better editor functionality
- thread list with thread debugging
- grep
- fix other numerous bugs
- full-featured menu