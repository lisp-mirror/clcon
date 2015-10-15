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
IDE slowly moves to maturity. The most problematic part is oduvanchik editor backend
which has some "floating" crashes. Also some necessary things for serious development
are still missing, e.g. working in other package than cl-user is hard now. 

It is likely that you fall into trouble at any place. 
So if you want to take a look at clcon, do not try to just "play around" 
with the IDE. Take a [demo tour](doc/demo-tour.md) instead.

Current features are: 

- REPL for Lisp and Tcl with history
- Debugger 
- Stepper
- Multitabbed Editor (currenty have no save confirmation when quitting, beware!)
- Completion for Lisp, Tcl and filenames in REPL
- Find Source for Lisp and Tcl. Find source for TCL seem to be rather unique feature of clcon
- Some commands of lisp mode (navigate through forms, auto-indent, have bugs...)
- Inspector
- Compilation error browser

## Installation and startup
For windows, we have [File release](https://bitbucket.org/budden/clcon/downloads/clcon-0.2.4.zip). 
Anyway, see [doc/INSTALL.md](https://bitbucket.org/budden/clcon/src/default/doc/INSTALL.md) for installation instructions. 

## Documentation

[Demo tour](doc/demo-tour.md) - narrow footpath through best views of some features of clcon (a bit outdated)

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
- cross-reference navigation
- better editor functionality
- thread list with thread debugging
- grep
- fix other numerous bugs
- full-featured menu