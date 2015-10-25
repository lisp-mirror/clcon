# README #

## What's that
**clcon** is a cross-platform Common Lisp IDE under construction. Current version is 0.2.6 (see tags in repo).

## Note about versions
If your're reading this document online, this document describes trunk version. 
Description of file release is under c:/clcon/quicklisp/local-projects/clcon/readme.md when release is installed.

## Platform support
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
I begun switching to devopment of clcon in clcon itself. Some features are still missing when
compared to EMACS, I'm adding them as far as their absense becomes intolerable. 

If you want to try using clcon, prepare to problems - this is still prototype quality. 
So first of all take a [demo tour](doc/demo-tour.md).

## Current features
- REPL for Lisp and Tcl with history
- Completion, find source, apropos for Lisp and TCl in REPL
- Find in files (prototype)
- Debugger, Stepper, Inspector from SWANK 
- Editor with lisp syntax and paren highlight
- Some parts of lisp mode: auto-indent, navigate through forms
- Compilation error browser

## Installation and startup
For windows, we have [File release](https://bitbucket.org/budden/clcon/downloads/clcon-0.2.6.zip). 
Note: online documentation describes current repository trunk. Documentation for the file release version (including readme file) is inside the release archive itself, at clcon/quicklisp/local-projects/clcon subdirectory of archive. See doc/INSTALL.md for installation instructions.

For the trunk, see [INSTALL.md](https://bitbucket.org/budden/clcon/src/default/doc/INSTALL.md).

## Documentation
[Demo tour (trunk version)](doc/demo-tour.md) - narrow footpath through best views of some features of clcon.

[User manual](https://bitbucket.org/budden/clcon/src/default/doc/user-manual.md)

[Wiki](https://bitbucket.org/budden/clcon/wiki/) - contains [screenshots](https://bitbucket.org/budden/clcon/wiki/Screenshots), but otherwise is not very useful.  

[FAQ](https://bitbucket.org/budden/clcon/src/default/doc/FAQ.md)

[Docs directory, including some of the above](https://bitbucket.org/budden/clcon/src/default/doc/)

## License
MIT (or BSD) license (see [tkcon's](http://tkcon.sourceforge.net/) copyright)

## Roadmap
- minimal "list definitions in file" tool
- print "who calls" info
- package-awareness of IDE
- use as a GUI for [cl-stirling-engine](https://bitbucket.org/budden/cl-stirling-engine)
- close all bugs (ha-ha!)