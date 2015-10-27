# README #

## What's that
**clcon** is a cross-platform Common Lisp IDE under construction. Current release is 0.3.0 (see tags in repo).

## Note about versions
If your're reading this document online, this document describes trunk version. 
Description of file release is under c:/clcon/quicklisp/local-projects/clcon/readme.md when release is installed.

## Platform support
Supported lisp implementation(s):

- SBCL

Supported tcl version(s):

- tcl 8.6.3, 8.6.4

Supported platforms: 

- Debian 8 (32 bit) at x86 processor
- Debian Mint Rafaela (64 bit) with the help of tombert's tcltk, see http://wiki.tcl.tk/668
- Windows 7 (32 bit)
- as of 2015-09-11, reported by pcchange90 to run on windows 7(64 bit) and sbcl(x86,not amd64)

## Screenshots
[See here](https://bitbucket.org/budden/clcon/wiki/Screenshots)

## Project goals
- GUI for [cl-stirling-engine](https://bitbucket.org/budden/cl-stirling-engine)
- Just an alternative to EMACS/SLIME with permissive license.

## Current state
Clcon is at "alpha" stage. It is currently used for self development and shows itself rather robust in careful hands of its creator. Crashes or necessity to use EMACS are infrequent.

Clcon have most of essential features required for efficient CL development. 
But some of them are still missing. 

If you want to try using clcon, first of all take a [demo tour](doc/demo-tour.md).

## Current features
- REPL for Lisp and Tcl with history
- Completion, find source, apropos for Lisp and TCl in REPL
- Find in files
- Debugger, Stepper, Inspector from SWANK 
- Editor with lisp syntax highlight, autoindent, lisp forms navigation
- Compile lisp from IDE, compilation error browser with "jump to source"
- List definitions in a file for Lisp and Tcl (very primitive version)

## Installation and startup
For windows, we have [File release](https://bitbucket.org/budden/clcon/downloads/clcon-0.3.0.zip). 
Documentation related to release are in the file itself. 

For Linux, see [doc/INSTALL.md](doc/INSTALL.md).

## Documentation
[Demo tour (trunk version)](doc/demo-tour.md) - a footpath through best views of some features of clcon.

[User manual](doc/user-manual.md) 

[Wiki](https://bitbucket.org/budden/clcon/wiki/) - contains [screenshots](https://bitbucket.org/budden/clcon/wiki/Screenshots), but otherwise is not very useful.  

[FAQ](https://bitbucket.org/budden/clcon/src/default/doc/FAQ.md)

[Docs directory, including some of the above](https://bitbucket.org/budden/clcon/src/default/doc/)

## License
MIT (or BSD) license (see [tkcon's](http://tkcon.sourceforge.net/) copyright)

## Roadmap
- package-awareness of IDE (status bar, prompt, find source)
- use as a GUI for [cl-stirling-engine](https://bitbucket.org/budden/cl-stirling-engine)
- close all bugs (ha-ha!)
- print "who calls" info