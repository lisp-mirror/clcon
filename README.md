# README #

# VERY IMPORTANT NEWS #

Clcon didn't attract great attention in the world. Most important, I have zero patches from third-party developers (not counting one patch for documentation). So at the beginnig of 2016 I'll start localizing clcon in a "destructive" fashion. All English strings in clcon user interface would be replaced with Russian ones. Documentation will be translated and no further support of English version will continue. If someone is interested in keeping English version, there are two ways:

a) contribute. Supply me a technology which allows to keep both versions. Russian version is planned to be primary, but I guess there are some tools which could allow for keeping translation table to replace all Russian strings with English and to build English version of interface. Technology must not complicate things essentially. E.g. replacement of strings with named constants is inappropriate complexity. 

b) donate.

## What's that
**clcon** is a cross-platform Common Lisp IDE under construction. Current release is 0.3.3 (see tags in repo). 

## Note about versions
If your're reading this document online, this document describes trunk version. 
Description of file release is under c:/clcon/lp/clcon/readme.md when release is installed.

## Platform support
Supported lisp implementation(s):

- SBCL

Supported tcl version(s):

- tcl 8.6.3, 8.6.4

Platforms where testing is 

- Debian 8 (32 bit) at x86 processor - tested regularly
- Debian 17.2 (64 bit) with the help of tombert's tcltk, see http://wiki.tcl.tk/668, tested occasionally
- Windows 7 (32 bit) - tested regularly
- Windows 7 (64 bit), SBCL x86 - tested occasionally

## Screenshots
[See here](https://bitbucket.org/budden/clcon/wiki/Screenshots)

## Project goals
- Permissively licenenced GUI for embedding of CL and CL-based languages into applications

## Current state
See [news](doc/NEWS.md)

Clcon is at "alpha" stage. It is currently used for self development and shows itself rather robust in careful hands of its creator. Crashes or necessity to use EMACS are infrequent.

Clcon have most of essential features required for efficient CL development. 
But some of them are still missing. 

If you want to try using clcon, first of all take a [demo tour](doc/demo-tour.md).

## Current features
- REPL for Lisp and Tcl with history
- Completion, find source, apropos for Lisp
- Limited completion, find source, apropos for Tcl
- Find in files
- Debugger, Stepper, Inspector from SWANK 
- Multi-tabbed Editor with lisp syntax highlight, autoindent, lisp forms navigation
- Compile lisp from IDE, compilation error browser with "jump to source"
- List definitions in a file for Lisp and Tcl (very primitive version)

## Installation and startup
For windows, we have [file release](https://bitbucket.org/budden/clcon/downloads/clcon-0.3.3.zip)

Also you can download 
[cl-stirling-engine](https://bitbucket.org/budden/cl-stirling-engine/downloads/cl-stirling-engine-at-clcon-0.3.1.zip) which
contains clcon 0.3.1 . Documentation related to release is in the file itself. 

For Linux, see [doc/INSTALL.md](doc/INSTALL.md).

## Documentation
**Warning!** All documentation describes trunk version. If you download the release, 
documentation is inside the release. **Do not read online documentation** then!

[Demo tour (trunk version)](doc/demo-tour.md) - a footpath through best views of some features of clcon.

[User manual (trunk version)](doc/user-manual.md) 

[Wiki](https://bitbucket.org/budden/clcon/wiki/) - contains [screenshots](https://bitbucket.org/budden/clcon/wiki/Screenshots), but otherwise is not very useful.  

[FAQ](https://bitbucket.org/budden/clcon/src/default/doc/FAQ.md)

[Docs directory, including some of the above](https://bitbucket.org/budden/clcon/src/default/doc/)

## License
MIT (or BSD) license (see [tkcon's](http://tkcon.sourceforge.net/) copyright)

## Roadmap
- keep number of open bugs from growing
- issure 1.0 someday