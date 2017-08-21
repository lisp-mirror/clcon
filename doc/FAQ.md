clcon FAQ

[TOC]

Which tcl/tk binding library do you use?
================================

Almost no library. Lisp side does not deals with tcl/tk objects/widgets as it is only server for processing of lisp data, threads, stacks, etc. Tk code is written in tcl and is mostly stored in *.tcl files. This is why we need no bindings for tk widgets. Sometimes we generate tcl code on lisp side, for quoting data we use couple functions from [cl-tk](http://marijnhaverbeke.nl/cl-tk/).


Why I can not activate the editor with Ctrl-Shift-E?
=====

You can only activate the editor after you have some file opened. 
Ways to open file are described in [user manual](user-manual.md)

Note that currently closing the editor with cross or with Alt-F4 does not
close files, but only hides editor window! And that no file save 
functionality implemented at all! Don't forget clcon is on prototype stage.


What's wrong with SLIME?
=====

1. Emacs (unconventional interface)
2. Emacs (GPL - can't embed it into commercial apps)
3. Nothing - we use SLIME's backend, called SWANK.


Я написал процедуру в файле .tcl и загружаю с помощью F7, но она не видна в среде
===========================

И при определении, и при вызове процедуры следует указать пространство имён. Например, глобальное:

    proc ::МояПроцедура {} { ... }


Why do you use tk and not Qt
=========================
Using Qt assumes some kind of bindings . Qt is written in C++. What happens if I need to debug my application? If I debug Lisp part, I use lisp debugger. If I debug C++ part, I use C++ debugger. I have no debugger which is good on both Lisp and C. If my problem is on the boundary between C++ and Lisp, it has consequences:

- I need both debuggers
- If my app crashes I have to restart it
- If my bindings generate C code to be compiled with C compiler, I have to restart my app when I change bindings

So, language with bindings combines weak sides of C++ (low reliability, necessity of restart when you change code) and weak sides of Lisp (less details in the debugger). Application gets fragile. Sometimes I run the same lisp image I develop for several days w/o restart, suspending my PC in the evening and resuming it in the morning. One can hardly imagine that C++ application being under heavy development can run for days w/o restart.

What else wrong with Qt? 

- Qt changes quickly. Some Qt<->Lisp interface projects die time from time, so when I try to use them with new versions of Qt, I first have to deal with Qt version upgrade
- Qt license is non-permissive - either *GPL or commercial
- Qt is for modified C++, which not always the best fit for use with CL. It has different OOP and different memory management. 

Why do you use client-server and not ltk
=====================================
First of all, ltk license is not good for me. I avoid *GPL because I suspect that image containing LLGPL libraries is licensed under LLGPL and it means that I have to open all my sources if I deliver my application with LLGPL library. Even if it was permissive, it offers a very limited set of widgets. It generates small pieces of tcl code on the fly - this is not the efficient way to use tcl, because tcl has some optimizer. Re-optimizing snippets of tcl code at each keystroke is inefficient. Also it would require much more network exchange between server and client. If some problem arises on the side of tk, I have to learn tk. Then I find a workaround for the problem, it is a piece of tcl code, not a piece of code written in ltk. So I have to translate the code to lisp to obtain this workaround. 

So I split application in two and write everything related to GUI directly in tcl. 

Also using tcl directly allowed me to borrow tkcon (about 4kloc of tcl code) and take it as a starting point. I took tcltextedit as a starting point of the editor frontend. My plans are to use RamDebugger to debug the IDE and maybe even change my window management to emply RamDebugger's frame-based tool layout. All this would be impossible if I would limit myself to bindings - I'd have to rewrite everything from the scratch or translate manually. But tcl is denser language than CL - it has stronger MP abilities and a denser standard containers. So I would not benefit from CL's strong sides if I would translate tcl code to lisp. 

So, the only advantage of using bindings like ltk is to get rid of necessity to learn tcl (which is an illusion). Another thing I lose is possible protection from "quoting hell". Almost everything else in this approach is a disadvantage, so it was easy to take the decision. For lisper, tcl is easy and fun to learn (except quoting, again, but this is also not extremely hard). 

Why are you using tcl and not Python
====================================
tcl is a natural choice for tk. Using python with tk is a kind of bindings, so it makes things more fragile, increases complexity, might hide some features. ALso I'm afraid using tk w/o tcl is simply impossible, so that even if you're using Python with tk, anyway tcl interpreter(s) is(are) created behind the scenes with all consequeneces which follow from that. Using tcl is just "KISS principle"



