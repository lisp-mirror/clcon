clcon FAQ

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
