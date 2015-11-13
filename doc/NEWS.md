clcon news

<<TRUNK>> vs 0.3.2
=====
- Support of named-readtables by the editor, see [user manual](user-manual.md)
- Most of the tools have "Window" menu with shortcuts to switch to console, editor, debugger, error browser, editor buffer list. 
- Keyboard shortcuts like "control-s" work regardless of CAPS LOCK state and in Russian keyboard layout. 
- Completion now works as in slime-c-p-c, e.g. with package completion enabled.
- Fixed some bugs in "New" and "Save As"
- Fixed #||# highlight
- Maybe improved highlight colors


0.3.2 vs 0.3.1 
=====
- "New" and "Save As" now obey tradition
- Fixed nasty bugs in Linux version - editor didn't work at all
- Updated Linux startup script and installation instructions

0.3.1 relative to 0.3.0
================================
- package is displayed in the console and is used to find source when editing a file; previously, cl-user was always assumed

- clco:load-system-for-tcl tries to collect compilation notes to a browser. If some errors are not handled by SWANK and debugger pops up, we have new "file" menu to open current asdf component and current system. 

- we are now based on a forked SWANK and forked NAMED-READTABLES to handle readtables more correctly.
No hotpatching was developed. I hope my pull requests
will be accepted and this inconvinience will go away.

- new primitives for file search: clco:find-string-in-files
and clco:files-by-glob-list

- fixed some really nasty bugs and introduced some new bugs 

- in the editor, open dialog uses directory of current file as initial directory

