clcon news

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
