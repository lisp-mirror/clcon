# How to implement readtable support in SLIME? 

## Behavior and API are not documented

Neither SLIME nor NAMED-READTABLE's project documentation describe how do those two cooperate. But this cooperation is both non-trivial and important. I'm certain that top lisp experts know how it works, but generally this is an issue that limits the usability of readtables and thus hurts the popularity of CL. So it would be nice to improve that. 

## Current state of SLIME + NAMED-READTABLE cooperation

### One example
SLIME does not fully support readtables. Consider we have files X.lisp and Y.lisp
```
;; X.lisp
(in-package :foo)
(in-readtable :foo-rt)
;; eof

;; Y.lisp
(in-package :foo)
;;
```
Y.lisp is inherently problematic. What is intended by omitted `IN-READTABLE`? Normally, it means
that Y.lisp is in standard readtable. But it can also mean that it is arranged to be processed by some non-standard readtable, which is set up around compiling of Y.lisp .E.g. if one uses ASDF, there are [perform :around methods](https://common-lisp.net/project/asdf/asdf.html#How-do-I-work-with-readtables_003f). If so, how can SLIME know which readtable to use for slime-compile-defun? 

There is a dirty hack which generally works. That is, undocumented `swank:*READTABLE-ALIST*` . This variable keeps package-name -> readtable-name mapping. 

`READTABLE-ALIST` is filled from two sources: 

- [default-readtable-alist](https://github.com/slime/slime/blob/6e20d01e446334848ea31ace0ce041cec25647ab/swank/sbcl.lisp#L441) gives an initial value for READTABLE-ALIST, specific to this CL implementation
- [in-readtable](https://github.com/melisgl/named-readtables/blob/master/src/named-readtables.lisp#L168) . Every time `IN-READTABLE` form is being processed at compile-toplevel, load-toplevel or execute situation, `*PACKAGE*` is mapped to readtable named. 

### Case studies of current SLIME behavior

A. When loading the project, X.lisp is compiled first. After that, Y.lisp is compiled with `*readtable* = #<named-readtable foo-rt>` . Then everything is relatively ok. SLIME has correct idea about readtable for Y.lisp

B. When loading the project, X.lisp is compiled first. After that, Y.lisp is compiled with standard readtable. Then SLIME would think that Y.lisp is in foo-rt, which is wrong.

C. When loading the project, Y.lisp is compiled with some :alternative-rt . In this case SLIME might misinterpret both Y.lisp and X.lisp, depending on sequence of user's livecoding activity. 

### Implicit constraints on readtable use
From all that, one rule follows: 

- if you use more than one readtable for the same package, you will have issues with livecoding

This rule is quite non-obvious. 


### Conslusion
To sum up, SLIME + NAMED-READTABLES behavior is currently non-documented and unreliable. 

## Suggested solution

- currently `(in-readtable DESIGNATOR)` maps `*PACKAGE*` to `DESIGNATOR` globally every time `in-readtable` form is executed, storing the mapping in `*READTABLE-ALIST*`, see `%frob-swank-readtable-alist`. Let us make a provision to set up named-readtables so that it would NOT touch `*READTABLE-ALIST*` for specific packages and/or readtables. 

- currently for any file operation (e.g. `slime-compile-defun`) SLIME parses the file and finds the closest `(in-package)` form above the point to learn current package. Let us make it look for `(in-readtable)` form also.

- currently SLIME sends package designator to SWANK with most requests `(:emacs-rex request)`. Readtable is deduced by SWANK from `*READTABLE-ALIST*`. Let us modify SLIME so that it would send a readtable designator also. Readtable designator is taken from `(in-readtable)` form. For REPL-related requests, `*readtable*` is sent. If there is neither `(in-readtable)` form nor `*readtable*` in the context, SLIME sends some "no readtable specified" marker. When "no readtable specified" is sent, SWANK takes the readtable from `*READTABLE-ALIST*` for a package given. 

## Use of SLIME with this solution

### Projects which work well with current behavior
Nothing is changed

### Projects which want better behavior
- set packages and readtables of the project to the exclusion list, so that `*READTABLE-ALIST*` is not touched when `(in-readtable)` form is executed for those packages and readtables
- insert `(in-readtable)` form into each source file
- issue `(in-readtable)` after `(in-package)` in REPL
- if one needs a default readtable for package, it can be set in `*READTABLE-ALIST*` manually
