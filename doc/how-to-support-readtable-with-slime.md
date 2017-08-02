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

## Straightforward solution

### Idea
- insert (IN-READTABLE) into every file where non-standard readtable is used
- make SLIME to use readtable as follows
```  
If there is an IN-READTABLE form above the point
  use that readtable (closest one if there are many)
elseIf there is an entry in *READTABLE-ALIST* for the package
  use that readtable
else
  use standard readtable
```
- change IN-READTABLE so that it does not modify `*READTABLE-ALIST*`
- document `*READTABLE-ALIST*`

### Objections
- some code is already adapted to current state of things and will break from that decision
- mass update is required, not only for maintained libraries, but for unmaintained also

## Maybe a better solution

### Idea
Key issue is that generally effect of IN-READTABLE on `*READTABLE-ALIST*` is bad, but sometimes it is necessary. 

So the idea is to conditionalize behavior of IN-READTABLE. E.g. one can build the list of projects where IN-READTABLE behavior should be kept as it is. For unmaintained repos, this list can be kept with NAMED-READTABLES source or in some dedicated repository. For maintained, it would be fine to extend asdf with a special clause. 

So, IN-READTABLE would set an entry to `*READTABLE-ALIST*` for some list of packages only. If package is not in list, IN-READTABLE does not touch its entry. In this case, the person who maintains the image can can manually add entries to `*READTABLE-ALIST*`. 

### Transition

How to introduce that new behavior? We might want an option for IN-READTABLE to signal an error when it sees that readtable assigned is in contradiction with non-nil readtable from `*READTABLE-ALIST*`. This way we well see all problems when we first try to build our image with the new version of NAMED-READTABLES. This option should be enabled by default. 

### Objections and comments
Are welcome :)





