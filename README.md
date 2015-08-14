# README #

An attempt to adapt tkcon to Common Lisp (communication via SWANK)

To test, first of all start your lisp, load swank and do 

```
#!lisp
(swank:create-server :port 4009 :dont-close t)
```

Then start clcon.tcl . It should connect automatically to swank. 
Then you can type in expression like 

```
"\\\"asdf\\\""
```
at the console and press Return. Expression should be evaluated on lisp side correctly. 

The only problem is you have to use my patched version of SLIME from here

https://github.com/slime/slime/pull/259