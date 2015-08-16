# README #

An attempt to adapt [tkcon](http://tkcon.sourceforge.net/) to Common Lisp (communication via SWANK)
MIT (or BSD) license (see tkcon's copyright)

To test, start your lisp, load swank and do 

```
#!lisp
(swank:create-server :port 4009 :dont-close t)
```

Then set up path to :clcon-server system and load it
```
(push "path/to/clcon-server/" asdf:*central-registry*)
(asdf:load-system :clcon-server)
```

Then start clcon.tcl . It should connect automatically to swank. 
Then type in expression like 

```
#!lisp
"asdf"
```
or
```
#!lisp
(defun my-func (x) (map 'string 'identity (list #\\ #\" #\$)))
```

at the console and press Return. Expression should be evaluated on lisp side correctly
and result should be printed at clcon (along with lots of debugging traces). 

cl-user package is assumed for all interaction. If you change package, consequences are undefined.