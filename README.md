# README #

An attempt to adapt [tkcon](http://tkcon.sourceforge.net/) to Common Lisp (communication via SWANK)

To test, 
1. Start your lisp, load swank and do 

```
#!lisp
(swank:create-server :port 4009 :dont-close t)
```

2. Then set up path to :clcon-server system and load it
```
(push "path/to/clcon-server/" asdf:*central-registry*)
(asdf:load-system :clcon-server)
```

3. Start clcon.tcl . It should connect automatically to swank. 

4. Then you can type in expression like 

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