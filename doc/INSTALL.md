## Clcon installation and startup 

### Load and install dependencies, part 1

- fresh versions of [SBCL](http://www.sbcl.org/platform-table.html) 
- fresh version of tcl/tk. On Windows, you can download tcl/tk from [Activestate](http://www.activestate.com/activetcl/downloads). On Debian, use your package manager to download tk8.6 .
- [quicklisp](https://www.quicklisp.org/beta/) set up at your SBCL
- fresh version of [budden-tools](https://bitbucket.org/budden/budden-tools). Put it under local-projects directory of quicklisp, e.g. (for Linux)
```
cd ~/quicklisp/local-projects
hg clone https://bitbucket.org/budden/budden-tools
```  
- good version of clcon itself, put it where convenient. No "installation" procedure exists yet. 

### Choose if you want to try oduvanchik's backend
Oduvanchik's backend enables a lisp mode. It is not documented yet
so it is unlikey you'll be able to run it. But if you want to give 
it a try, you should load and install dependencies, part 2. 

### Load and install dependencies, part 2
(Only if you choose to try lisp mode)
- put fresh version of [oduvanchik](https://bitbucket.org/budden/oduvanchik) under local-projects directory of quicklisp. E.g. (Linux)
  ```
  cd ~/quicklisp/local-projects
  hg clone https://bitbucket.org/budden/oduvanchik
  ```

### Start your SBCL and install dependencies. 

```
#!lisp
(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :swank) ; don't need it if you have SLIME already
(ql:quickload :cl-tk)
```

To load server-side code to lisp, use
```
#!lisp
;; Set some features.
;; Don't forget to clear your asdf fasl cache and rebuile
;; if you change any feature

; enable oduvanchik backend. This feature will be removed soon
; as oduvanchik backends goes well
(pushnew :clcon-oduvan *features*)

; If :oduvan-invisible set, we can build and start on Windows
; If :oduvan-invisible is not set, we can run only on Linux, but
; we also see normal Oduvanchik's GUI
; I usually develop with :oduvan-invisible disabled
(pushnew :oduvan-invisible *features*)

; uncomment next line to see highlight (oduvanchik would hangs up soon)
; (pushnew :oduvan-enable-highlight *features*) 

;; Setting paths and loading
(pushnew #P"path/to/clcon-server-source/" asdf:*central-registry* :test 'equalp)
;in windows you can write like this:
;(pushnew #P"C:/xxx/xxx/clcon/" asdf:*central-registry* :test 'equalp)
(asdf:load-system :clcon-server)
(swank:create-server :port 4009 :dont-close t)
```

After that, start IDE:

Unix:
```
wish clcon.tcl
```

Windows: double click clcon.tcl

It should start up the IDE and connect automatically to swank. 
Then type in expression like 

```
#!lisp
(print "Hello, world")
```

at the IDE's console and press Return. Expression should be evaluated on lisp side correctly
and result should be printed at clcon. 

cl-user package is assumed for all interaction. If you change package, consequences are undefined.
