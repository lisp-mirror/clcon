Clcon installation and startup 
==============

Warning
-------
Clcon is alpha stage. Before trying to use, please take a [Demo Tour](demo-tour.md)). You'll see that some
features sometimes work. After that, you can try to use clcon, but it is likely you'll have a trouble. 

I use clcon to develop itself, but editing files is rather dangerous. Editor can crash at any moment. 
So it is easy to lose your work. 

Windows: installing file release
----------
- Download file release from [Downloads page](https://bitbucket.org/budden/clcon/downloads)
- Unpack it to c:\clcon
- Start c:\clcon\bin\clcon-server-and-client.cmd - this will load and run the IDE. When running for the first time, building will take a while. Subsequent loads are faster
- To see what clcon can do, take a [Demo Tour](demo-tour.md)
- If all is ok, you have clcon with SBCL (use Alt-. to find sources of SBCL objects) and quicklisp (use ql:quickload to load libraries)
- Repositories of clcon components (clcon, oduvanchik, budden-tools) are at c:\clcon\quicklisp\local-projects - feel free to send patches :)

Alternative way: loading sources and building (Linux)
-----------

### Load and install dependencies

- fresh versions of [SBCL](http://www.sbcl.org/platform-table.html) 
- fresh version of tcl/tk. On Windows, you can download tcl/tk from [Activestate](http://www.activestate.com/activetcl/downloads). On Debian, use your package manager to download tk8.6 .
- [quicklisp](https://www.quicklisp.org/beta/) set up at your SBCL
- fresh version of [budden-tools](https://bitbucket.org/budden/budden-tools). Put it under local-projects directory of quicklisp, e.g. (for Linux)
```
cd ~/quicklisp/local-projects
hg clone https://bitbucket.org/budden/budden-tools
```  
- good version of clcon and [oduvanchik](https://bitbucket.org/budden/oduvanchik), 
put them under local-projects directory of quicklisp. Trunk can contain arbitrary debug
features and code, or be badly broken. We recommend updating to
lateset tagged "release", e.g. 0.2.6. At Linux this can look like:
``
cd ~/quicklisp/local-projects
hg clone -u 0.2.6 https://bitbucket.org/budden/oduvanchik
hg clone -u 0.2.6 https://bitbucket.org/budden/clcon
``

### Start your SBCL and install dependencies. 

```
#!lisp
(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :swank) ; don't need it if you have SLIME already
(ql:quickload :cl-tk)
(ql:quickload :named-readtables)
(ql:quickload :command-line-arguments)
(ql:quickload :cl-utilities)
```

To load server-side code to lisp, use
```
#!lisp
;; Set some features.
;; Don't forget to clear your asdf fasl cache and rebuild
;; if you change any feature

; enable oduvanchik backend. This feature will be removed soon
; as oduvanchik backends goes well
(pushnew :clcon-oduvan *features*)

; If :oduvan-invisible set, we can build and start on MS Windows
; If :oduvan-invisible is not set, we can run only on Linux, but
; we also see normal Oduvanchik's GUI
(pushnew :oduvan-invisible *features*)

; May be needed sometimes for the debugging. 
; (pushnew :oduvan-use-sleep-in-dispatch *features*)

; comment next line if code highlight causes problems 
(pushnew :oduvan-enable-highlight *features*) 

;; Setting paths and loading
(pushnew #P"path/to/clcon-server-source/" asdf:*central-registry* :test 'equalp)
;in windows you can write like this:
;(pushnew #P"C:/xxx/xxx/clcon/" asdf:*central-registry* :test 'equalp)
(asdf:load-system :clcon-server)
(swank:create-server :port 4009 :dont-close t)
```

As you have sbcl running, do

```
wish clcon.tcl
```
Then type in expression like 

```
#!lisp
(print "Hello, world")
```

at the IDE's console and press Return. Expression should be evaluated on lisp side correctly
and result should be printed at clcon. 

cl-user package is assumed for all interaction. If you change package, consequences are undefined.

To see what clcon can do, take a [Demo Tour](demo-tour.md)
