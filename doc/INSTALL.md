Clcon <TRUNK> installation and startup 
==============

Warning
-------
Clcon is alpha quality software. Before trying to use, please take a [Demo Tour](demo-tour.md). 
You'll see that some features indeed work at some cases. 

I use clcon to develop itself, but editing files is rather dangerous. Editor can hang up at any moment. 
So it is easy to lose your work. 

Windows: installing file release
----------
- Download file release from [Downloads page](https://bitbucket.org/budden/clcon/downloads)
- Unpack it to c:\clcon
- Start c:\clcon\bin\clcon-server-and-client.cmd - this will load and run the IDE. When running for the first time, building will take a while. Subsequent loads are faster
- To see what clcon can do, take a [Demo Tour](demo-tour.md)
- If all is ok, you have clcon with SBCL (use Alt-. to find sources of SBCL objects) and quicklisp (use ql:quickload to load libraries)
- Repositories of clcon components (clcon, oduvanchik, budden-tools) are at c:\clcon\quicklisp\local-projects - feel free to send patches :)

Linux: 
-----------

### Prerequisites
- [SBCL](http://www.sbcl.org/platform-table.html) >= 1.2.7. Check your sbcl version with `(lisp-implementation-version)`
- [tcl/tk](http://tcl.tk) >= 8.6.2. To check your tcl tk version start wish from the console and type in in the console: `info patchlevel`. If your distribution have earlier version, try Tombert's tcltk, see http://wiki.tcl.tk/668, or something else.
- [quicklisp](https://www.quicklisp.org/beta/) set up at your SBCL
- [slime](https://common-lisp.net/project/slime/) must be known to your quicklisp (do `(ql:quickload :slime)` once), but not loaded in your initialization files (e.g. your ~/.sbclrc)
- fresh version of [budden-tools](https://bitbucket.org/budden/budden-tools). Put it under local-projects directory of quicklisp, e.g. (for Linux)
- setting of `sb-impl::*default-external-format*` to `:utf-8` in your initialization file. 
- hg (Mercurial) `sudo apt-get install mercurial`
- budden-tools: 
    `
    cd ~/quicklisp/local-projects
    hg clone https://bitbucket.org/budden/budden-tools
    `  

### Installation
- choose good version of clcon. You can load trunk, but it is better to load latest tagged "release", e.g. 0.3.0. Note that online documentation usually describes trunk. To be sure that it matches software functionality, read not the online documentation, but documentation you have installed. 
- find where your quicklisp's local-projects are located. Simplest way to do it is to start sbcl and type in: `ql:*local-project-directories*`. It will print list of local projects directories, choose any of these. Usually it is `~/quicklisp/local-projects`.
- decide where you will locate clcon. You can put it just to local-projects directory, but you can also put it to a more convenient place and make a symlink to its directory at the local-projects dir with `ln -s`. The same goes to oduvanchik. 
- download chosen releases of oduvanchik and clcon to chosen directory, e.g.
    `
    cd ~/quicklisp/local-projects
    hg clone -u 0.3.0 https://bitbucket.org/budden/oduvanchik
    hg clone -u 0.3.0 https://bitbucket.org/budden/clcon
    `


### Attempt to build and start server: 
    `
    sbcl --load ~/quicklisp/local-projects/clcon/load-clcon-server-linux.lisp
    `
    If all is ok, you will see 
   `
   ;; Swank started at port: 4009.
   ;; Swank started at port: 4005.
   Waiting for oduvanchik to start.
   *
   `

### Start client
   
   `~/quicklisp/local-projects/clcon/clcon.tcl`
    
### Demo tour
To see what clcon can do, take a [Demo Tour](demo-tour.md)
