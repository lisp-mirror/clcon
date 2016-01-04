Clcon 0.3.5 - Установка и запуск
==============

Warning
-------
Clcon is alpha quality software. Before trying to use, please take a [Demo Tour](demo-tour.md). 
You'll see that some features indeed work at some cases. 

I use clcon to develop itself, but editing files is rather dangerous. Editor can hang up at any moment. 
So it is easy to lose your work. 

Windows: installing file release
----------
Download file release from [Downloads page](https://bitbucket.org/budden/clcon/downloads)

Unpack it to c:\clcon

Start c:\clcon\bin\clcon-server-and-client.cmd - this will load and run the IDE. When running for the first time, building will take a while. Subsequent loads are faster

To see what clcon can do, take a [Demo Tour](demo-tour.md)

If all is ok, you have clcon with SBCL (use Alt-. to find sources of SBCL objects) and quicklisp (use ql:quickload to load libraries)

Repositories of clcon components (clcon, oduvanchik, budden-tools) are at c:\clcon\lp - feel free to send patches :)

Linux: ПРОЦЕДУРА УСТАНОВКИ НЕ ТЕСТИРОВАНА В ДАННОЙ ВЕРСИИ at 0.3.5!
-----------

### Prerequisites

#### Mercurial (aka hg)

    sudo apt-get install mercurial

#### Git
   
    sudo apt-get install git

#### SBCL

[SBCL](http://www.sbcl.org/platform-table.html) >= 1.2.7. To check your SBCL version, call `sbcl --version` from the shell, or `(lisp-implementation-version)` from SBCL prompt.

#### Tcl/tk

[Tcl/tk](http://tcl.tk) >= 8.6.2. To check your tcl tk version start `wish` from the console and type in in the console: `info patchlevel`. If your distribution have earlier version, try Tombert's tcltk, see http://wiki.tcl.tk/668, or something else.

#### Quicklisp

[Quicklisp](https://www.quicklisp.org/beta/). Clcon is now based on patched SLIME and NAMED-READTABLES so you might want to install another copy of Qucklisp if you have one. If you have no Quicklisp, download it, but do not set up SBCL to use it. On my machine, quicklisp is at ~/ql.sbcl.l, and it has ~/ql.sbcl.l/local-projects directory which we will use.  

#### Decide clcon version

You can try trunk, but it is better to load latest tagged "release", 0.3.4. Note that online documentation usually describes trunk. To be sure that it matches software functionality, read not the online documentation, but documentation you have installed. 

#### Lisp libraries, part 1 and clcon components

    cd /s2/lib/ql.sbcl.l/local-projects
    hg clone https://bitbucket.org/budden/budden-tools
    git clone https://github.com/budden/slime 
    git clone https://github.com/budden/named-readtables
    # to download clcon's trunk, remove '-u 0.3.5' 
    hg clone -u 0.3.5 https://bitbucket.org/budden/oduvanchik
    hg clone -u 0.3.5 https://bitbucket.org/budden/clcon

#### Lisp libraries, part 2

hyperdoc, hyperspec-lookup, toposort: I patched two of these. The only way
to get them is to download windows file release and get clcon/lp/third-party directory. Extract this directory as /s2/lib/ql.sbcl.l/local-projects/third-party .

#### Refreshing system-index for quicklisp: 
    cd /s2/lib/ql.sbcl.l/local-projects
    rm system-index.txt 

### Build and start server

    sbcl --no-userinit --load ~/ql.sbcl.l/local-projects/clcon/load-clcon-server-linux.lisp

If all is ok, you will see something like:

    ;; Swank started at port: 4009.
    Waiting for oduvanchik to start.
    * 

### Start client
   
    chmod u+x ~/ql.sbcl.l/local-projects/clcon/clcon.tcl
    ~/ql.sbcl.l/local-projects/clcon/clcon.tcl

### EMACS

  When something goes wrong, EMACS is still useful. See 

   https://bitbucket.org/budden/clcon/wiki/Portion%20of%20my%20.emacs%20relevant%20to%20clcon

### Demo tour

To see what clcon can do, take a [Demo Tour](demo-tour.md)
