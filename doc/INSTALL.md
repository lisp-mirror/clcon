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
Download file release from [Downloads page](https://bitbucket.org/budden/clcon/downloads)

Unpack it to c:\clcon

Start c:\clcon\bin\clcon-server-and-client.cmd - this will load and run the IDE. When running for the first time, building will take a while. Subsequent loads are faster

To see what clcon can do, take a [Demo Tour](demo-tour.md)

If all is ok, you have clcon with SBCL (use Alt-. to find sources of SBCL objects) and quicklisp (use ql:quickload to load libraries)

Repositories of clcon components (clcon, oduvanchik, budden-tools) are at c:\clcon\lp - feel free to send patches :)

Linux: 
-----------
I sometimes need EMACS to check what is the right client/server dialog. I forked SLIME and enabled it to understand readtables in the file,
see https://github.com/budden/slime. But this breaks my current installed version of SLIME. So I suspend further development on Linux. 
I hope my pull request will be accepted to SLIME's trunk, in which case I'll restore installation procedure for Linux. 
Until that time, you can download repo at older 0.3.0 version and read installation instruction for that version inside your clone of the repo.
    
### Demo tour

To see what clcon can do, take a [Demo Tour](demo-tour.md)
