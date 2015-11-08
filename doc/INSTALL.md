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
Read carefully the following:

 ../load-clcon-server-linux.lisp
 
 https://bitbucket.org/budden/clcon/wiki/Portion%20of%20my%20.emacs%20relevant%20to%20clcon

Then adapt for your needs. If something is wrong, feel free to contact me (budden73@gmail.com)
    
### Demo tour

To see what clcon can do, take a [Demo Tour](demo-tour.md)
