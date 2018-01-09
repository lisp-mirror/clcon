Clcon 0.3.6 - Установка и запуск
==============

##  Этот документ мало актуален

clcon больше не устанавливается отдельно - он теперь входит в [проект Яр](https://bitbucket.org/budden/yar). Здесь остаётся информация, нужная
для сборки clcon "с нуля". 

### Версии

#### SBCL

[SBCL](http://www.sbcl.org/platform-table.html) >= 1.2.7. To check your SBCL version, call `sbcl --version` from the shell, or `(lisp-implementation-version)` from SBCL prompt.

#### Tcl/tk

[Tcl/tk](http://tcl.tk) >= 8.6.2. To check your tcl tk version start `wish` from the console and type in in the console: `info patchlevel`. If your distribution have earlier version, try Tombert's tcltk, see http://wiki.tcl.tk/668, or something else.

#### Lisp libraries, part 1 and clcon components

    cd /y/yar/lp
    hg clone https://bitbucket.org/budden/budden-tools
    git clone https://github.com/budden/slime 
    git clone https://github.com/budden/named-readtables
    hg clone https://bitbucket.org/budden/oduvanchik
    hg clone https://bitbucket.org/budden/clcon

#### Lisp libraries, part 2

hyperdoc, hyperspec-lookup, toposort: I patched two of these. The only way
to get them is to download windows file release and get yar/lp/third-party directory. 

### EMACS

  When something goes wrong, EMACS is still useful. See 

   https://bitbucket.org/budden/clcon/wiki/Portion%20of%20my%20.emacs%20relevant%20to%20clcon

