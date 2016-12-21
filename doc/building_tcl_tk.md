Этот файл устарел, см. [постройка-tcl-tk-8.6.6.md](постройка-tcl-tk-8.6.6.md)

BUILDING tcl/tk 8.4.6:

copies lib*.a to *.lib (don't know if it will fail)

http://wiki.tcl.tk/14828

c:\mingw\msys\1.0\msys.bat

export PATH=$PATH:/c/MinGW/bin

cd d:/tcktk8.6.4/tcl8.6.4/win

configure --prefix=/c/tcl

building tk: 

missing shobjidl.h - 

removed '#include <shobjidl.h>' from tk sources. 

then it requried snit. Donwloading tcllib 1.17...
c:\MinGW\msys\1.0\opt\tcl\bin\wish86.exe c:\setup\tcllib-1.17\installer.tcl - worked ok. 

tablelist and wcb - just downloaded and put to lib library. 


Alternatives are listed at https://www.linux.org.ru/forum/development/12058824


Then just copied under our tree.
