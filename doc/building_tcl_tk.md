
BUILDING tcl/tk 8.4.6:

http://wiki.tcl.tk/14828

export PATH=$PATH:/c/MinGW/bin

building tk: 

missing shobjidl.h - 

removed '#include <shobjidl.h>' from tk sources. 

then it requried snit. Donwloading tcllib 1.17...
c:\MinGW\msys\1.0\opt\tcl\bin\wish86.exe c:\setup\tcllib-1.17\installer.tcl - worked ok. 

tablelist and wcb - just downloaded and put to lib library. 


Alternatives are listed at https://www.linux.org.ru/forum/development/12058824


Then just copied under our tree.