Этот файл не md, а просто txt. Просто он сделан с расширением md, потому что команда fics не ищет в файлах txt, а ищет только в md. Извините :) 

Собираем(строим,компилируем) tcl/tk 8.6.6:

Сборка через Visual Studio Express
==================================

Официально http://www.tcl.tk/doc/howto/compile.html

требуется собирать через Visual Studio. Качаем Visual Studio Express 2008 отсюда:
http://download.microsoft.com/download/E/8/E/E8EEB394-7F42-4963-A2D8-29559B738298/VS2008ExpressWithSP1ENUX1504728.iso

Устанавливаем g:\VCExpress
Распаковываем в  c:\yar\tcl-8.6.6\build\tcl8.6.6
cmd
cd /d c:\yar\tcl-8.6.6\build\tcl8.6.6\win
"c:\Program Files (x86)\Microsoft Visual Studio 9.0\VC\bin\vcvars32.bat"
nmake /f makefile.vc
nmake /f makefile.vc INSTALLDIR=c:\yar\tcl-8.6.6 install

УРА! Теперь tk

Распаковываем в  c:\yar\tcl-8.6.6\build\tk8.6.6
cmd
cd /d c:\yar\tcl-8.6.6\build\tk8.6.6\win
"c:\Program Files (x86)\Microsoft Visual Studio 9.0\VC\bin\vcvars32.bat"
nmake /f makefile.vc TCLDIR=c:\yar\tcl-8.6.6\build\tcl8.6.6 
nmake /f makefile.vc INSTALLDIR=c:\yar\tcl-8.6.6 install

УРА!

tcllib 1.18 (новейшая на данный момент) - ищем гуглом, скачиваем с 
Распаковываем в 
c:\yar\tcl-8.6.6\build\tcllib-1.18
Далее 
cmd и в нём 
c:\yar\tcl-8.6.6\bin\tclsh86t.exe c:\yar\tcl-8.6.6\build\tcllib-1.18\installer.tcl

tablelist и wcb - скачиваем с http://www.nemethi.de/
Копируем в c:\yar\tcl-8.6.6\lib

Меняем путь: look for 8.4.4. everywhere

Боремся с размазанными шрифтами:
c:\yar\tcl-8.6.6\bin\wish86t.exe
правая кнопка/свойства/совместимость/отключить масштабирование изображения при высоком разрешении экрана

Заходим в консоль. 
.edit_initialization_file
tkcon font oemfixed 12

Предыдущая деятельность
=======================================
Деятельность по установке 8.6.4 описана в building_tcl_tk.md

Пытаемся собрать отладочную версию
==================================
Делаем резервную копию всей директории tcl-8.6.6
cmd
cd /d c:\yar\tcl-8.6.6\build\tcl8.6.6\win
"c:\Program Files (x86)\Microsoft Visual Studio 9.0\VC\bin\vcvars32.bat"
nmake /f makefile.vc clea
nmake /f makefile.vc OPTS=symbols
nmake /f makefile.vc OPTS=symbols INSTALLDIR=c:\yar\tcl-8.6.6 install

cd /d c:\yar\tcl-8.6.6\build\tk8.6.6\win
nmake /f makefile.vc OPTS=symbols TCLDIR=c:\yar\tcl-8.6.6\build\tcl8.6.6 
nmake /f makefile.vc OPTS=symbols INSTALLDIR=c:\yar\tcl-8.6.6 install

Меняем скрипт запуска клиента: 


НЕУДАЧНАЯ попытка сборки MinGW
=================================

Инструкции - тут: http://wiki.tcl.tk/14828

1. Устанавливаем MinGW - всё по умолчанию. Дальше выбираем пакет gcc. 
2. Скачиваем https://sourceforge.net/projects/tcl/files/Tcl/8.6.6/ - tcl866-src.zip, tk688-src.zip

Отключаем антивирус. 
3. Раскрываем архивы в c:\MinGW\msys\1.0\src 
c:\mingw\msys\1.0\msys.bat
export PATH=$PATH:/c/MinGW/bin


4. 
mkdir /build
mkdir /build/tcl
# это на самом деле мы создали c:\MinGW\msys\1.0\build\tcl
cd /build/tcl
/src/tcl8.6.6/win/configure --prefix=/opt/tcl
make

Жалуется на uintptr_t. ChangeLog.2007 содержит запись от 2006-11-13  Daniel Steffen  <das@users.sourceforge.net>
Там generic/tclHash.c упоминается в ряду с generic/tclEncoding.c, который успешно компилируется. Можно посмотреть,
в чём разница между этими файлами и попробовать поправить tclHash.c (может быть, не хватает какого-нибудь #include?)

