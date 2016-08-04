Как я апгрейдил SBCL.
1. Скачиваем 32-разрядный двоичный SBCL и исходники. При установке SBCL отказываемся от установки переменных окружения. 
2. Копируем файлы SBCL и контрибы в c:\yar\sbcl\1.3.4 (у вас будет другая версия)
3. Архив исходников распаковываем в c:\yar\sbcl\1.3.4\source (файл README должен лечь в c:\yar\sbcl\1.3.4\source\README
4. Все c:\yar\sbcl\1.3.4\contrib\*.fasl, кроме asdf.fasl и uiop.fasl, ставим с признаком "только для чтения"
5. Копируем из c:\yar\sbcl\1.3.0\ в 1.3.4 следующие файлы:

.sbclrc
libeay32.dll
libssl32.dll
ssleay32.dll
       
в sbclrc обновляем пути. 

6. В c:\yar\bin\*.cmd меняем версию SBCL (поиск с заменой).
7. c:\yar\bin\util\clear-asdf-cache.cmd
8. Заново копируем uiop.fasl, asdf.fasl
9. Строим образ Яр и запускаем


