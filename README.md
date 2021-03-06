# clcon - среда разработки для Common Lisp, [языка Яр](https://bitbucket.org/budden/yar) и tcl/tk

English version is not develped anymore, but it can be downloaded from https://bitbucket.org/budden/clcon_en . 

## О языках

clcon находится в процессе русификации. Часть документов может быть на Английском языке. 
﻿
## Что это? 

**clcon** - это кросс-платформенная среда разработки для Common Lisp, [нового языка Яр](https://bitbucket.org/budden/yar) и tcl/tk. Текущий релиз - 0.3.9 (см. теги в репозитории). 

## Замечание о версиях

Если вы читаете этот документ в интернете, он относится к головной ревизии, а не к релизу. 
Документация к релизу находится в c:/clcon/lp/clcon/readme.md после установки, либо в аналогичном месте под Unix. 

## Поддерживаемые платформы

Поддерживаемая реализация CL: 

- SBCL 1.3.18

Поддерживаемые версии Tcl:

- 8.6.3, 8.6.4. Под Windows используется 8.6.6 с патчем для исправления проблемы клавиатуры, http://core.tcl.tk/tk/tktview?name=720879afe9 . С патчем собран только отладочный wish86tg. 

Тестируется на платформах:

- Windows 10 (64 bit) 
- Windows 7 (64 bit) - когда-то тестировался
- Windows XP SP3 (32 bit) - когда-то тестировался
- Ubuntu 16.04 

## Видео

[См. сюда](https://www.youtube.com/watch?v=nMhwvZ56jHU)

## Снимки экрана

[См. сюда](https://bitbucket.org/budden/clcon/wiki/Screenshots)

## Цели проекта

- IDE под пермиссивной лицензией для встраивания CL или языков на платформе CL в приложения

## Текущее состояние

См [Новости](doc/NEWS.md)

Clcon находится в стадии "альфа". Я использую его как единственную среду разработки для программ на CL, в аккуратных руках его автора он работает приемлемо. Однако разного рода проблемы имеют место. 

Clcon включает в себя большинство возможностей, нужных для эффективной разработки под Common Lisp. Но кое-чего пока не хватает.


Если вы хотите попробовать использовать clcon, начните с [демонстрационного сеанса](doc/demo-tour.md).

## Реализованные возможности

- Не требует конфигурации (под Windows). Скачайте один архив, распакуйте и работайте
- Командная строка (REPL) для CL и Tcl с историей команд
- Продолжение имени, переход к определению, поиск имени по подстоке для CL
- Интернет-справка для стандартных функций CL и для некоторых библиотек
- Продолжение имени, переход к определению, поиск имени по подстоке для Tcl (с ограничениями)
- Поиск в файлах
- Отладчик, пошаговое выполнение, инспектор из SWANK 
- Редактор с закладками, подсветка синтаксиса лиспа, автоотступы, хождение по структуре лисповых форм
- Компиляция отдельных файлов лиспа из среды разработки, просмотр ошибок компиляции с переходом к определению
- Вывод списка определений в файле для языков CL и tcl (с ограничениями)
- Поиск места, где прочитан код, который интерпретируется (этого нет в EMACS)
- Возможность "за одно нажатие" перейти к файлу, который вызвал ошибку компиляции в asdf
- Поддержка конструкции (in-readtable) в исходных файлах
- Генерация Markdown 

## Установка и запуск

clcon распространяется в составе Яра. [Скачивайте Яр](https://bitbucket.org/budden/yar), он доступен для Windows и Linux. 

Обратите внимание, что точные инстукции по установке релиза находятся в самом релизе, а не на данном сайте.

## Документация

**Предупреждение!** Документация на сайте описывает головную версию и может отличаться от документации в релизе. 
**Читайте документацию из релиза, а не документацию на данном сайте**!

[Демонстрационный сеанс (головная версия)](doc/demo-tour.md) - демонстрирует основные возможности среды.

[Руководство пользователя (головная версия)](doc/user-manual.md) 

[Wiki](https://bitbucket.org/budden/clcon/wiki/) - содержит [снимки экрана](https://bitbucket.org/budden/clcon/wiki/Screenshots), но больше в ней ничего полезного нет.  

[ЧАВО](https://bitbucket.org/budden/clcon/ит/default/doc/FAQ.md)

[Каталог документации, включающий некоторые из выше перечисленных документов](https://bitbucket.org/budden/clcon/ит/default/doc/)

## Лицензия

На отдельные части clcon распространяется лицензия MIT, BSD, Public Domain или Artistic License. Clcon в целом - (С) Денис Будяк 2015-2016, лицензия MIT

## Планы

- развивать режим работы для Яра
- русифицировать всю среду
- повышать качество (стараться, чтобы количество ошибок хотя бы не возрастало)
- сделать, чтобы консоль и редактор работали с лиспом одинаково (сейчас есть различия)
- сделать автодополнение команд среды
- исправить баги в режиме Markdown 
- интеграция с quickdocs
