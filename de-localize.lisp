
(def-merge-packages::! :DE-LOCALIZE-CLCON (:use :cl :budden-tools)
  (:always t))

(in-package :de-localize-clcon)

(defparameter *translations*
  '(
    ("1.Файл 2.Правка 3.Консоль 4.Настройка 5.История 6.Избранное 7.Окно 8.Справка"
     "1.File 2.Edit 3.REPL 4.Settings 5.History 6.Favorites 7.Window 8.Help")
    (".файл " ".file ")
    (".правка " ".edit ")
    (".правка]" ".edit]")
    (".консоль " ".repl ")
    (".настройка " ".settings ")
    (".настройка]" ".settings]")
    (".история " ".history ")
    (".история]" ".history]")
    (".избранное " ".favorites ")
    (".избранное]" ".favorites]")
    (".окно" ".window")
    (".справка " ".help ")
    (".справка]" ".help]")
    ("Создать и редактировать файл" "Edit new file")
    ("Открыть для редактирования" "Open for edit")
    ("Выполнить файл Tcl" "Run Tcl script")
    ("Показать последнюю ошибку Tcl" "Show last Tcl error")
    ("Сохранить поток консоли..." "Save output...")
    ("4.Перезагрузить часть исходных текстов ИСР (IDE)" "4.Reload some of IDE sources")
    ("Очистить кеш asd-систем в quicklisp" "Delete quicklisp's ASDF system index")
    ("5.Открыть недавний..." "5.Open recent...")
    ("Выход из лиспа и ИСР" "Quit lisp and IDE")
    ("6.Выход только из ИСР" "Quit IDE only")
    ("Консоль - $title" "Console - $title")
    ("1.Подключиться к текстовому лисп-серверу SWANK" "1.Attach SWANK server")
    ("2.Отключиться от текстового лисп-сервера SWANK" "2.Detach SWANK server")
    ("3.Принудительно остановить подключенный лисп-сервер SWANK" "3.Quit currently attached SWANK server image")
    ("4.Очистить консоль" "4.Clear console")
    ("О программе" "About")
    ("Поиск идентификатора в Common Lisp Hyperdoc" "Common Lisp Hyperdoc lookup")
    ("1.Руководство clcon" "clcon manual")
    ("6.Сайт по tcl/tk (англ)" "tcl.tk website")
    ("Лисп: автодополнение" "Lisp: complete symbol")
    ("Лисп: перейти к определению" "Lisp: goto definition")
    ("Tcl: автодополнение" "Tcl: complete symbol")
    ("Tcl: перейти к определению" "Tcl: goto proc definition")
    ("м. Маленький шрифт" "v. Small font")
    ("с. Средний шрифт" "c. Average font")
    ("б. Большой шрифт" "b. Big font")
    ("Список окон редактора" "Editor Buffer List")
    ("Консоль" "REPL")
    ("Редактор" "Editor")
    ("Отладчик лиспа" "Lisp Debugger")
    ("Список ошибок" "Compilation errors")
    ("Потоки" "Threads")
    
    ))


(defparameter *files*
  '("c:/yar/lp/clcon/clcon.tcl"
    "c:/yar/lp/clcon/window_menu.tcl"))

(defun do-all ()
  (perga
   (dolist (pair *translations*)
     (let from (first pair))
     (let to (second pair))
     (dolist (file *files*)
       (editor-budden-tools::replace-string-in-file file from to :all t)))))

(do-all)
