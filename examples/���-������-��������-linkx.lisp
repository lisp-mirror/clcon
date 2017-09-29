; -*- coding: utf-8; -*-
; (C) Денис Будяк 2017
;; Скрипт
;;  - создаёт таблицу чтения :linkx-readtable
;;  - загружает систему :linkx-tests
;;  - по ходу дела перегенерирует пакет с импортами тестов, :linkx-tests-imports
;; Требует среды Яр для исполнения и полагается на то, что linkx находится в поддиректории "v"
;; относительно корня Яра
(named-readtables:in-readtable nil)

(def-merge-packages::! :linkx-loader
                       (:always t)
  (:import-from :budden-tools budden-tools:perga budden-tools:_f)
  (:use :cl)
  (:intern #:ЗАГРУЗИ-LINKX-И-СГЕНЕРИРУЙ-ИНТЕРФЕЙСНЫЙ-ПАКЕТ-ДЛЯ-ТЕСТОВ))

(in-package :linkx-loader)

(decorate-function:portably-without-package-locks
 (defun :linkx-readtable () "Помечаем место, где определена таблица чтения :linkx-readtable, для навигации по исходникам" (error "Это не функция")))

(defun Устрой-Linkx-readtable ()
  (when (named-readtables:find-readtable :linkx-readtable)
    (BUDDEN-TOOLS::reset-to-standard-readtable :linkx-readtable))
  (named-readtables:defreadtable :linkx-readtable 
    (:merge :standard))
  ;; это нужно для того, чтобы автоматически получать списки импортируемых символов. В 
  ;; промышленной версии не нужно
  (budden-tools::ENABLE-BUDDENS-READTABLE-EXTENSIONS :linkx-readtable
                                                     ;:paren-reader-with-closing-paren-notification nil
                                                     ))

(Устрой-Linkx-readtable)

(defparameter *Неинтересные-пакеты* '(:common-lisp :alexandria.0.dev :keyword :linkx-tests)
  "Здесь должны быть перечислены пакеты, к-рые linkx использует через use и т.п.")

(defstruct Импорты-из-одного-пакета
  Пакет
  Имена-символов ; список
  )

(defun Загрузи-linkx-и-сгенерируй-интерфейсный-пакет-для-тестов ()
  "Загружает систему тестов и смотрит, какие символы были прочитаны из заданных пакетов"
  (perga
   (let Предложения-импорта nil)
   (ql:quickload :linkx)
   (asdf:find-system :linkx-tests)
   (perga
    (let budden-tools:*fn-before-return-symbol-from-reader-hook*
      (lambda (Символ Поток)
        (perga
         (let Имя-файла (ignore-errors (pathname Поток)))
         (when
             (and Имя-файла
                  (string= (namestring (budden-tools:path-to-a-file Имя-файла))
                           (namestring (cl-user::putq-otnositelqno-kornya-yara "v/t/"))))
           (_f
            Добавить-в-предложения-импорта
            Предложения-импорта
            Символ))
           Символ)))
    (let *readtable* (named-readtables:find-readtable :linkx-readtable))
    (asdf:test-system :linkx :force t))
   (let Предл-имп-для-defpackage (Подготовить-предложения-импорта-к-печати Предложения-импорта))
   (:@ with-open-file (П (cl-user::putq-otnositelqno-kornya-yara "v/t/package-linkx-dependencies.lisp")
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede
                         )
       (format П ";;; -*- coding: utf-8; system: linkx-tests; -*-~%
;; Файл может быть автогенерирован из ~S на базе исходных текстов тестов.
;; Он не играет никакой роли в коде и нужен только для декларации символов, используемых в тестах
~{~&~S~}~%
  " #.(or *compile-file-truename* *load-truename*) Предл-имп-для-defpackage))))


(defun Добавить-в-предложения-импорта (Предложения-импорта Символ)
  (perga
   (let Пакет (symbol-package Символ))
   (when Пакет
     (unless (member (budden-tools:keywordize (package-name Пакет)) *Неинтересные-пакеты*)
       (let Вхождение (find Пакет Предложения-импорта :key 'Импорты-из-одного-пакета-Пакет))
       (cond
        (Вхождение
         (pushnew (string Символ)
                  (Импорты-из-одного-пакета-Имена-символов Вхождение) :test 'string=))
        (t
         (let Вхождение (make-Импорты-из-одного-пакета
                         :Пакет Пакет
                         :Имена-символов (list (string Символ))))
         (push Вхождение Предложения-импорта))))
     Предложения-импорта)))

(defun Подготовить-предложения-импорта-к-печати (Предложения-импорта)
  (perga
   (let Рез nil)
   (let Пакеты-импорта nil)
   (_f sort Предложения-импорта
       'string< :key (lambda (ц) (package-name (Импорты-из-одного-пакета-Пакет ц))))
   (dolist (Ииоп Предложения-импорта)
     (let Имена (copy-list (Импорты-из-одного-пакета-Имена-символов Ииоп)))
     (_f sort Имена 'string<)
     (let Имя-пакета (package-name (Импорты-из-одного-пакета-Пакет Ииоп)))
     (let Имя-пакета-импорта (alexandria:format-symbol :keyword "IMPORTS--LINKX--FROM--~A" Имя-пакета)) 
     (push `(defpackage ,Имя-пакета-импорта
                         (:use)
                         (:import-from 
                         ,(budden-tools:keywordize Имя-пакета)
                         ,@(mapcar 'make-symbol Имена)))
           Рез)
     (push Имя-пакета-импорта Пакеты-импорта))
   (_f nreverse Пакеты-импорта)
   (push `(defpackage :linkx-dependencies (:use ,@Пакеты-импорта)) Рез)
   (nreverse Рез)))

(clco:eval-in-tcl "::tkcon::SendEventToSwank {
                   (progn
                     (named-readtables:in-readtable nil)
                     (uiop/package:symbol-call :linkx-loader \"ЗАГРУЗИ-LINKX-И-СГЕНЕРИРУЙ-ИНТЕРФЕЙСНЫЙ-ПАКЕТ-ДЛЯ-ТЕСТОВ\")
                     (named-readtables:in-readtable :linkx-readtable)
                   )} {}
                   ")
