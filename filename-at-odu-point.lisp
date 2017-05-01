;; -*- coding: utf-8 ; Encoding: utf-8 ; system :clcon-server ; -*-
;; Extracting symbol from oduvanchik's point
(in-package :oduvanchik)
(named-readtables::in-readtable :oduvanchik-ext-readtable)


(defun БУКВА-КОТОРУЮ-СЧИТАЕМ-ГРАНИЦЕЙ-ИМЕНИ-ФАЙЛА (ч)
  (member ч '(#\  #\" #\< #\> #\( #\))))

(defun РАСПОЗНАТЬ-ФРАГМЕНТ-В-ТЕКУЩЕЙ-СТРОКЕ-РЕДАКТОРА-ОКОЛО-ТЕКУЩЕЙ-ТОЧКИ-ОГРАНИЧЕННЫЙ-ПРЕДИКАТАМИ (ПРЕДИКАТ-НАЧАЛА ПРЕДИКАТ-КОНЦА ГДЕ-ДОЛЖНЫ-СТОЯТЬ)
  "Сделано по образцу odu::get-symbol-from-current-point, но обобщить не выйдет, 
   потому что там мы контролируем длину и читаем ридером, а здесь туда и обратно читаем лексером.  
   ГДЕ-ДОЛЖНЫ-СТОЯТЬ =
   0 - должны стоять внутри
   1 - можем стоять внтри или сзади от искомого фрагмента
   2 - мы должны стоять ровно в конце фрагмента, иначе ругаемся"
  (perga-implementation:perga
   function
   (let ТОЧКА (odu::current-point))
   (let ТОЧ1 (odu::copy-mark ТОЧКА :temporary))
   (let НАЧАЛО-СТРОКИ (odu::copy-mark ТОЧКА :temporary))
   (oi::line-start НАЧАЛО-СТРОКИ)
   (let УЖЕ-ПРОВЕРИЛИ-ГДЕ-МЫ-ДОЛЖНЫ-ИЗНАЧАЛЬНО-СТОЯТЬ nil)
   (let НАЧАЛО-ФРАГМЕНТА nil)
   (let Ы-ВНУТРИ nil) ; истина, когда внутри фрагмента
   (let ТЕК-ВНУТРИ nil) ; истинка, когда текущая буква - внутри фрагмента
   (flet БУКВА-МОЖЕТ-БЫТЬ-ВНУТРИ-Ф (Б)
     (and (not (funcall ПРЕДИКАТ-НАЧАЛА Б))
          (not (funcall ПРЕДИКАТ-КОНЦА Б))))
   (do ()
       ((not (odu::mark> ТОЧ1 НАЧАЛО-СТРОКИ)) nil)
     (let ПРЕДЫДУЩАЯ-БУКВА (odu::previous-character ТОЧ1))
     (setq ТЕК-ВНУТРИ (funcall #'БУКВА-МОЖЕТ-БЫТЬ-ВНУТРИ-Ф ПРЕДЫДУЩАЯ-БУКВА))
     (unless УЖЕ-ПРОВЕРИЛИ-ГДЕ-МЫ-ДОЛЖНЫ-ИЗНАЧАЛЬНО-СТОЯТЬ
       (let ГДЕ-МЫ-ЕСТЬ
         (where-is-mark-relative-to-symbol
          ТОЧ1
          :ПРЕДИКАТ #'БУКВА-МОЖЕТ-БЫТЬ-ВНУТРИ-Ф))
       (cond
        ((and (= ГДЕ-ДОЛЖНЫ-СТОЯТЬ 2)
              (/= ГДЕ-МЫ-ЕСТЬ 0))
         (bell-with-tcl)
         (return-from function (values "" nil))))
       (setq УЖЕ-ПРОВЕРИЛИ-ГДЕ-МЫ-ДОЛЖНЫ-ИЗНАЧАЛЬНО-СТОЯТЬ t))
     (when ТЕК-ВНУТРИ
       (unless Ы-ВНУТРИ
         (setf Ы-ВНУТРИ t)))
     (when Ы-ВНУТРИ
       (unless ТЕК-ВНУТРИ
         (setf НАЧАЛО-ФРАГМЕНТА (odu::copy-mark ТОЧ1 :temporary))
         (return nil)))
     (odu::character-offset ТОЧ1 -1)
     )
   (cond
    ((not НАЧАЛО-ФРАГМЕНТА)
     ; ничего нет
     (return-from function (values "" nil)))
    (t
     (Р-Ф-В-Е-С-Р-О-Т-Е-О-П-2 НАЧАЛО-ФРАГМЕНТА ТОЧКА ПРЕДИКАТ-КОНЦА)))))

(defun Р-Ф-В-Е-С-Р-О-Т-Е-О-П-2 (НАЧАЛО-ФРАГМЕНТА ТОЧКА ПРЕДИКАТ-КОНЦА)
  (perga-implementation:perga
   ; переходим на английский из-за недостатка времени (копипастим из get-symbol-from-current-point-part-2
   (let lookup-end (odu::copy-mark НАЧАЛО-ФРАГМЕНТА :temporary))
   (let КОНЕЦ-СТРОКИ (odu::copy-mark ТОЧКА :temporary))
   (odu::line-end КОНЕЦ-СТРОКИ)
   ; Теперь нужно найти конец символа. Для нашей - просто вырезать кусок максимально возможной длины, т.к. мы будем читать символ ридером
   (do () ((not (odu::mark< lookup-end КОНЕЦ-СТРОКИ)) nil)
     (when (funcall ПРЕДИКАТ-КОНЦА (odu::next-character lookup-end))
       (setf lookup-end (oi::copy-mark lookup-end :temporary))
       (return))
     (odu::character-offset lookup-end 1)
     )
   (let ss (clco::string-between-marks НАЧАЛО-ФРАГМЕНТА lookup-end))
   ss))
   

(defun get-file-name-from-current-point ()
  "Функция для упрощения апропоса, нигде не вызывается"
  (РАСПОЗНАТЬ-ИМЯ-ФАЙЛА-В-ТЕКУЩЕЙ-ТОЧКЕ-РЕДАКТОРА-ИЛИ-ПЕРЕД-НЕЙ))


(defun ОПРЕДЕЛИТЬ-РАСШИРЕНИЕ-ОТКРЫВАЕМОГО-ФАЙЛА-ПО-УМОЛЧАНИЮ-ПО-РАСШИРЕНИЮ-ДАННОГО-ФАЙЛА (РАСШИРЕНИЕ-ДАННОГО-ФАЙЛА)
  (let ((Ш РАСШИРЕНИЕ-ДАННОГО-ФАЙЛА))
  (cond
   ((string= Ш "asd") "lisp")
   (t Ш))))
     

(defun РАСПОЗНАТЬ-ИМЯ-ФАЙЛА-В-ТЕКУЩЕЙ-ТОЧКЕ-РЕДАКТОРА-ИЛИ-ПЕРЕД-НЕЙ ()
  "Пытаемся выкусить имя файла из текущей точки. Имя файла текущего буфера в лиспе недоступно (или информация о том, где оно, утеряна), поэтому нам его должны прислать из tcl. Возвращает список (признак-наличия-файла имя-файла). Признак наличия:
 0 - под курсором нет ничего похожего на имя файла
 1 - имя файла есть, но файла такого нет
 2 - есть имя и есть файл с таким имененем
ПРАВМЯ по-хорошему, нужно искать файл в наборе путей, который как-то зависит от типа файла, например, что будет, если нас просят открыть stdio.h? "
  (perga-implementation:perga
   (let РАСПОЗНАННОЕ-ИМЯ
     (РАСПОЗНАТЬ-ФРАГМЕНТ-В-ТЕКУЩЕЙ-СТРОКЕ-РЕДАКТОРА-ОКОЛО-ТЕКУЩЕЙ-ТОЧКИ-ОГРАНИЧЕННЫЙ-ПРЕДИКАТАМИ
      #'БУКВА-КОТОРУЮ-СЧИТАЕМ-ГРАНИЦЕЙ-ИМЕНИ-ФАЙЛА
      #'БУКВА-КОТОРУЮ-СЧИТАЕМ-ГРАНИЦЕЙ-ИМЕНИ-ФАЙЛА
      1))
   (let ПУТЬ--LINUX--К-ФАЙЛУ-ТЕКУЩЕГО-БУФЕРА (oi:buffer-pathname (oi:current-buffer)))
   (cond
    ((and РАСПОЗНАННОЕ-ИМЯ (not (string= "" РАСПОЗНАННОЕ-ИМЯ)))
     (let РАСШИРЕНИЕ-ФАЙЛА-ТЕКУЩЕГО-БУФЕРА (pathname-type ПУТЬ--LINUX--К-ФАЙЛУ-ТЕКУЩЕГО-БУФЕРА))
     (let РАСШИРЕНИЕ-ОТКРЫВАЕМОГО-ФАЙЛА-ПО-УМОЛЧАНИЮ
       (ОПРЕДЕЛИТЬ-РАСШИРЕНИЕ-ОТКРЫВАЕМОГО-ФАЙЛА-ПО-УМОЛЧАНИЮ-ПО-РАСШИРЕНИЮ-ДАННОГО-ФАЙЛА РАСШИРЕНИЕ-ФАЙЛА-ТЕКУЩЕГО-БУФЕРА))
     (let ИМЯ-С-РАСШИРЕНИЕМ
       (merge-pathnames РАСПОЗНАННОЕ-ИМЯ
                        (make-pathname :host nil :device nil :directory nil
                                       :name nil :type РАСШИРЕНИЕ-ОТКРЫВАЕМОГО-ФАЙЛА-ПО-УМОЛЧАНИЮ)))
     (let РЕЗУЛЬТАТ-БЕЗ-КАНОНИКАЛИЗАЦИИ 
          (merge-pathnames ИМЯ-С-РАСШИРЕНИЕМ
                           ПУТЬ--LINUX--К-ФАЙЛУ-ТЕКУЩЕГО-БУФЕРА))
     (let РЕЗУЛЬТАТ (uiop/filesystem:native-namestring РЕЗУЛЬТАТ-БЕЗ-КАНОНИКАЛИЗАЦИИ))
     (list (if (probe-file РЕЗУЛЬТАТ) 2 1)
           (namestring РЕЗУЛЬТАТ))
     )
    (t (list 0 "")) 
    )))
