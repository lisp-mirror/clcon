;; -*- coding: utf-8 ; Encoding: utf-8 ; system :clcon-server ; -*-
;; Extracting symbol from oduvanchik's point
(in-package :oduvanchik)
(named-readtables::in-readtable :oduvanchik-ext-readtable)

(defun where-is-mark-relative-to-symbol (p1 &key (ПРЕДИКАТ 'editor-budden-tools::char-can-be-in-symbol))
  "ПРЕДИКАТ - функция от буквы. Истина, если буква может быть внутри символа.
   0 - mark is just after the end of symbol string
   1 - mark is inside or just after the end of symbol string
   2 - mark is at the beginning of symbol string
   3 - mark is not in symbol string. Syntax info is ignored, we look
   just at the char itself
   "
  (perga-implementation:perga
   (let prev-char (odu::previous-character p1))
   (let prev-in-symbol
         (and prev-char
              (funcall ПРЕДИКАТ prev-char)))
   (let next-char (odu::next-character p1))
   (let next-in-symbol
         (and next-char
              (funcall ПРЕДИКАТ next-char)))
   (cond
    ((and (not prev-in-symbol) (not next-in-symbol))
     3)
    ((and (not prev-in-symbol) next-in-symbol)
     2)
    ((and prev-in-symbol (not next-in-symbol))
     0)
    (t
     1))))


;; ПРАВЬМЯ перед тем, как тут что-то править, отрефакторь костыль с таблицей чтения!
(defun get-symbol-from-current-point (&REST keyargs &KEY (PREVIOUS 1) (MAX-LENGTH 100) (MAX-NON-ALPHANUMERIC 15) (return-symbol-too t) (CREATE-NEW nil))
  "See also odu::symbol-string-at-point . Если мы в нашей таблице чтения, читаем символ с помощью ридера, переключённого в спец. режим. Если с пакетом неясность, ищем все символы с таким именем и предлагаем пользователю выбор. Имеем возможность не создавать символ при попытке забрать его из буфера - это аккуратная политика. 
  Выражение a^b рассматриваем как два символа a и b. 

  create-new - разрешить создавать символ. Если мы не в нашей таблице чтения (null (packages-seen-p...)) , то create-new игнорируется и символ может быть создан

  Возвращает два значения:
  1. Строка символа, к-рая под курсором, даже если мы не знаем такого символа
  2. Если нас просили вернуть символ и под курсором - известный символ, либо нам разрешили его создать, то возвращает найденный или созданный символ. Если мы не знаем такого символа или не в нашей таблице чтения, возвращаем nil
  Если строки символа нет, вернёт (values \"\" nil)

  previous -
    0 мы должны стоять уже внутри символа (возможно, в конце)
    1 мы берём подходящий символ выше по тексту или тот, в котором стоим. 
    2 мы должны стоять в конце символа, иначе пищим.
  "
  (declare (ignore max-non-alphanumeric keyargs))
  (perga-implementation:perga function
    (check-type previous (integer 0 2))
    (let point (odu::current-point))
    (let buf-beg (oi::buffer-start-mark (mark-buffer point)))
    (let buf-end (oi::buffer-end-mark (mark-buffer point)))
    (let readtable (or (named-readtables:find-readtable (odu::readtable-at-point))
                       (named-readtables:find-readtable nil)))
    (let package-designator (odu::package-at-point))
    (let our-readtable
      (cond
       ((budden-tools::packages-seen-p readtable) readtable)
       (t
        (perga-implementation:perga
         (let res (copy-readtable readtable))
         (BUDDEN-TOOLS:ENABLE-BUDDENS-READTABLE-EXTENSIONS res)
         res))))
    

    ;; ПРАВЬМЯ КОСТЫЛЬ - отрефакторить и убрать readtalbe вообще
    (setf readtable our-readtable)

    (let reg (and (region-active-p (current-buffer))
                  (current-region)))
    (cond
     (reg
      (get-symbol-from-current-point-part-2
       (copy-mark (region-start reg) :temporary)
       (length (region-to-string reg))
       buf-end
       our-readtable
       return-symbol-too
       create-new
       package-designator
       readtable
       :invalid ; для выделения точка не должна быть нужна
       :selection-p t))
     (t
      (let p1 (odu::copy-mark point :temporary))
      (let rest-length (or max-length -1))
      (let symbol-beginning nil)
      (let v-in-symbol nil) ; истина, когда внутри символа
      (let cur-in-symbol nil) ; истина, когда текущий char относится к символу
      ; Движемся назад от текущей точки и ищем начало символа, записывая в p1
      (let checked-where-we-must-be-initially nil)
      (when (odu::mark= p1 buf-beg)
        (oduvanchik-interface:loud-message "Не могу определить символ в начале буфера")
        (return-from function (values "" nil)))
      (do ()
          ((not (and (> rest-length 0)
                     (odu::mark> p1 buf-beg))) nil)
        (let prev-char (odu::next-character p1))
        (setf cur-in-symbol (editor-budden-tools::char-can-be-in-symbol prev-char))
        (unless checked-where-we-must-be-initially
          (let where-we-are (where-is-mark-relative-to-symbol p1))
          (cond
           ((and (= previous 0) ; Должны стоять в символе изначально
                 (not (member where-we-are '(0 1 2))) ; и не стоим в символе
                 )
            (return-from function (values "" nil)))
           ((and (= previous 2)
                 (/= where-we-are 0))
            (bell-with-tcl) 
            (return-from function (values "" nil))))  
          (setf checked-where-we-must-be-initially t))
        (when cur-in-symbol
          (unless v-in-symbol
            (setf v-in-symbol t)))
        (when v-in-symbol
          (unless cur-in-symbol
            (setf symbol-beginning (odu::copy-mark p1 :temporary))
            (return nil)))
        (odu::character-offset p1 -1)
        (incf rest-length -1)
        )
      (cond
       ((not symbol-beginning)
        ; ничего нет
        (return-from function (values "" nil)))
       (t
        (odu::character-offset symbol-beginning 1)
        (get-symbol-from-current-point-part-2
         symbol-beginning max-length buf-end our-readtable return-symbol-too create-new package-designator readtable point :selection-p nil)
        ))))))
    
(defun get-symbol-from-current-point-part-2 (symbol-beginning max-length buf-end our-readtable return-symbol-too create-new package-designator readtable point &key selection-p)
  "Если selection-p, то символ взят из выделения"
  (perga-implementation:perga
   ;(when selection-p (break))
   (let lookup-end (odu::copy-mark symbol-beginning :temporary))
   ; Теперь нужно найти конец символа. Для нашей - просто вырезать кусок максимально возможной длины, т.к. мы будем читать символ ридером
   (cond
    (selection-p
     ;; если передан selection, то max-length - это длина выделения
     (oi::character-offset lookup-end max-length))
    (t
     ;; находим конец начертания символа
     (let lookup-end-count max-length)
     (do () ((not (and (odu::mark< lookup-end buf-end)
                       (or 
                        (null max-length) 
                        (> lookup-end-count 0)))) nil)
       (unless (and (not selection-p) (editor-budden-tools::char-can-be-in-symbol (odu::next-character lookup-end)))
         (setf lookup-end (oi::copy-mark lookup-end :temporary))
         (return))
       (odu::character-offset lookup-end 1)
       (incf lookup-end-count -1))))
   (let ss (clco::string-between-marks symbol-beginning lookup-end))
   (let package (or
                 (find-package package-designator)
                 (progn
                   (message "Не могу понять пакет в ~S. Предполагаю :CL-USER" point)
                   (find-package :cl-user))))
   (get-symbol-from-current-point-part-3 ss package readtable our-readtable return-symbol-too create-new selection-p)))

(defun get-symbol-from-current-point-part-3 (ss package readtable our-readtable return-symbol-too create-new selection-p)
  (perga-implementation:perga
   (cond
    ((not (and our-readtable return-symbol-too))
     (values ss nil)
     )
    (create-new
     (ignore-errors
      (let ((budden-tools::*inhibit-readmacro* t)
            (*readtable* readtable)
            (*read-eval* nil)
            (*package* package))
        (multiple-value-bind (maybe-symbol error)
                             (ignore-errors (read-from-string ss))
          (cond
           ((typep error 'error)
            (if selection-p
                (message "Ошибка чтения символа из выделенного фрагмента")
                (message "Ошибка чтения символа под курсором. Попробуйте выделить символ и повторите команду")))
           ((not (symbolp maybe-symbol))
            (message "~A - не символ, а объект типа ~S"
                     (if selection-p
                         "Выделенный фрагмент"
                         "Под курсором")
                     (type-of maybe-symbol)))
           (t
            (values ss maybe-symbol)))))))
    (t 
     (:@ multiple-value-bind (maybe-potential-symbol maybe-error)
         (ignore-errors
          (perga-implementation:perga 
           (let sbcl-reader-budden-tools-lispworks:*return-package-and-symbol-name-from-read* t)
           (let *package* package)
           (let budden-tools::*inhibit-readmacro* t)
           (read-from-string ss))
          ))
     (cond
      ((typep maybe-error 'error) ; maybe-symbol can be nil, and read returns position. 
       ; so we check if there is a error
       (if selection-p
           (message "Ошибка чтения символа из выделенного фрагмента")
           (message "Ошибка чтения символа под курсором. Попробуйте выделить символ и повторите команду"))
       (values "" nil) ; no symbol
       )
      ((symbolp maybe-potential-symbol)
       (values ss maybe-potential-symbol))
      ((sbcl-reader-budden-tools-lispworks:potential-symbol-p maybe-potential-symbol)
       (assert (null editor-budden-tools::*in-find-source*))
       ; if this assertion is not true, we could return not the symbol
       ; we had under cursor, but something irrelevant

       (cond
        (return-symbol-too
         (:@ multiple-value-bind (symbol found)
             (editor-budden-tools::process-potential-symbol
              maybe-potential-symbol package)
             )
         (cond
          (found
           (values ss symbol))
          (t
           (if selection-p
               (message "Не найден символ в выделенном фрагменте")
               (message "Не найден символ в строке ~S. Если я неправильно определила границы символа, попробуйте выделить его и повторите команду" ss))
           (values ss nil))))
        (t
         (values ss nil))))
      
      ;((not (symbolp maybe-symbol))
      ; (editor-error "~S is not a symbol name" maybe-symbol))
      ((and (consp maybe-potential-symbol)
            (eq (car maybe-potential-symbol) 'budden-tools:|^|)
            (SBCL-READER-BUDDEN-TOOLS-LISPWORKS:potential-symbol-p (second maybe-potential-symbol)))
       (:@ multiple-value-bind (symbol found)
           (editor-budden-tools::process-potential-symbol (second maybe-potential-symbol) package))
       (assert found)
       (values ss symbol))
      (t ; in some modes we should not err
       (if selection-p
           (message "Не найден символ в выделенном фрагменте")
           (message "Не найден символ в строке ~S. Если я неправильно определила границы символа, попробуйте выделить его и повторите команду" ss))
       (values "" nil)
       )
      )
     )
    ))) 


