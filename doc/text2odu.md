﻿Lisp-mode для clcon
=====================

Краткая сводка
--
Есть код одуванчика (в прошлом - цикута), написанный на лиспе. Он умеет делать автоотступ, раскраску, подсветку парных скобок, определение текущего символа, жонглирование (aka paredit). Мы работаем в ctext и хотим это туда перетащить. Принято за аксиому, что одуванчик - плохо отлаженный редактор и мы не готовы его отлаживать. Поэтому записываем поток изменений, происходящих в ctext, отправляем их в лисп. В лиспе невидимый одуванчик отображает все эти изменения у себя с помощью всего трёх команд: добавить текст, удалить текст, заменить текст. Этот текст лисп анализирует и раскрашивает внутри себя. Он вычисляет автоиндент и подаёт ctext-у команды, что делать. О результатах выполнения узнаёт только от text-а - сам по своей инициативе свой буфер не правит, а только по командам ctext. 

Дополнительные источники информации
--
Дана ссылка на /s2/oduvanchik2/doc/ps/oduvanchik-cim.ps
[Он же в репозитории](https://bitbucket.org/budden/oduvanchik/src/default/doc/ps/oduvanchik-cim.ps?at=default)

Особенности
--


Архитектура одуванчика
--

    Нажатие буквы: /s2/oduvanchik2/src/command.lisp
    
     insert-string , insert-character
     current-point - должно приходить с событием

     oduvanchik-internals::insert-character mark character

     oduvanchik-internals::mark - класс:
       line charpos %kind

     mark-некая структура, ссылающаяся на внутренние структуры odu, в т.ч. 
        на line. 
     Пришёл индекс l.c -> по нему вычислить mark. Как? 
         объект line. 

     mark-line - типа
     oduvanchik-internals::line
       line-chars -> chars
       previous next
       marks
       %buffer - буфер, но может быть и ничем
     number
  
     oduvanchik-internals::buffer
        содержит 
     oduvanchik-internals::region

buffer содержит start и end - две марки. С марков выходим на строку, со строки - на 
  следующую и т.п. Таким образом, поиск строки по номеру занимает линейное время.



Подзадачи
---------
Стратегия движения - двигаться по градиенту риска. 

### 1. Научить одуванчик зеркалить происходящее в text.
Уже сделан ::clcon_text::clcon_text, способный отправлять команды. Нужно сделать формат сообщений и принимающую часть. Формат сообщений можно сделать пока через вызовы ::tkcon::EvalInSwankAsync . Принимающей частью будет видимый одуванчик в изначально пустом буфере и три команды работы с текстом. 

### 2. Система координат для п.1.
Задача состоит в том, чтобы по индексам ctext правильно определять индексы в одуванчике.
pathName index index возвращает индекс в форме line.char . Нужно написать ф-ю одуванчика для нахождения соответствующего места и установки марка на неё. 

### 3. Проблемы многопоточности в одуванчике
Вместо мьютексов (или критических секций?) применяется without-interrupts. Это требует решения.

### 4. Команда "определить отступ".
Алгоритм: сообщение об определении отступа проходит через ту же очередь на отправку, что и все сообщения о нажатиях на кнопку, дабы не нарушился порядок. Далее вызываем такую же команду в одуванчике, но она должна быть модифицирована. Примерный план для insert-lisp-indentation.

    (defun insert-lisp-indentation (m)
      (delete-horizontal-space m)
      (indent-to-column m (lisp-indentation m)))

    (defmacro with-mark (mark-bindings &rest forms)
      "With-Mark ({(Mark Pos [Kind])}*) {declaration}* {form}*"
       "With-Mark binds a variable named Mark to a mark specified by Pos.  This"
        "mark is :temporary, or of kind Kind.  The forms are then evaluated."
        )
Создаётся временная марка в каком-то месте текста. Она может перемещаться командами.
А потом её используют скажем, для определения границы удаляемого куска текста. Здесь
нет проблемы: просто нужно обратное преобразование координатов метки в r.c
Есть mark-charpos (вроде позиция строки). Алгоритм вычисления номера строки можно откопать
в следующем куске кода или около него:

    (defcommand "Goto Absolute Line" (p)
      (with-mark ((m point))
        (unless (line-offset (buffer-start m) (1- p) 0)
          (editor-error "Not enough lines in buffer."))
        (move-mark point m)))))
       

    (defun delete-horizontal-space (mark)
      "Deletes all :space characters on either side of mark."
      (with-mark ((start mark)) ; см. ниже
       (reverse-find-attribute start :space #'zerop)
       (find-attribute mark :space #'zerop)
       (delete-region (region start mark))))

    indent-to-column - примерно то же самое

Марка может подвинуться при командах редактирования текста, но это даже не страшно.
Таким образом, по ходу чтения дерева вызовов команды вычисления отступа мы выявляем
те примитивы, в которых прямое обращение к одуваничку нужно подменить работой с лисповым буфером. 

#### 4.1 Факторы риска в команде "определить отступ".
  Как и в любой команде, выполнение занимает некоторое время, в течение которого будет происходить обмен информацией между клиентом и сервером. Основной фактор риска - смогут ли события от клавиатуры сколько-то подождать, пока лисп протупится и сможет их обработать. Второй фактор риска - возможные проблемы параллельности. 

#### 4.2 Критерии решения задачи блокировки
Мы можем смоделировать тупёж лиспа следующим путём:

  1. В лиспе запускаем любый (loop (sleep) (print)), чтобы было труднее
  2. Связываем на кнопку F7 алгоритм, который:

     отправляет в лисп (sleep) (eval-in-tcl "insert .text insert "adfjkl")

  3. Отчаянно жмём на кнопки, пытаемся переставить курсор
  4. Должно появиться asdfjkl и только потом наши нажатые кнопки

Более простой вариант - без лиспа. Просто включаем/выключаем режим затупа. В режиме
затупа сообщения пишутся в очередь. При отключении режима затупа - достаются из очереди. 
Режим затупа то включаем, то выключаем, и пробуем при этом что-то печатать и делать мышью. 

#### 4.3 Вариант решения А
  Вдруг само получится? Если события клавиатуры ложатся в другую очередь (что вряд ли) 

#### 4.4 Вариант решения Б
  На F7 включаем режим, в котором text перестаёт отвечать на нажатия, а складывает свои события мыши и клавы в очередь. Беда случится при ресайзе, но это и нормально - нехрен трогать когда не надо. 

#### 4.5. Критерии решения задачи параллельности. 
  Видимо, нужно ставить условные брекпойнты в разных неприятных местах и в это время работать во втором буфере. Или нужно прочитать код, выяснить используемые примитивы блокировки и заменить их на современные. 


### 5. Раскраска: 
видимо, нужно найти примитивы раскраски, которые используются в одуванчике и перешибить их. Все примитивы раскраски, применённые после принятого из tcl редактирования N, должны записываться в очередь. Когда tcl присылает следующее редактирование N+1, мы говорим ему: посоревнуйся сперва с моим младшим братом. Отправляем tcl-ю все примитивы раскраски, он их применяет. После этого мы выполняем в лиспе редактирование N+1. 

Если пользователь не трогает текст, то tcl должен запросить её по таймеру. Пока не пытаемся оптимизировать раскраску, поскольку для файлов до 100 кб она работает достаточно быстро. За счёт блокировки буфера на время обращения к лиспу у нас гарантировано совпадение буферов tcl и одуванчика в процессе обмена данными, значит нет проблемы, что при передаче первой раскраски придёт слишком много данных и раскраска займёт время. 

План
----
1. Блокировка буфера редактирования с записью сообщений в очередь - без лиспа.

На данный момент выяснили: 
>..bindtags .__edit3.text
.__edit3.text Snit::clcon_text::clcon_text.__edit3.text Ctext .__edit3 all
>..foreach tag [bindtags .__edit3.text] { bind $tag }
>..foreach tag [bindtags .__edit3.text] { puts [bind $tag] }
<Destroy>
<Destroy>

>..foreach tag [bindtags .__edit3.text.t] { puts [bind $tag] }
<Destroy>
<KeyRelease-Return> <Configure>
<Shift-Button-5> <Shift-Button-4> <Button-5> <Button-4> <Shift-MouseWheel> <MouseWheel> <B2-Motion> <Button-2> <Control-Key-h> <Meta-Key-Delete> <Meta-Key-BackSpace> <Meta-Key-greater> <Meta-Key-less> <Meta-Key-f> <Meta-Key-d> <Meta-Key-b> <<Redo>> <<Undo>> <Control-Key-t> <Control-Key-o> <Control-Key-k> <Control-Key-d> <Key-KP_Enter> <Key-Escape> <Control-Key> <Meta-Key> <Alt-Key> <Key> <Key-Insert> <<PasteSelection>> <<Clear>> <<Paste>> <<Copy>> <<Cut>> <<SelectNone>> <<SelectAll>> <Shift-Key-Select> <Control-Shift-Key-space> <Key-Select> <Control-Key-space> <Key-BackSpace> <Key-Delete> <Key-Return> <Control-Key-i> <Control-Shift-Key-Tab> <Control-Key-Tab> <Shift-Key-Tab> <Key-Tab> <Control-Shift-Key-End> <Control-Key-End> <Control-Shift-Key-Home> <Control-Key-Home> <<SelectLineEnd>> <<LineEnd>> <<SelectLineStart>> <<LineStart>> <Control-Key-Next> <Control-Key-Prior> <Shift-Key-Next> <Key-Next> <Shift-Key-Prior> <Key-Prior> <<SelectNextPara>> <<SelectPrevPara>> <<SelectNextWord>> <<SelectPrevWord>> <<NextPara>> <<PrevPara>> <<NextWord>> <<PrevWord>> <<SelectNextLine>> <<SelectPrevLine>> <<SelectNextChar>> <<SelectPrevChar>> <<NextLine>> <<PrevLine>> <<NextChar>> <<PrevChar>> <Control-Button-1> <ButtonRelease-1> <B1-Enter> <B1-Leave> <Triple-Shift-Button-1> <Double-Shift-Button-1> <Shift-Button-1> <Triple-Button-1> <Double-Button-1> <B1-Motion> <Button-1>
<Control-Key-period> <Control-Key-F12> <Control-Key-w>
<<PrevWindow>> <<NextWindow>> <Key-F10> <Alt-Key>

Т.е., часть биндингов относится к ctext, а часть - к его t. Наверное, можно обернуть. 

План действий:
!1. Переходим обратно на text.
!2. Извлекаем все биндинги из text
!3. Оборачиваем кажды из них в заморозчик и называем FreezableText
!4. В конструкторе FreezableText с помощью bindtags подменяет биндинги для конкретного экземпляра clcon_text с Text на FreezableText
!5. Также оборачиваем почти все команды меню с помощью WrapEventScriptForFreezedText или WrapFreezingAndFreezableHandlerScript. 



2. Зеркалить один буфер из tcl в видимый одуванчик. Попутно везде смотрим в лиспе на многопоточность и реентерабельность. 

  Как? Все события модификации асинхронно отправляем в лисп. Считаем их текущее кол-во 
с помощью опции виджета по имени private_pending_sent_modifications 
  Норма - когда в состоянии покоя эта опция равна нулю. Если больше нуля - одуванчик захлебнулся или что-то сломалось.




3. Автоотступ
4. Раскраска