Заготовки для многооконного редактора
---------------------

О чём нужно подумать?

oduvanchik-internals::*do-editing-on-tcl-side* должна быть локальной для буфера.


Переделка существующего GUI редактора
------------
Изначально у нас было окно и в нём текст. 
``Теперь будет 4 уровня:
1. Окно
2. Рамка буфера редактора
3. btext
4. text``


Что тут нужно поменять? 
``
1. Управление созданием и уничтожением.
2. Биндинги. 
3. Хитрые сочетания с Control и т.п. 
Видимо, нужен биндтаг для двух нажатых клавиш и биндтаг для одной.
Они ставятся впереди других биндтагов для каждого уровня вложенности интерфейса,
который может иметь фокус (это много, блин). 
Видимо, имя биндтага должно иметь в своём составе id редактора (__editNN). 

Текущий план
-----------
Окно никогда не уничтожается (на wm protocol поставить withdraw)
Настройка окна при переключении редактора:
либо нужно перебиндить все команды, либо иметь на каждый буфер набор биндтагов, либо ни одна команда не должна принимать аргументом буфер, а принимать окно. Для определения буфера использовать ф-ю текущего буфера. Наверное, это будет наилучшим. 

Разбиваем редактор на две части:
1. Часть, незавимая от окон editor.edt.tcl
2. Часть, зависимая от окон editor_buffer.edt.tcl


Irrelevant functions
--------------------
#irr CanonicalizeEditArgs
#irr CompileAndLoadTheFile 	
#irr EditorParseArgs - 
#irr EncodeTypeForBufferList
#irr FindSourceCommand
#irr FindSourceContinuation
GenReuseCounter
InitEditorWindowLists
IsFileBeingEdited
ReadFileIntoString
SingleEditorWindow - нерелевантно, потому что пока не меняется
TextSelectionCoordinates
TextSetSelectionTo

Relevant functions
------------------
+AddToWindowLists - использует только id окна, можно переделать
+CurrentlyVisibleBuffer - использует winfo
+EditCloseFile - уничтожать frame, а не окно

?EnsureEditorWindow - распадается на два
?FindOrMakeEditorWindow - вызывает EnsureEditorWindow. Распадается?
?GenEditorWindowName - генерировать id, а не полный путь окна?
?HideAllEditorWindows
?HideEditorWindow
+LoadContents - формально не при чём, но ссылается на имена
+MakeLispModeMenu
+MaybeDestroyEditorWindow
+OduFnMenuItem - убрать зависимость от буфера на входе, определять его внутри
+RemoveWindowFromLists
+SetupEditorWindow - распадётся на две - для окна в целом и для конкретного буфера
+ShowExistingBuffer
ShowSomeEditor - видимо, по смыслу отобразить окно, если оно скрыто? Но оно у нас никогда не скрыто. Убрать вообще. 
SyncCursor - да, убрать параметр текст или разбить на два слоя. 
UpdateMRUAndBufferList - да, использовать ключ окна
e_indent - да, т.к. вызывается из команды. Расслоить.
edit - да, ссылка на элементы окна.
oImplementation - да, ссылка на эл-ты окна
w2tw о да, использовать далее. 
wesppt - да, абстрагировать? 


Т.о., SetupEditorWindow разбивается на две. 
1. SetupMainEditorWindow - вызывается один раз при создании редактора. 
2. SetupBufferGUI - вызывается для создания одного буфера. 


Обособляем id буфер от его виджета.
id теперь будет buf<NN>

Пишем функцию "получить окно по id" и "получить id по окну".
Это всё вроде сделано, см. ../edt_structure.edt.tcl
Работает даже список оконо и отладчик.

Теперь абстрагируем команды от текущего окна. Возможно, поделим настройку
окна на 2 части, одна для окна в целом, другая - для конкретного буфера.
Но начнём с отдельных команд.

cW - виджет (напр.рамка)
cTW - окно
c_btext - текущий btext (clcon_text)
c_text - текущий ctext.