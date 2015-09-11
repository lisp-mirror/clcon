Примеры метапрограммирования в нашем проекте
===========

Отображение выражения и его значения [../util.tcl](../util.tcl)
----

    proc showVar {name} {
        puts "sV:$name=[uplevel 1 [string cat {format %s $} $name]]"
    }


search-tablelist.srchtblst.tcl [../search-tablelist.srchtblst.tcl](../search-tablelist.srchtblst.tcl)
---

### TestFnMan1

1. Создаём тело продолжения и сразу передаём в качестве параметра ContinuationBody:

    ```... { if {!$found} {error "TableListTest1 failure 2"} else {puts "found 2"}  
          }```

Тело ссылается на параметры, к-рые будут доступны в момент исполнеия.

2. Формируем lambda-выражение
    
    ```set lambda [list {tablelist found SearchState} $ContinuationBody]```

3. Вызываем apply с фактическими параметрами
    
    ```apply $lambda $tbl 0 $SearchState```

Этот случай прост тем, что тело продолжения не зависит от локальных переменных и не требует подстановок. 


Обмен данными со swank:
----

Смысл, как и в предыдущем примере, передаётся тело функции, а потом к нему приделываются параметры и оно выполняется.

    proc ::insp::InspectNthPart {w id} {
        ...
        set OnReply "::insp::ShowSomethingNewInInspector $w \$EventAsList"
        ::tkcon::EvalInSwankAsync ... $OnReply ...
    }
    

Здесь тело задано в виде строки. $w подставляется в момент создания продолжения,
а \$EventAsList будет выполнен позже.

[Более красивое замыкание](../lisp-debugger.ldbg.tcl)
-----------
Пытаемся сделать красивее. Вот так:

        ::tkcon::EvalInSwankAsync $lispCmd [subst -nocommands {
            ::ldbg::InsertLocsNTagsForFrameIntoTree $RowName \$EventAsList
        }] 0 [GetDebuggerThreadId]

Здесь subst даёт замену переменных, доступных в момент определения. \$ защищает
переменные, которые будут доступны в момент выполнения (параметры лямбды при apply).
Можно было сразу сгенерировать лямбду, но по (неудачно придуманному) соглашению мы
передаём только её тело.

