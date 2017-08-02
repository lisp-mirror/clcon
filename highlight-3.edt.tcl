# По аналогии с highlight.edt.tcl и с частичным использованием
# его инфраструктуры. 

namespace eval ::edt {

    # См. .apr Закодировать-один-мазок
    # {Нарисовать/стереть Строка-начала Колонка-начала Строка-конца Колонка-конца Число}
    # Строки и колонки - в координатах indices из индекса edit, т.е. строка от 1, колонка от 0
    proc ПрименитьРаскраскуКРегиону {text tick_count s} {
        #puts "ПрименитьРаскраскуКРегиону $tick_count $s"
        if { $tick_count < [$text cget -tick_count] } {
            return 
        }
        foreach {Команда Slojj r1 c1 r2 c2 font} $s break
        # puts $s
        set i1 [string cat $r1 . $c1]
        set i2 [string cat $r2 . $c2]
        set TagName [::edt::HighlightTagName $Slojj $font]
        if {${Команда} == 2} {
            ::edt::DeleteHighlightInRegion $text $Slojj $i1 $i2
        } elseif {${Команда} == 1} {
            $text tag add $TagName $i1 $i2
        } elseif {${Команда} == 3} {
            ::edt::DeleteHighlightInRegion $text $Slojj 1.0 end
        } else {
            error "Неверная команда"
        }
    }


    proc DeleteHighlightInRegion {text Slojj i1 i2} {
        variable ColorTable
        set i 0
        foreach e $ColorTable {
            set TagName [HighlightTagName $Slojj $i]
            #puts "$text tag remove $TagName $i1 $i2"
            $text tag remove $TagName $i1 $i2
            incr i
        }
    }


    # см. clco::eval-highlight-3
    # см. также ::edt::ApplyHighlightToLine - "старая" раскраска, для Лиспа
    proc ApplyHighlight3 {clcon_text tick_count s} {
        if {[winfo exists $clcon_text]} {
            variable NumberOfPendingHighlights
            incr NumberOfPendingHighlights
            after idle [list ::edt::DoApplyHighlight3 $clcon_text $tick_count $s]
        }
    }

    proc DoApplyHighlight3 {clcon_text tick_count s} {
        variable NumberOfPendingHighlights
        # текст мог исчезнуть
        if {[winfo exists $clcon_text]} {
            DoApplyHighlight3Inner $clcon_text $tick_count $s
        }
        incr NumberOfPendingHighlights -1
    }
    
    proc DoApplyHighlight3Inner {clcon_text tick_count s} {
        set i 0
        foreach e $s {
            ::edt::ПрименитьРаскраскуКРегиону $clcon_text $tick_count $e
            incr i
        }
    }

    # Мы могли бы отслеживать, успешно ли завершилась раскраска. Похоже, что в этом нет нужды.
    # Если мы отправили запрос на раскраску из tcl в лисп, то либо раскраска успеет устареть,
    # но это будет значить, что буфер tk поменялся, и будет подан новый запрос на раскраску. 
    # В этом случае судьба старого запроса нас не интересует. 
    # Либо она не успеет устарить и завершится успешно. Но тогда и проверять нечего. 
    proc ПопроситьЛиспПрислатьДанныеОРаскраске {clcon_text Код-слоя} {
        if {![winfo exists $clcon_text]} {
            return
        }
        set tick_count [$clcon_text cget -tick_count]
        set tick_count-когда-перекрашивали [$clcon_text cget -tick_count-когда-перекрашивали ]
        set КогдаПерекрашивалиЭтотСлой [lindex ${tick_count-когда-перекрашивали} ${Код-слоя}]
        if {${КогдаПерекрашивалиЭтотСлой} < $tick_count} {
            # puts "Просим перекрасить, $tick_count"
            ::clcon_text::MaybeSendToLisp $clcon_text h ${Код-слоя}
            set tick_count-когда-перекрашивали \
               [lreplace ${tick_count-когда-перекрашивали} ${Код-слоя} ${Код-слоя} $tick_count]
            $clcon_text configure -tick_count-когда-перекрашивали ${tick_count-когда-перекрашивали}
        }
    }
}
