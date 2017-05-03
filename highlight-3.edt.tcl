# По аналогии с highlight.edt.tcl и с частичным использованием
# его инфраструктуры. 

namespace eval ::edt {

    # См. .apr Закодировать-один-мазок
    # {Нарисовать/стереть Строка-начала Колонка-начала Строка-конца Колонка-конца Число}
    proc ПрименитьРаскраскуКРегиону {text tick_count s} {
        if { $tick_count < [$text cget -tick_count] } {
            return 
        }
        foreach {ВклВыкл Slojj r1 c1 r2 c2 font} $s break
        puts $s
        set i1 [string cat $r1 . $c1]
        set i2 [string cat $r2 . $c2]
        set TagName [::edt::HighlightTagName $font]
        if {${ВклВыкл} == 2} {
            ::edt::DeleteHighlightInRegion $text $Slojj $i1 $i2
        } elseif {${ВклВыкл} == 1} {
            $text tag add $TagName $i1 $i2
        }
    }


    proc DeleteHighlightInRegion {text Slojj i1 i2} {
        variable ColorTable
        set i 0
        foreach e $ColorTable {
            set TagName [HighlightTagName $i]
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
}
